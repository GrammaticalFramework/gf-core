{-# LANGUAGE BangPatterns, FlexibleContexts #-}
module GF.Compile.GrammarToPGF (mkCanon2pgf) where

--import GF.Compile.Export
import GF.Compile.GeneratePMCFG
import GF.Compile.GenerateBC

import PGF(CId,mkCId,utf8CId)
import PGF.Internal(fidInt,fidFloat,fidString,fidVar)
import PGF.Internal(updateProductionIndices)
import qualified PGF.Internal as C
import GF.Grammar.Predef
--import GF.Grammar.Printer
import GF.Grammar.Grammar
import qualified GF.Grammar.Lookup as Look
import qualified GF.Grammar as A
import qualified GF.Grammar.Macros as GM

import GF.Infra.Ident
import GF.Infra.Option
import GF.Infra.UseIO (IOE)
import GF.Data.Operations

import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Data.Array.IArray

mkCanon2pgf :: Options -> SourceGrammar -> ModuleName -> IOE C.PGF
mkCanon2pgf opts gr am = do
  (an,abs) <- mkAbstr am
  cncs     <- mapM mkConcr (allConcretes gr am)
  return $ updateProductionIndices (C.PGF Map.empty an abs (Map.fromList cncs))
  where
    cenv = resourceValues opts gr

    mkAbstr am = return (mi2i am, C.Abstr flags funs cats)
      where
        aflags = err (const noOptions) mflags (lookupModule gr am)

        adefs =
            [((cPredefAbs,c), AbsCat (Just (L NoLoc []))) | c <- [cFloat,cInt,cString]] ++ 
            Look.allOrigInfos gr am

        flags = Map.fromList [(mkCId f,x) | (f,x) <- optionsPGF aflags]

        funs = Map.fromList [(i2i f, (mkType [] ty, arity, mkDef gr arity mdef, 0)) | 
                                   ((m,f),AbsFun (Just (L _ ty)) ma mdef _) <- adefs,
                                   let arity = mkArity ma mdef ty]

        cats = Map.fromList [(i2i c, (snd (mkContext [] cont),catfuns c, 0)) |
                                   ((m,c),AbsCat (Just (L _ cont))) <- adefs]

        catfuns cat =
              [(0,i2i f) | ((m,f),AbsFun (Just (L _ ty)) _ _ (Just True)) <- adefs, snd (GM.valCat ty) == cat]

    mkConcr cm = do
      let cflags  = err (const noOptions) mflags (lookupModule gr cm)

      (ex_seqs,cdefs) <- addMissingPMCFGs
                            Map.empty 
                            ([((cPredefAbs,c), CncCat (Just (L NoLoc GM.defLinType)) Nothing Nothing Nothing Nothing) | c <- [cInt,cFloat,cString]] ++
                             Look.allOrigInfos gr cm)

      let flags = Map.fromList [(mkCId f,x) | (f,x) <- optionsPGF cflags]

          seqs = (mkSetArray . Set.fromList . concat) $
                     (Map.keys ex_seqs : [maybe [] elems (mseqs mi) | (m,mi) <- allExtends gr cm])

          ex_seqs_arr = mkMapArray ex_seqs :: Array SeqId Sequence

          !(!fid_cnt1,!cnccats) = genCncCats gr am cm cdefs
          !(!fid_cnt2,!productions,!lindefs,!linrefs,!cncfuns)
                                = genCncFuns gr am cm ex_seqs_arr seqs cdefs fid_cnt1 cnccats
        
          printnames = genPrintNames cdefs
      return (mi2i cm, C.Concr flags
                              printnames
                              cncfuns
                              lindefs
                              linrefs
                              seqs
                              productions
                              IntMap.empty
                              Map.empty
                              cnccats
                              IntMap.empty
                              fid_cnt2)
      where
        -- if some module was compiled with -no-pmcfg, then
        -- we have to create the PMCFG code just before linking
        addMissingPMCFGs seqs []                  = return (seqs,[])
        addMissingPMCFGs seqs (((m,id), info):is) = do
          (seqs,info) <- addPMCFG opts gr cenv Nothing am cm seqs id info
          (seqs,is  ) <- addMissingPMCFGs seqs is
          return (seqs, ((m,id), info) : is)

i2i :: Ident -> CId
i2i = utf8CId . ident2utf8

mi2i :: ModuleName -> CId
mi2i (MN i) = i2i i

mkType :: [Ident] -> A.Type -> C.Type
mkType scope t =
  case GM.typeForm t of
    (hyps,(_,cat),args) -> let (scope',hyps') = mkContext scope hyps
                           in C.DTyp hyps' (i2i cat) (map (mkExp scope') args)

mkExp :: [Ident] -> A.Term -> C.Expr
mkExp scope t = 
  case t of
    Q (_,c)  -> C.EFun (i2i c)
    QC (_,c) -> C.EFun (i2i c)
    Vr x     -> case lookup x (zip scope [0..]) of
                  Just i  -> C.EVar  i
                  Nothing -> C.EMeta 0
    Abs b x t-> C.EAbs b (i2i x) (mkExp (x:scope) t)
    App t1 t2-> C.EApp (mkExp scope t1) (mkExp scope t2)
    EInt i   -> C.ELit (C.LInt (fromIntegral i))
    EFloat f -> C.ELit (C.LFlt f)
    K s      -> C.ELit (C.LStr s)
    Meta i   -> C.EMeta i
    _        -> C.EMeta 0

mkPatt scope p = 
  case p of
    A.PP (_,c) ps->let (scope',ps') = mapAccumL mkPatt scope ps
                   in (scope',C.PApp (i2i c) ps')
    A.PV x      -> (x:scope,C.PVar (i2i x))
    A.PAs x p   -> let (scope',p') = mkPatt scope p
                   in (x:scope',C.PAs (i2i x) p')
    A.PW        -> (  scope,C.PWild)
    A.PInt i    -> (  scope,C.PLit (C.LInt (fromIntegral i)))
    A.PFloat f  -> (  scope,C.PLit (C.LFlt f))
    A.PString s -> (  scope,C.PLit (C.LStr s))
    A.PImplArg p-> let (scope',p') = mkPatt scope p
                   in (scope',C.PImplArg p')
    A.PTilde t  -> (  scope,C.PTilde (mkExp scope t))

mkContext :: [Ident] -> A.Context -> ([Ident],[C.Hypo])
mkContext scope hyps = mapAccumL (\scope (bt,x,ty) -> let ty' = mkType scope ty
                                                      in if x == identW
                                                           then (  scope,(bt,i2i x,ty'))
                                                           else (x:scope,(bt,i2i x,ty'))) scope hyps 

mkDef gr arity (Just eqs) = Just ([C.Equ ps' (mkExp scope' e) | L _ (ps,e) <- eqs, let (scope',ps') = mapAccumL mkPatt [] ps]
                                 ,generateByteCode gr arity eqs
                                 )
mkDef gr arity Nothing    = Nothing

mkArity (Just a) _        ty = a   -- known arity, i.e. defined function
mkArity Nothing  (Just _) ty = 0   -- defined function with no arity - must be an axiom
mkArity Nothing  _        ty = let (ctxt, _, _) = GM.typeForm ty  -- constructor
                               in length ctxt

genCncCats gr am cm cdefs =
  let (index,cats) = mkCncCats 0 cdefs
  in (index, Map.fromList cats)
  where
    mkCncCats index []                                                = (index,[])
    mkCncCats index (((m,id),CncCat (Just (L _ lincat)) _ _ _ _):cdefs) 
      | id == cInt    = 
            let cc            = pgfCncCat gr lincat fidInt
                (index',cats) = mkCncCats index cdefs
            in (index', (i2i id,cc) : cats)
      | id == cFloat  = 
            let cc            = pgfCncCat gr lincat fidFloat
                (index',cats) = mkCncCats index cdefs
            in (index', (i2i id,cc) : cats)
      | id == cString = 
            let cc            = pgfCncCat gr lincat fidString
                (index',cats) = mkCncCats index cdefs
            in (index', (i2i id,cc) : cats)
      | otherwise     =
            let cc@(C.CncCat _s e _) = pgfCncCat gr lincat index
                (index',cats)        = mkCncCats (e+1) cdefs
            in (index', (i2i id,cc) : cats)
    mkCncCats index (_                      :cdefs) = mkCncCats index cdefs

genCncFuns :: Grammar
           -> ModuleName
           -> ModuleName
           -> Array SeqId Sequence
           -> Array SeqId Sequence
           -> [(QIdent, Info)]
           -> FId
           -> Map.Map CId C.CncCat
           -> (FId,
               IntMap.IntMap (Set.Set C.Production),
               IntMap.IntMap [FunId],
               IntMap.IntMap [FunId],
               Array FunId C.CncFun)
genCncFuns gr am cm ex_seqs seqs cdefs fid_cnt cnccats =
  let (fid_cnt1,lindefs,linrefs,fun_st1) = mkCncCats cdefs fid_cnt IntMap.empty IntMap.empty Map.empty
      ((fid_cnt2,crc,prods),fun_st2)     = mkCncFuns cdefs lindefs ((fid_cnt1,Map.empty,IntMap.empty),fun_st1)
  in (fid_cnt2,prods,lindefs,linrefs,array (0,Map.size fun_st2-1) (Map.elems fun_st2))
  where
    mkCncCats []                                                          fid_cnt lindefs linrefs fun_st =
      (fid_cnt,lindefs,linrefs,fun_st)
    mkCncCats (((m,id),CncCat _ _ _ _ (Just (PMCFG prods0 funs0))):cdefs) fid_cnt lindefs linrefs fun_st =
      let mseqs   = case lookupModule gr m of
                      Ok (ModInfo{mseqs=Just mseqs}) -> mseqs
                      _                              -> ex_seqs
          (lindefs',fun_st1) = foldl' (toLinDef (m,id) funs0 mseqs) (lindefs,fun_st ) prods0
          (linrefs',fun_st2) = foldl' (toLinRef (m,id) funs0 mseqs) (linrefs,fun_st1) prods0
      in mkCncCats cdefs fid_cnt lindefs' linrefs' fun_st2
    mkCncCats (_                                                  :cdefs) fid_cnt lindefs linrefs fun_st =
      mkCncCats cdefs fid_cnt lindefs linrefs fun_st

    mkCncFuns []                                                        lindefs st = st
    mkCncFuns (((m,id),CncFun _ _ _ (Just (PMCFG prods0 funs0))):cdefs) lindefs st =
      let ty_C    = err error (\x -> x) $ fmap GM.typeForm (Look.lookupFunType gr am id)
          mseqs   = case lookupModule gr m of
                      Ok (ModInfo{mseqs=Just mseqs}) -> mseqs
                      _                              -> ex_seqs
          bundles = [([(args0,res0) | Production res0 funid0 args0 <- prods0, funid0==funid],lins) | (funid,lins) <- assocs funs0]
          !st'    = foldl' (toProd id lindefs mseqs ty_C) st bundles
      in mkCncFuns cdefs lindefs st'
    mkCncFuns (_                                                :cdefs) lindefs st =
      mkCncFuns cdefs lindefs st

    toLinDef mid funs0 mseqs st@(lindefs,fun_st) (Production res0 funid0 [arg0])
      | arg0 == [fidVar] =
          let res  = mkFId mid res0

              lins = amap (newSeqId mseqs) (funs0 ! funid0)

              !funid    = Map.size fun_st
              !fun_st'  = Map.insert ([([C.PArg [] fidVar],res)],lins) (funid, C.CncFun [] lins) fun_st

              !lindefs' = IntMap.insertWith (++) res [funid] lindefs
          in (lindefs',fun_st')
    toLinDef res funs0 mseqs st _ = st

    toLinRef mid funs0 mseqs st (Production res0 funid0 [arg0])
      | res0 == fidVar  =
          let arg  = map (mkFId mid) arg0

              lins = amap (newSeqId mseqs) (funs0 ! funid0)

          in foldr (\arg (linrefs,fun_st) -> 
                          let !funid    = Map.size fun_st
                              !fun_st'  = Map.insert ([([C.PArg [] arg],fidVar)],lins) (funid, C.CncFun [] lins) fun_st

                              !linrefs' = IntMap.insertWith (++) arg [funid] linrefs
                          in (linrefs',fun_st'))
                   st arg
    toLinRef res funs0 mseqs st _ = st

    toProd id lindefs mseqs (ctxt_C,res_C,_) (prod_st,fun_st) (sigs0,lins0) =
      let (prod_st',sigs) = mapAccumL mkCncSig prod_st sigs0
          lins            = amap (newSeqId mseqs) lins0
      in addBundle id (prod_st',fun_st) (concat sigs,lins)
      where
        mkCncSig prod_st (args0,res0) =
          let !(prod_st',args) = mapAccumL mkArg prod_st (zip ctxt_C args0)
              res              = mkFId res_C res0
          in (prod_st',[(args,res) | args <- sequence args])

        mkArg st@(fid_cnt,crc,prods) ((_,_,ty),fid0s) =
          case fid0s of
            [fid0] -> (st,map (flip C.PArg (mkFId arg_C fid0)) ctxt)
            fid0s  -> case Map.lookup fids crc of
                        Just fid -> (st,map (flip C.PArg fid) ctxt)
                        Nothing  -> let !crc'   = Map.insert fids fid_cnt crc
                                        !prods' = IntMap.insert fid_cnt (Set.fromList (map C.PCoerce fids)) prods
                                    in ((fid_cnt+1,crc',prods'),map (flip C.PArg fid_cnt) ctxt)
          where
            (hargs_C,arg_C) = GM.catSkeleton ty
            ctxt = mapM mkCtxt hargs_C
            fids = map (mkFId arg_C) fid0s

        mkCtxt (_,cat) =
          case Map.lookup (i2i cat) cnccats of
            Just (C.CncCat s e _) -> [(C.fidVar,fid) | fid <- [s..e], Just _ <- [IntMap.lookup fid lindefs]]
            Nothing               -> error "GrammarToPGF.mkCtxt failed"

    newSeqId mseqs i = binSearch (mseqs ! i) seqs (bounds seqs)
      where
        binSearch v arr (i,j)
          | i <= j    = case compare v (arr ! k) of
                          LT -> binSearch v arr (i,k-1)
                          EQ -> k
                          GT -> binSearch v arr (k+1,j)
          | otherwise = error "binSearch"
          where
            k = (i+j) `div` 2

    addBundle id ((fid_cnt,crc,prods),fun_st) bundle@(sigs,lins) =
      case Map.lookup bundle fun_st of
        Just (funid, C.CncFun funs lins) -> 
          let !fun_st' = Map.insert bundle (funid, C.CncFun (i2i id:funs) lins) fun_st
              !prods'  = foldl' (\prods (args,res) -> IntMap.insert res (Set.singleton (C.PApply funid args)) prods) prods sigs
          in ((fid_cnt,crc,prods'),fun_st')
        Nothing                          -> 
          let !funid   = Map.size fun_st
              !fun_st' = Map.insert bundle (funid, C.CncFun [i2i id] lins) fun_st
              !prods'  = foldl' (\prods (args,res) -> IntMap.insert res (Set.singleton (C.PApply funid args)) prods) prods sigs
          in ((fid_cnt,crc,prods'),fun_st')

    mkFId (_,cat) fid0 =
      case Map.lookup (i2i cat) cnccats of
        Just (C.CncCat s e _) -> s+fid0
        Nothing               -> error ("GrammarToPGF.mkFId: missing category "++showIdent cat)


genPrintNames cdefs =
  Map.fromAscList [(i2i id, name) | ((m,id),info) <- cdefs, name <- prn info]
  where
    prn (CncFun _ _   (Just (L _ tr)) _) = [flatten tr]
    prn (CncCat _ _ _ (Just (L _ tr)) _) = [flatten tr]
    prn _                                = []

    flatten (K s)      = s
    flatten (Alts x _) = flatten x
    flatten (C x y)    = flatten x +++ flatten y

--mkArray    lst = listArray (0,length lst-1) lst
mkMapArray map = array (0,Map.size map-1) [(v,k) | (k,v) <- Map.toList map]
mkSetArray set = listArray (0,Set.size set-1) [v | v <- Set.toList set]
