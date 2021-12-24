{-# LANGUAGE BangPatterns, FlexibleContexts, MagicHash #-}
module GF.Compile.GrammarToPGF (grammar2PGF) where

import GF.Compile.GeneratePMCFG
import GF.Compile.GenerateBC
import GF.Compile.OptimizePGF

import PGF2 hiding (mkType)
import PGF2.Transactions
import GF.Grammar.Predef
import GF.Grammar.Grammar hiding (Production)
import qualified GF.Grammar.Lookup as Look
import qualified GF.Grammar as A
import qualified GF.Grammar.Macros as GM

import GF.Infra.Ident
import GF.Infra.Option
import GF.Infra.UseIO (IOE)
import GF.Data.Operations

import Control.Monad(forM_)
import Data.List
import Data.Char
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Data.Array.IArray
import Data.Maybe(fromMaybe)
import System.FilePath
import System.Directory

grammar2PGF :: Options -> Maybe PGF -> SourceGrammar -> ModuleName -> Map.Map PGF2.Fun Double -> IO PGF
grammar2PGF opts mb_pgf gr am probs = do
  let abs_name = mi2i am
  pgf <- case mb_pgf of
           Just pgf | abstractName pgf == abs_name   ->
                         do return pgf
           _        | snd (flag optLinkTargets opts) ->
                         do let fname = maybe id (</>)
                                              (flag optOutputDir opts)
                                              (fromMaybe abs_name (flag optName opts)<.>"ngf")
                            exists <- doesFileExist fname
                            if exists
                              then removeFile fname
                              else return ()
                            putStr ("(Boot image "++fname++") ")
                            newNGF abs_name (Just fname)
                    | otherwise ->
                         do newNGF abs_name Nothing

  pgf <- modifyPGF pgf $ do
    sequence_ [setAbstractFlag name value | (name,value) <- optionsPGF aflags]
    sequence_ [createCategory c ctxt p | (c,ctxt,p) <- cats]
    sequence_ [createFunction f ty arity bcode p | (f,ty,arity,bcode,p) <- funs]
    forM_ (allConcretes gr am) $ \cm ->
      createConcrete (mi2i cm) $ do
        let cflags = err (const noOptions) mflags (lookupModule gr cm)
        sequence_ [setConcreteFlag name value | (name,value) <- optionsPGF cflags]
        let id_prod = Production [] [PArg [] (LParam 0 [])] (LParam 0 []) [[SymCat 0 (LParam 0 [])]]
            prods   = ([id_prod],[id_prod])
            infos   = (((cPredefAbs,cInt),   CncCat (Just (noLoc GM.defLinType)) Nothing Nothing Nothing (Just prods))
                      :((cPredefAbs,cString),CncCat (Just (noLoc GM.defLinType)) Nothing Nothing Nothing (Just prods))
                      :((cPredefAbs,cFloat), CncCat (Just (noLoc GM.defLinType)) Nothing Nothing Nothing (Just prods))
                      : Look.allOrigInfos gr cm)
        forM_ infos createCncCats
        forM_ infos createCncFuns
  return pgf
  where
    aflags = err (const noOptions) mflags (lookupModule gr am)

    adefs =
        [((cPredefAbs,c), AbsCat (Just (L NoLoc []))) | c <- [cFloat,cInt,cString]] ++ 
        Look.allOrigInfos gr am

    toLogProb = realToFrac . negate . log

    cats = [(c', snd (mkContext [] cont), toLogProb (fromMaybe 0 (Map.lookup c' probs))) |
                               ((m,c),AbsCat (Just (L _ cont))) <- adefs, let c' = i2i c]

    funs = [(f', mkType [] ty, arity, bcode, toLogProb (fromMaybe 0 (Map.lookup f' funs_probs))) |
                               ((m,f),AbsFun (Just (L _ ty)) ma mdef _) <- adefs,
                               let arity = mkArity ma mdef ty,
                               let bcode = mkDef gr arity mdef,
                               let f' = i2i f]
                               
    funs_probs = (Map.fromList . concat . Map.elems . fmap pad . Map.fromListWith (++))
                    [(i2i cat,[(i2i f,Map.lookup f' probs)]) | ((m,f),AbsFun (Just (L _ ty)) _ _ _) <- adefs,
                                                               let (_,(_,cat),_) = GM.typeForm ty,
                                                               let f' = i2i f]
      where
        pad :: [(a,Maybe Double)] -> [(a,Double)]
        pad pfs = [(f,fromMaybe deflt mb_p) | (f,mb_p) <- pfs]
          where
            deflt = case length [f | (f,Nothing) <- pfs] of
                      0 -> 0
                      n -> max 0 ((1 - sum [d | (f,Just d) <- pfs]) / fromIntegral n)

    createCncCats ((m,c),CncCat (Just (L _ ty)) _ _ mprn (Just (lindefs,linrefs))) = do
      createLincat (i2i c) (type2fields gr ty) lindefs linrefs
      case mprn of
        Nothing        -> return ()
        Just (L _ prn) -> setPrintName (i2i c) (unwords (term2tokens prn))
    createCncCats _ = return ()

    createCncFuns ((m,f),CncFun _ _ mprn (Just prods)) = do
      createLin (i2i f) prods
      case mprn of
        Nothing        -> return ()
        Just (L _ prn) -> setPrintName (i2i f) (unwords (term2tokens prn))
    createCncFuns _ = return ()

    term2tokens (K tok)     = [tok]
    term2tokens (C t1 t2)   = term2tokens t1 ++ term2tokens t2
    term2tokens (Typed t _) = term2tokens t
    term2tokens _           = []

i2i :: Ident -> String
i2i = showIdent

mi2i :: ModuleName -> String
mi2i (MN i) = i2i i

mkType :: [Ident] -> A.Type -> PGF2.Type
mkType scope t =
  case GM.typeForm t of
    (hyps,(_,cat),args) -> let (scope',hyps') = mkContext scope hyps
                           in DTyp hyps' (i2i cat) (map (mkExp scope') args)

mkExp :: [Ident] -> A.Term -> Expr
mkExp scope t =
  case t of
    Q (_,c)  -> EFun (i2i c)
    QC (_,c) -> EFun (i2i c)
    Vr x     -> case lookup x (zip scope [0..]) of
                  Just i  -> EVar  i
                  Nothing -> EMeta 0
    Abs b x t-> EAbs b (i2i x) (mkExp (x:scope) t)
    App t1 t2-> EApp (mkExp scope t1) (mkExp scope t2)
    EInt i   -> ELit (LInt (fromIntegral i))
    EFloat f -> ELit (LFlt f)
    K s      -> ELit (LStr s)
    Meta i   -> EMeta i
    _        -> EMeta 0
{-
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
-}

mkContext :: [Ident] -> A.Context -> ([Ident],[PGF2.Hypo])
mkContext scope hyps = mapAccumL (\scope (bt,x,ty) -> let ty' = mkType scope ty
                                                      in if x == identW
                                                           then (  scope,(bt,i2i x,ty'))
                                                           else (x:scope,(bt,i2i x,ty'))) scope hyps 

mkDef gr arity (Just eqs) = generateByteCode gr arity eqs
mkDef gr arity Nothing    = []

mkArity (Just a) _        ty = a   -- known arity, i.e. defined function
mkArity Nothing  (Just _) ty = 0   -- defined function with no arity - must be an axiom
mkArity Nothing  _        ty = let (ctxt, _, _) = GM.typeForm ty  -- constructor
                               in length ctxt
{-
genCncCats gr am cm cdefs = mkCncCats 0 cdefs
  where
    mkCncCats index []                                                = (index,[])
    mkCncCats index (((m,id),CncCat (Just (L _ lincat)) _ _ _ _):cdefs) 
      | id == cInt    = 
            let cc            = pgfCncCat gr (i2i id) lincat fidInt
                (index',cats) = mkCncCats index cdefs
            in (index', cc : cats)
      | id == cFloat  = 
            let cc            = pgfCncCat gr (i2i id) lincat fidFloat
                (index',cats) = mkCncCats index cdefs
            in (index', cc : cats)
      | id == cString = 
            let cc            = pgfCncCat gr (i2i id) lincat fidString
                (index',cats) = mkCncCats index cdefs
            in (index', cc : cats)
      | otherwise     =
            let cc@(_, _s, e, _) = pgfCncCat gr (i2i id) lincat index
                (index',cats)    = mkCncCats (e+1) cdefs
            in (index', cc : cats)
    mkCncCats index (_                                          :cdefs) = mkCncCats index cdefs

genCncFuns :: Grammar
           -> ModuleName
           -> ModuleName
           -> Array SeqId [Symbol]
           -> ([Symbol] -> [Symbol] -> Ordering)
           -> Array SeqId [Symbol]
           -> [(QIdent, Info)]
           -> FId
           -> Map.Map PGF2.Cat (Int,Int)
           -> (FId,
               [(FId, [Production])],
               [(FId, [FunId])],
               [(FId, [FunId])],
               [(PGF2.Fun,[SeqId])])
genCncFuns gr am cm ex_seqs ciCmp seqs cdefs fid_cnt cnccat_ranges =
  let (fid_cnt1,funs_cnt1,funs1,lindefs,linrefs) = mkCncCats cdefs fid_cnt  0 [] IntMap.empty IntMap.empty
      (fid_cnt2,funs_cnt2,funs2,prods0)          = mkCncFuns cdefs fid_cnt1 funs_cnt1 funs1 lindefs Map.empty IntMap.empty
      prods                                      = [(fid,Set.toList prodSet) | (fid,prodSet) <- IntMap.toList prods0]
  in (fid_cnt2,prods,IntMap.toList lindefs,IntMap.toList linrefs,reverse funs2)
  where
    mkCncCats []                                                          fid_cnt funs_cnt funs lindefs linrefs =
      (fid_cnt,funs_cnt,funs,lindefs,linrefs)
    mkCncCats (((m,id),CncCat _ _ _ _ (Just (PMCFG prods0 funs0))):cdefs) fid_cnt funs_cnt funs lindefs linrefs =
      let !funs_cnt' = let (s_funid, e_funid) = bounds funs0
                       in funs_cnt+(e_funid-s_funid+1)
          lindefs'   = foldl' (toLinDef (am,id) funs_cnt) lindefs prods0
          linrefs'   = foldl' (toLinRef (am,id) funs_cnt) linrefs prods0
          funs'      = foldl' (toCncFun funs_cnt (m,mkLinDefId id)) funs (assocs funs0)
      in mkCncCats cdefs fid_cnt funs_cnt' funs' lindefs' linrefs'
    mkCncCats (_                                                  :cdefs) fid_cnt funs_cnt funs lindefs linrefs =
      mkCncCats cdefs fid_cnt funs_cnt funs lindefs linrefs

    mkCncFuns []                                                        fid_cnt funs_cnt funs lindefs crc prods =
      (fid_cnt,funs_cnt,funs,prods)
    mkCncFuns (((m,id),CncFun _ _ _ (Just (PMCFG prods0 funs0))):cdefs) fid_cnt funs_cnt funs lindefs crc prods =
      let ty_C           = err error (\x -> x) $ fmap GM.typeForm (Look.lookupFunType gr am id)
          !funs_cnt'     = let (s_funid, e_funid) = bounds funs0
                           in funs_cnt+(e_funid-s_funid+1)
          !(fid_cnt',crc',prods')
                         = foldl' (toProd lindefs ty_C funs_cnt)
                                  (fid_cnt,crc,prods) prods0
          funs'          = foldl' (toCncFun funs_cnt (m,id)) funs (assocs funs0)
      in mkCncFuns cdefs fid_cnt' funs_cnt' funs' lindefs crc' prods'
    mkCncFuns (_                                                :cdefs) fid_cnt funs_cnt funs lindefs crc prods = 
      mkCncFuns cdefs fid_cnt funs_cnt funs lindefs crc prods

    toProd lindefs (ctxt_C,res_C,_) offs st (A.Production fid0 funid0 args0) =
      let !((fid_cnt,crc,prods),args) = mapAccumL mkArg st (zip ctxt_C args0)
          set0    = Set.fromList (map (PApply (offs+funid0)) (sequence args))
          fid     = mkFId res_C fid0
          !prods' = case IntMap.lookup fid prods of
                     Just set -> IntMap.insert fid (Set.union set0 set) prods
                     Nothing  -> IntMap.insert fid set0 prods
      in (fid_cnt,crc,prods')
      where
        mkArg st@(fid_cnt,crc,prods) ((_,_,ty),fid0s) =
          case fid0s of
            [fid0] -> (st,map (flip PArg (mkFId arg_C fid0)) ctxt)
            fid0s  -> case Map.lookup fids crc of
                        Just fid -> (st,map (flip PArg fid) ctxt)
                        Nothing  -> let !crc'   = Map.insert fids fid_cnt crc
                                        !prods' = IntMap.insert fid_cnt (Set.fromList (map PCoerce fids)) prods
                                    in ((fid_cnt+1,crc',prods'),map (flip PArg fid_cnt) ctxt)
          where
            (hargs_C,arg_C) = GM.catSkeleton ty
            ctxt = mapM (mkCtxt lindefs) hargs_C
            fids = map (mkFId arg_C) fid0s

    mkLinDefId id = prefixIdent "lindef " id

    toLinDef res offs lindefs (A.Production fid0 funid0 args) =
      if args == [[fidVar]]
        then IntMap.insertWith (++) fid [offs+funid0] lindefs
        else lindefs
      where
        fid = mkFId res fid0

    toLinRef res offs linrefs (A.Production fid0 funid0 [fargs]) =
      if fid0 == fidVar
        then foldr (\fid -> IntMap.insertWith (++) fid [offs+funid0]) linrefs fids
        else linrefs
      where
        fids = map (mkFId res) fargs

    mkFId (_,cat) fid0 =
      case Map.lookup (i2i cat) cnccat_ranges of
        Just (s,e) -> s+fid0
        Nothing    -> error ("GrammarToPGF.mkFId: missing category "++showIdent cat)

    mkCtxt lindefs (_,cat) =
      case Map.lookup (i2i cat) cnccat_ranges of
        Just (s,e) -> [(fid,fid) | fid <- [s..e], Just _ <- [IntMap.lookup fid lindefs]]
        Nothing    -> error "GrammarToPGF.mkCtxt failed"

    toCncFun offs (m,id) funs (funid0,lins0) =
      let mseqs = case lookupModule gr m of
                    Ok (ModInfo{mseqs=Just mseqs}) -> mseqs
                    _                              -> ex_seqs
      in (i2i id, map (newIndex mseqs) (elems lins0)):funs
      where
        newIndex mseqs i = binSearch (mseqs ! i) seqs (bounds seqs)

        binSearch v arr (i,j)
          | i <= j    = case ciCmp v (arr ! k) of
                          LT -> binSearch v arr (i,k-1)
                          EQ -> k
                          GT -> binSearch v arr (k+1,j)
          | otherwise = error "binSearch"
          where
            k = (i+j) `div` 2


genPrintNames cdefs =
  [(i2i id, name) | ((m,id),info) <- cdefs, name <- prn info]
  where
    prn (CncFun _ _   (Just (L _ tr)) _) = [flatten tr]
    prn (CncCat _ _ _ (Just (L _ tr)) _) = [flatten tr]
    prn _                                = []

    flatten (K s)      = s
    flatten (Alts x _) = flatten x
    flatten (C x y)    = flatten x +++ flatten y

mkArray    lst = listArray (0,length lst-1) lst
mkMapArray map = array (0,Map.size map-1) [(v,k) | (k,v) <- Map.toList map]
mkSetArray set = listArray (0,Set.size set-1) (Set.toList set)

-- The following is a version of Data.List.sortBy which together
-- with the sorting also eliminates duplicate values 
sortNubBy cmp = mergeAll . sequences
  where
    sequences (a:b:xs) =
      case cmp a b of
        GT -> descending b [a]  xs
        EQ -> sequences (b:xs)
        LT -> ascending  b (a:) xs
    sequences xs = [xs]

    descending a as []     = [a:as]
    descending a as (b:bs) =
      case cmp a b of
        GT -> descending b (a:as) bs
        EQ -> descending a as bs
        LT -> (a:as) : sequences (b:bs)

    ascending a as []     = let !x = as [a]
                            in [x]
    ascending a as (b:bs) =
      case cmp a b of
        GT -> let !x = as [a]
              in x : sequences (b:bs)
        EQ -> ascending a as bs
        LT -> ascending b (\ys -> as (a:ys)) bs

    mergeAll [x] = x
    mergeAll xs  = mergeAll (mergePairs xs)

    mergePairs (a:b:xs) = let !x = merge a b
                          in x : mergePairs xs
    mergePairs xs       = xs

    merge as@(a:as') bs@(b:bs') =
      case cmp a b of
        GT -> b:merge as  bs'
        EQ -> a:merge as' bs'
        LT -> a:merge as' bs
    merge [] bs         = bs
    merge as []         = as

-- The following function does case-insensitive comparison of sequences.
-- This is used to allow case-insensitive parsing, while
-- the linearizer still has access to the original cases.

compareCaseInsensitive []     []     = EQ
compareCaseInsensitive []     _      = LT
compareCaseInsensitive _      []     = GT
compareCaseInsensitive (x:xs) (y:ys) =
  case compareSym x y of
    EQ -> compareCaseInsensitive xs ys
    x  -> x
  where
    compareSym s1 s2 =
      case s1 of
        SymCat d1 r1
          -> case s2 of
               SymCat d2 r2
                 -> case compare d1 d2 of
                      EQ -> r1 `compare` r2
                      x  -> x
               _ -> LT
        SymLit d1 r1
          -> case s2 of
               SymCat {} -> GT
               SymLit d2 r2
                 -> case compare d1 d2 of
                      EQ -> r1 `compare` r2
                      x  -> x
               _ -> LT
        SymVar d1 r1
          -> if tagToEnum# (getTag s2 ># 2#)
               then LT
               else case s2 of
                      SymVar d2 r2
                        -> case compare d1 d2 of
                             EQ -> r1 `compare` r2
                             x  -> x
                      _ -> GT
        SymKS t1
          -> if tagToEnum# (getTag s2 ># 3#)
               then LT
               else case s2 of
                      SymKS t2 -> t1 `compareToken` t2
                      _          -> GT
        SymKP a1 b1
          -> if tagToEnum# (getTag s2 ># 4#)
               then LT
               else case s2 of
                      SymKP a2 b2
                        -> case compare a1 a2 of
                             EQ -> b1 `compare` b2
                             x  -> x
                      _ -> GT
        _ -> let t1 = getTag s1
                 t2 = getTag s2
             in if tagToEnum# (t1 <# t2) 
                  then LT
                  else if tagToEnum# (t1 ==# t2)
                         then EQ
                         else GT
  
    compareToken []     []     = EQ
    compareToken []     _      = LT
    compareToken _      []     = GT
    compareToken (x:xs) (y:ys)
      | x == y    = compareToken xs ys
      | otherwise = case compare (toLower x) (toLower y) of
                      EQ -> case compareToken xs ys of
                              EQ -> compare x y
                              x  -> x
                      x  -> x
-}
