{-# LANGUAGE RankNTypes #-}
module GF.Compile.GeneratePMCFG
    (generatePMCFG, pmcfgForm, type2fields
    ) where

import GF.Infra.Ident
import GF.Infra.Option
import GF.Infra.CheckM
import GF.Data.Operations
import GF.Grammar.Grammar
import GF.Grammar.Lookup
import GF.Grammar.Macros
import GF.Grammar.Predef
import GF.Grammar.Printer hiding (ppValue)
import GF.Text.Pretty hiding (empty)
import GF.Compile.Compute hiding ( getMeta, setMeta, globals, variants )
import qualified GF.Text.Pretty as PP
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Applicative
import Control.Monad (foldM,zipWithM,liftM,liftM2,forM,MonadPlus(..))
import Control.Monad.Fix
import Data.Maybe
import Data.List(mapAccumL,sortBy,sortOn,intersperse)
import Data.Containers.ListUtils(nubOrd)
import Prelude hiding ((<>))


generatePMCFG :: Options -> FilePath -> SourceGrammar -> SourceModule -> Check SourceModule
generatePMCFG opts cwd gr cmo@(cm,cmi)
  | mstatus cmi == MSComplete && isModCnc cmi =
                do let gr' = prependModule gr cmo
                       g   = Gl gr' (stdPredef g) False
                   js <- Map.traverseWithKey (addPMCFG cwd g cmi) (jments cmi)
                   return (cm,cmi{jments = js})
  | otherwise = return cmo

addPMCFG cwd g cmi id (CncCat mty@(Just (L loc ty)) mdef mref mprn Nothing) = do
  defs <- case mdef of
            Nothing        -> checkInModule cwd cmi loc ("Happened in the rule generation for the lindef of" <+> id) $ do
                                t <- mkLinDefault sgr ty
                                pmcfgForm g t [(Explicit,identW,Sort cStr)] ty
            Just (L loc t) -> checkInModule cwd cmi loc ("Happened in the PMCFG generation for the lindef of" <+> id) $ do
                                pmcfgForm g t [(Explicit,identW,Sort cStr)] ty
  refs <- case mref of
            Nothing        -> checkInModule cwd cmi loc ("Happened in the rule generation for the linref of" <+> id) $ do
                                t <- mkLinReference sgr ty
                                pmcfgForm g t [(Explicit,identW,ty)] (Sort cStr)
            Just (L loc t) -> checkInModule cwd cmi loc ("Happened in the PMCFG generation for the linref of" <+> id) $ do
                                pmcfgForm g t [(Explicit,identW,ty)] (Sort cStr)
  mprn  <- case mprn of
             Nothing          -> return Nothing
             Just (L loc prn) -> checkInModule cwd cmi loc ("Happened in the computation of the print name for" <+> id) $ do
                                   prn <- normalForm g prn
                                   return (Just (L loc prn))
  return (CncCat mty mdef mref mprn (Just (defs,refs)))
  where
    Gl sgr _ _ = g
addPMCFG cwd g cmi id (CncFun (Just lty@(cats,cat,ctxt,ty)) mlin@(Just (L loc term)) mprn Nothing) = do
  rules <- checkInModule cwd cmi loc ("Happened in the rule generation for" <+> id) $
             pmcfgForm g term ctxt ty
  mprn  <- case mprn of
             Nothing          -> return Nothing
             Just (L loc prn) -> checkInModule cwd cmi loc ("Happened in the computation of the print name for" <+> id) $ do
                                   prn <- normalForm g prn
                                   return (Just (L loc prn))
  return (CncFun (Just lty) mlin mprn (Just rules))
  where
    Gl sgr _ _ = g

addPMCFG cwd g cmi id info = return info

pmcfgForm g t ctxt ty = do
  let (ms,s',t',arg_params) = apply 0 Map.empty unit ctxt t []
  let v = eval g [] s' t' []
  (ms,_,_,fn) <- breakDown g ms unit 0 [] v ty (return []) empty
  fmap nubOrd $ runGenM g ms [] $ do
    (r,rs,v,res_params) <- fn
    (subst,arg_params) <- mapAccumM params2int Map.empty arg_params
    (subst,res_params) <- params2int subst res_params
    (subst,lin_idx)    <- params2int' subst r rs
    (subst,seq)        <- flatten subst v
    qs <- quantifiers (Map.toList subst)
    return (Rule qs res_params arg_params lin_idx seq)
  where
    Gl sgr _ _ = g

    quantifiers vars = GenM (\(Gl sgr _ _) k svs ms ->
      k [boundsOf sgr ms variable | (variable,v) <- sortOn snd vars]
        svs ms)
      where
        boundsOf sgr ms i =
          case Map.lookup i ms of
            Just (Narrowing _ pty) -> case countParamValues sgr pty of
                                        Ok c    -> c
                                        Bad msg -> error msg
            _                      -> error (show (ppLVar i <+> "is not a free variable"))

    apply d ms s []              t args = (ms,s,t,reverse args)
    apply d ms s ((_,_,ty):ctxt) t args =
       let (ms',s',_,t2,params) = type2metaTerm sgr d ms s 0 [] ty []
       in apply (d+1) ms' s' ctxt (App t t2) (params:args)

type2fields :: SourceGrammar -> Type -> [String]
type2fields gr = map show . type2fields PP.empty
  where
    type2fields d (Sort s) | s == cStr = [show d]
    type2fields d (RecType lbls) =
      concatMap (\(lbl,_,ty) -> type2fields (d <+> pp lbl) ty) lbls
    type2fields d (Table p q) =
      let Ok ts = allParamValues gr p
      in concatMap (\t -> type2fields (d <+> ppTerm Unqualified 5 t) q) ts
    type2fields d _ = []


mkLinDefault :: SourceGrammar -> Type -> Check Term
mkLinDefault gr typ = liftM (Abs Explicit varStr) $ mkDefField typ
 where
   mkDefField ty =
     case ty of
       Table p t  -> do t' <- mkDefField t
                        let T _ cs = mkWildCases t'
                        return $ T (TWild p) cs
       Sort s | s == cStr -> return (Vr varStr)
       QC p       -> case allParamValues gr ty of
                       Ok []    -> checkError ("no parameter values given to type" <+> ppQIdent Qualified p)
                       Ok (v:_) -> return v
                       Bad msg  -> fail msg
       RecType r -> do
         let (ls,_,ts) = unzip3 r
         ts <- mapM mkDefField ts
         return $ R (zipWith assign ls ts)
       _ | Just _ <- isTypeInts ty -> return $ EInt 0 -- exists in all as first val
       _ -> checkError ("a field in a linearization type cannot be" <+> ty)

mkLinReference :: SourceGrammar -> Type -> Check Term
mkLinReference gr typ = do
  mb_term <- mkRefField typ (Vr varStr)
  return (Abs Explicit varStr (fromMaybe Empty mb_term))
  where
    mkRefField ty trm =
      case ty of
        Table pty ty -> do ps <- allParamValues gr pty
                           case ps of
                             []     -> fail (render ("no parameter values given to type" <+> pty))
                             (p:ps) -> mkRefField ty (S trm p)
        Sort s | s == cStr -> return (Just trm)
        QC p       -> return Nothing
        RecType rs -> traverse rs trm
        _ | Just _ <- isTypeInts ty -> return Nothing
        _ -> fail (render ("a field in a linearization type cannot be" <+> typ))

    traverse []            trm = return Nothing
    traverse ((l,_,ty):rs) trm = do res <- mkRefField ty (P trm l)
                                    case res of
                                      Just trm -> return (Just trm)
                                      Nothing  -> traverse rs trm


type2metaTerm :: SourceGrammar -> Int -> MetaVars -> Choice -> LIndex -> [(LIndex,(Ident,Type))] -> Type -> [(Value,Type)] -> (MetaVars,Choice,Int,Term,[(Value,Type)])
type2metaTerm gr d ms s r rs (Sort srt)     params | srt == cStr = (ms,s,r+1,TSymCat d r rs,params)
type2metaTerm gr d ms s r rs (RecType lbls) params =
  let ((ms',s',r',params'),ass) =
          mapAccumL (\(ms,s,r,params) (lbl,_,ty) -> case lbl of
                                                      LVar j -> ((ms,s,r,params),(lbl,(Just ty,TSymVar d j)))
                                                      lbl    -> let (ms',s',r',t,params') = type2metaTerm gr d ms s r rs ty params
                                                                in ((ms',s',r',params'),(lbl,(Just ty,t))))
                    (ms,s,r,params) lbls
  in (ms',s',r',R ass,params')
type2metaTerm gr d ms s r rs (Table p q) params
  | count == 1 = let (ms',s',r',t,params') = type2metaTerm gr d ms s r rs q params
                 in (ms',s',r+(r'-r),T (TTyped p) [(PW,t)],params')
  | otherwise  = let pv     = varX (length rs+1)
                     (ms',s',r',t,params') = type2metaTerm gr d ms s r ((delta,(pv,p)):rs) q params
                     delta  = r'-r
                 in (ms',s',r+delta*count,T (TTyped p) [(PV pv,t)],params')
  where
    count = case countParamValues gr p of
              Ok c    -> c
              Bad msg -> error msg
type2metaTerm gr d ms c r rs ty@(QC q) params =
  let i = Map.size ms + 1
      (c1,c2) = split c
  in (Map.insert i (Narrowing c1 ty) ms,c2,r,Meta i,(VMeta i [],ty):params)
type2metaTerm gr d ms c r rs ty params
  | Just n <- isTypeInts ty =
      let i = Map.size ms + 1
          (c1,c2) = split c
      in (Map.insert i (Narrowing c1 ty) ms,c2,r,Meta i,(VMeta i [],ty):params)


breakDown g ms s r rs v (Sort sort)    fn0 fn
  | sort == cStr                              =
      let fn' = do params <- fn0
                   v      <- force v
                   return (r,rs,v,params)
                <|>
                do fn
      in return (ms,r+1,fn0,fn')
breakDown g ms s r rs v (RecType lbls) fn0 fn = traverse ms r rs lbls fn0 fn
  where
    traverse ms r rs []                fn0 fn = return (ms,r,fn0,fn)
    traverse ms r rs ((lbl,_,ty):lbls) fn0 fn = do (ms,r,fn0,fn) <- breakDown g ms s r rs (project v) ty fn0 fn
                                                   traverse ms r rs lbls fn0 fn
      where
        project (VR as)       = case lookup lbl as of
                                  Nothing -> error (render ("Missing value for label" <+> pp lbl $$
                                                            "in" <+> ppValue Unqualified 0 (VR as)))
                                  Just v  -> v
        project (VFV c fvs)   = VFV c (fmap project fvs)
        project (VMeta i vs)  = VSusp i (\v -> project (apply g v vs)) []
        project (VSusp i k vs)= VSusp i (\v -> project (apply g (k v) vs)) []
        project v             = VP v lbl []
breakDown g ms c r rs v (Table p q) fn0 fn = do
  let i  = Map.size ms + 1
      v2 = VMeta i []
      v0 = VS v v2 []
      (c1,c2) = split c
      Gl gr _ _ = g
  cnt <- countParamValues gr p
  (ms,r',fn0,fn) <- mfix $ \(~(_,r',_,_)) ->
       breakDown g (Map.insert i (Narrowing c1 p) ms) c2 r ((r'-r,(v2,p)):rs) (select v0 v v2) q fn0 fn
  return (ms,r+(r'-r)*cnt,fn0,fn)
  where
    select v0 (VT _  env s cs) v2 = patternMatch g s v0 (map (\(p,t) -> (env,[p],[v2],t)) cs)
    select v0 (VV vty tvs)     v2 = vtableSelect g v0 vty tvs v2 []
    select v0 (VFV i fvs)      v2 = VFV i (fmap (\v1 -> select v0 v1 v2) fvs)
    select v0 (VMeta i vs)     v2 = VSusp i (\v -> select v0 (apply g v vs) v2) []
    select v0 (VSusp i k vs)   v2 = VSusp i (\v -> select v0 (apply g (k v) vs) v2) []
    select v0 v1               v2 = v0
breakDown g ms s r rs v ty@(QC q) fn0 fn =
  let fn0' = do params <- fn0
                v <- force v
                return ((v,ty):params)
      fn'  = do (r,rs,v',res_params) <- fn
                v <- force v
                return (r,rs,v',(v,ty):res_params)
  in return (ms,r,fn0',fn')
breakDown g ms s r rs v ty@(App (Q q) _) fn0 fn =
  let fn0' = do params <- fn0
                v <- force v
                return ((v,ty):params)
      fn'  = do (r,rs,v',res_params) <- fn
                v <- force v
                return (r,rs,v',(v,ty):res_params)
  in return (ms,r,fn0',fn')

force (VStr s) = return (VStr s)
force (VInt n) = return (VInt n)
force (VFlt d) = return (VFlt d)
force (VSymCat d r rs) = do
  rs <- mapM force_ rs
  return (VSymCat d r rs)
  where
    force_ (factor, (v, ty)) = do
      v <- force v
      return (factor, (v, ty))
force (VApp q vs) = do
  vs <- mapM force vs
  return (VApp q vs)
force (VAlts def alts) = do
  def  <- force def
  alts <- mapM force_ alts
  return (VAlts def alts)
  where
    force_ (x,y) = do
      x <- force x
      y <- force y
      return (x,y)
force VEmpty     = return VEmpty
force (VC v1 v2) = do
  v1 <- force v1
  v2 <- force v2
  return (VC v1 v2)
force (VMeta i vs) = do
  vs <- mapM force vs
  return (VMeta i vs)
force (VSusp i k vs) = do
  vs <- mapM force vs
  st <- getMeta i
  v <- case st of
         Narrowing c ty -> do v <- chooseMetaValue c ty
                              setMeta i (Bound undefined v)
                              return v
         Bound _ v      -> return v
  g <- globals
  force (apply g (k v) vs)
force (VStrs vs) = do
  vs <- mapM force vs
  return (VStrs vs)
force (VR as) = do
  as <- mapM (\(l,v) -> fmap ((,) l) (force v)) as
  return (VR as)
force v@(VPatt _ _ _) = return v
force (VFV c vs) = do
  v <- variants c (unvariants vs)
  force v
force v = compileError ("Cannot evaluate" <+> ppValue Unqualified 0 v)


flatten subst (VStr s) = return (subst,[SymKS s])
flatten subst (VSymCat d r rs) = do
  (subst,lin_index) <- params2int' subst r rs
  return (subst,[SymCat d lin_index])
flatten subst (VApp (m,id) [])
  | m == cPredef && id == cBIND       = return (subst,[SymBIND])
  | m == cPredef && id == cSOFT_BIND  = return (subst,[SymSOFT_BIND])
  | m == cPredef && id == cSOFT_SPACE = return (subst,[SymSOFT_SPACE])
  | m == cPredef && id == cNonExist   = return (subst,[SymNE])
  | m == cPredef && id == cCAPIT      = return (subst,[SymCAPIT])
  | m == cPredef && id == cALL_CAPIT  = return (subst,[SymALL_CAPIT])
flatten subst v0@(VAlts def alts) = do
  (subst,def) <- flatten subst def
  (subst,alts) <- mapAccumM (\subst (alt,ps) -> do
                                (subst,alt) <- flatten subst alt
                                ps  <- to_strs ps
                                return (subst,(alt,ps)))
                            subst
                            alts
  return (subst,[SymKP def alts])
  where
    to_strs (VStrs vs)    = mapM to_str vs
    to_strs (VPatt _ _ p) = from_patt p
    to_strs v             = fail

    to_str (VStr s) = return s
    to_str _        = fail

    from_patt (PAlt p1 p2) = liftM2 (++) (from_patt p1) (from_patt p2)
    from_patt (PSeq _ _ p1 _ _ p2) = liftM2 (liftM2 (++)) (from_patt p1) (from_patt p2)
    from_patt (PString s)  = return [s]
    from_patt (PChars cs)  = return (map (:[]) cs)
    from_patt _            = fail

    fail = compileError ("Complex patterns are not supported in:" $$ nest 2 (ppValue Unqualified 0 v0))
flatten subst VEmpty     = return (subst,[])
flatten subst (VC v1 v2) = do
  (subst,s1) <- flatten subst v1
  (subst,s2) <- flatten subst v2
  return (subst,s1++s2)
flatten subst (VSusp i k vs) = do
  st <- getMeta i
  v <- case st of
         Narrowing c ty -> do v <- chooseMetaValue c ty
                              setMeta i (Bound undefined v)
                              return v
         Bound _ v      -> return v
  g <- globals
  flatten subst (apply g (k v) vs)
flatten subst (VFV c vs) = do
  v <- variants c (unvariants vs)
  flatten subst v
flatten subst v = compileError ("Cannot evaluate" <+> ppValue Unqualified 0 v  <+> "to a string")


params2int subst rs = do
  (subst,r,rs,_) <- compute subst rs
  return (subst,LParam r (order rs))
  where
    compute subst []              = return (subst,0,[],1)
    compute subst ((v,ty):params) = do
      (subst, r, rs, cnt ) <- param2int subst v ty
      (subst, r',rs',cnt') <- compute subst params
      return (subst, r*cnt'+r',combine cnt' rs rs',cnt*cnt')

params2int' subst r0 rs = do
  (subst,r,rs) <- compute subst rs
  return (subst,LParam (r0+r) (order rs))
  where
    compute subst []                     = return (subst,0,[])
    compute subst ((cnt',(v,ty)):params) = do
      (subst, r, rs, cnt) <- param2int subst v ty
      (subst, r',rs')     <- compute subst params
      return (subst,r*cnt'+r',combine cnt' rs rs')

param2int subst (VR as) (RecType lbls) = compute subst lbls
  where
    compute subst []                = return (subst,0,[],1)
    compute subst ((lbl,_,ty):lbls) = do
      case lookup lbl as of
        Just v   -> do (subst, r, rs ,cnt ) <- param2int subst v ty
                       (subst, r',rs',cnt') <- compute subst lbls
                       return (subst,r*cnt'+r',combine' cnt rs cnt' rs',cnt*cnt')
        Nothing  -> compileError ("Missing value for label" <+> pp lbl $$
                                  "among" <+> hsep (punctuate (pp ',') (map fst as)))
param2int subst (VApp q vs) ty = do
  (      r ,    ctxt,cnt ) <- getIdxCnt q
  (subst,r',rs',     cnt') <- compute subst ctxt vs
  return (subst,r+r',rs',cnt)
  where
    compute subst []              []     = return (subst,0,[],1)
    compute subst ((_,_,ty):ctxt) (v:vs) = do
      (subst, r, rs ,cnt ) <- param2int subst v ty
      (subst, r',rs',cnt') <- compute subst ctxt vs
      return (subst,r*cnt'+r',combine' cnt rs cnt' rs',cnt*cnt')
param2int subst (VInt n) ty
  | Just max <- isTypeInts ty= return (subst,fromIntegral n,[],fromIntegral max+1)
param2int subst (VMeta i _) ty = do
  st <- getMeta i
  case st of
    Narrowing c ty -> do count <- getCnt ty
                         case Map.lookup i subst of
                           Just v  -> return (subst,0,[(1,v)],count)
                           Nothing -> let v      = Map.size subst
                                          subst' = Map.insert i v subst
                                      in return (subst',0,[(1,v)],count)
    Bound _ v      -> param2int subst v ty
param2int subst (VSusp i k vs) ty = do
  st <- getMeta i
  v <- case st of
         Narrowing c ty -> do v <- chooseMetaValue c ty
                              setMeta i (Bound undefined v)
                              return v
         Bound _ v      -> return v
  g <- globals
  param2int subst (apply g (k v) vs) ty
param2int subst (VFV c vs) ty = do
  v <- variants c (unvariants vs)
  param2int subst v ty
param2int subst v ty = compileError ("the parameter:" <+> ppValue Unqualified 0 v $$
                                     "cannot be evaluated at compile time.")

combine' 1   rs 1    rs' = []
combine' 1   rs cnt' rs' = rs'
combine' cnt rs 1    rs' = rs
combine' cnt rs cnt' rs' = combine cnt' rs rs'

combine cnt'          []            rs' = rs'
combine cnt'          rs             [] = [(r*cnt',pv) | (r,pv) <- rs]
combine cnt' ((r,pv):rs) ((r',pv'):rs') =
  case compare pv pv' of
    LT -> (r*cnt',   pv ) : combine cnt' rs ((r',pv'):rs')
    EQ -> (r*cnt'+r',pv ) : combine cnt' rs ((r',pv'):rs')
    GT -> (       r',pv') : combine cnt' ((r,pv):rs) rs'


type ChoiceMap = Map.Map Choice Int
type MetaVars = Map.Map Int MetaState

newtype GenM a = GenM {unGen :: forall r . Globals -> (a -> ChoiceMap -> MetaVars -> r -> Check r) -> ChoiceMap -> MetaVars -> r -> Check r}

instance Functor GenM where
  fmap f (GenM m) = GenM (\g k -> m g (k . f))

instance Applicative GenM where
  pure x = GenM (\g k -> k x)
  (GenM f) <*> (GenM h) = GenM (\g k -> f g (\fn -> h g (\x -> k (fn x))))

instance Alternative GenM where
  empty = GenM (\g k svs ms r -> pure r)
  (GenM f) <|> (GenM h) = GenM (\g k svs ms r -> f g k svs ms r >>= h g k svs ms)

instance Monad GenM where
  (GenM f) >>= h = GenM (\g k -> f g (\x -> case h x of {GenM h -> h g k}))

instance MonadFail GenM where
  fail msg = GenM (\_ _ _ _ _ -> fail msg)

runGenM g ms r (GenM f) = f g (\x svs ms xs -> pure (x:xs)) Map.empty ms r

compileError d = GenM (\_ _ _ _ _ -> checkError d)

globals = GenM $ \g k -> k g

variants :: Choice -> [a] -> GenM a
variants c xs = GenM (\g k svs ms r ->
                          case Map.lookup c svs of
                            Just j  -> k (xs !! j) svs ms r
                            Nothing -> foldM (\r (j,x) -> k x (Map.insert c j svs) ms r) r (zip [0..] xs))

newMeta c ty = GenM $ \_ k svs ms ->
  let i = Map.size ms + 1
  in k i svs (Map.insert i (Narrowing c ty) ms)

getMeta i = GenM $ \_ k svs ms r ->
  case Map.lookup i ms of
    Just v  -> k v svs ms r
    Nothing -> checkError (pp "Meta variable" <+> ppMeta i <+> "is not defined")

setMeta i st = GenM $ \_ k svs ms ->
  k () svs (Map.insert i st ms)

getCnt ty = GenM $ \(Gl gr _ _) k svs ms r ->
  case countParamValues gr ty of
    Ok c    -> k c svs ms r
    Bad msg -> checkError (pp msg)

getIdxCnt q = GenM $ \(Gl gr _ _) k svs ms r ->
  case lookupOrigInfo gr q of
    Ok (_,ResValue (L _ ty) idx) -> 
      let (ctxt,QC p) = typeFormCnc ty
      in case lookupOrigInfo gr p of
           Ok (_,ResParam _ (Just (_,cnt))) -> k (idx,ctxt,cnt) svs ms r
           Bad msg -> checkError (pp msg)
    Bad msg -> checkError (pp msg)

chooseMetaValue :: Choice -> Type -> GenM Value
chooseMetaValue s ptyp = GenM $ \g@(Gl gr _ _) k svs ms r ->
  case ptyp of
    _ | Just n <- isTypeInts ptyp -> foldM (\r i -> k (VInt i) svs ms r) r [0..n]
    QC c -> do (mod,info) <- lookupOrigInfo gr c
               case info of
                 ResParam (Just ps) _ -> mkValue mod k svs ms r 0 (unLoc ps)
                 _                    -> checkError (ppQIdent Qualified c <+> "has no parameter values defined")
    Q  c -> lookupResDef gr c >>= \ty -> unGen (chooseMetaValue s ty) g k svs ms r
    RecType lbls -> unGen (mapAccumM mkField s lbls >>= \(_,lbls) -> return (VR lbls)) g k svs ms r
    _ -> checkError ("cannot find parameter values for" <+> ptyp)
  where
    mkValue mod k svs ms r idx []             = return r
    mkValue mod k svs ms r idx ((id,ctxt):ps) = do
      let (ms',args) = mkVars ms s ctxt
      r <- k (VApp (mod,id) args) (Map.insert s idx svs) ms' r
      mkValue mod k svs ms r (idx+1) ps

    mkVars ms c []              = (ms,[])
    mkVars ms c ((_,_,ty):ctxt) =
      let i = Map.size ms + 1
          (c1,c2) = split c
          (ms',args) = mkVars (Map.insert i (Narrowing c1 ty) ms) c2 ctxt
      in (ms',VMeta i []:args)

    mkField c (l,_,ty) = do
       let (c1,c2) = split c
       v <- chooseMetaValue c1 ty
       return (c2,(l,v))

order :: Ord a => [(a,b)] -> [(a,b)]
order = sortBy (\(r1,_) (r2,_) -> compare r2 r1)

mapAccumM f a []     = return (a,[])
mapAccumM f a (x:xs) = do (a, y) <- f a x
                          (a,ys) <- mapAccumM f a xs
                          return (a,y:ys)
