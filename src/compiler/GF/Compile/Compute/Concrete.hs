{-# LANGUAGE RankNTypes, BangPatterns, CPP #-}

-- | Functions for computing the values of terms in the concrete syntax, in
-- | preparation for PMCFG generation.
module GF.Compile.Compute.Concrete
           ( normalForm
           , Value(..), Thunk, ThunkState(..), Env, Scope, showValue
           , MetaThunks
           , EvalM(..), runEvalM, runEvalOneM, evalError, evalWarn
           , eval, apply, force, value2term, patternMatch
           , newThunk, newEvaluatedThunk
           , newResiduation, newNarrowing, getVariables
           , getRef, setRef
           , getResDef, getInfo, getResType, getAllParamValues
           ) where

import Prelude hiding ((<>)) -- GHC 8.4.1 clash with Text.PrettyPrint

import GF.Grammar hiding (Env, VGen, VApp, VRecType)
import GF.Grammar.Lookup(lookupResDef,lookupResType,
                         lookupOrigInfo,lookupOverloadTypes,
                         allParamValues)
import GF.Grammar.Predef
import GF.Grammar.Lockfield(lockLabel)
import GF.Grammar.Printer
import GF.Data.Operations(Err(..))
import GF.Infra.CheckM
import GF.Infra.Option
import Data.STRef
import Data.Maybe(fromMaybe)
import Data.List
import Data.Char
import Control.Monad
import Control.Monad.ST
import Control.Applicative hiding (Const)
import qualified Control.Monad.Fail as Fail
import qualified Data.Map as Map
import GF.Text.Pretty
import PGF2.Transactions(LIndex)

-- * Main entry points

normalForm :: Grammar -> Term -> Check Term
normalForm gr t =
  fmap mkFV (runEvalM gr (eval [] t [] >>= value2term []))
  where
    mkFV [t] = t
    mkFV ts  = FV ts

type Sigma s = Value s

data ThunkState s
  = Unevaluated (Env s) Term
  | Evaluated {-# UNPACK #-} !Int (Value s)
  | Hole        {-# UNPACK #-} !MetaId
  | Residuation {-# UNPACK #-} !MetaId (Scope s) (Value s)
  | Narrowing   {-# UNPACK #-} !MetaId Type

type Thunk s = STRef s (ThunkState s)
type Env s = [(Ident,Thunk s)]
type Scope s = [(Ident,Value s)]

data Value s
  = VApp QIdent [Thunk s]
  | VMeta (Thunk s) (Env s) [Thunk s]
  | VSusp (Thunk s) (Env s) (Value s -> EvalM s (Value s)) [Thunk s]
  | VGen  {-# UNPACK #-} !Int [Thunk s]
  | VClosure (Env s) Term
  | VProd BindType Ident (Value s) (Value s)
  | VRecType [(Label, Value s)]
  | VR [(Label, Thunk s)]
  | VP (Value s) Label [Thunk s]
  | VExtR (Value s) (Value s)
  | VTable (Value s) (Value s)
  | VT (Value s) (Env s) [Case]
  | VV (Value s) [Thunk s]
  | VS (Value s) (Thunk s) [Thunk s]
  | VSort Ident
  | VInt Integer
  | VFlt Double
  | VStr String
  | VEmpty
  | VC (Value s) (Value s)
  | VGlue (Value s) (Value s)
  | VPatt Int (Maybe Int) Patt
  | VPattType (Value s)
  | VAlts (Value s) [(Value s, Value s)]
  | VStrs [Value s]
    -- These last constructors are only generated internally
    -- in the PMCFG generator.
  | VSymCat Int LIndex [(LIndex, (Thunk s, Type))]
  | VSymVar Int Int


showValue (VApp q tnks) = "(VApp "++unwords (show q : map (const "_") tnks) ++ ")"
showValue (VMeta _ _ _) = "VMeta"
showValue (VSusp _ _ _ _) = "VSusp"
showValue (VGen _ _) = "VGen"
showValue (VClosure _ _) = "VClosure"
showValue (VProd _ _ _ _) = "VProd"
showValue (VRecType _) = "VRecType"
showValue (VR lbls) = "(VR {"++unwords (map (\(lbl,_) -> show lbl) lbls)++"})"
showValue (VP v l _) = "(VP "++showValue v++" "++show l++")"
showValue (VExtR _ _) = "VExtR"
showValue (VTable _ _) = "VTable"
showValue (VT _ _ cs) = "(VT "++show cs++")"
showValue (VV _ _) = "VV"
showValue (VS v _ _) = "(VS "++showValue v++")"
showValue (VSort _) = "VSort"
showValue (VInt _) = "VInt"
showValue (VFlt _) = "VFlt"
showValue (VStr s) = "(VStr "++show s++")"
showValue VEmpty = "VEmpty"
showValue (VC _ _) = "VC"
showValue (VGlue _ _) = "VGlue"
showValue (VPatt _ _ _) = "VPatt"
showValue (VPattType _) = "VPattType"
showValue (VAlts _ _) = "VAlts"
showValue (VStrs _) = "VStrs"
showValue (VSymCat _ _ _) = "VSymCat"

eval env (Vr x)         vs  = do (tnk,depth) <- lookup x env
                                 withVar depth $ do
                                   v <- force tnk
                                   apply v vs
                              where
                                lookup x []   = evalError ("Variable" <+> pp x <+> "is not in scope")
                                lookup x ((y,tnk):env)
                                  | x == y    = return (tnk,length env)
                                  | otherwise = lookup x env
eval env (Sort s)       []  = return (VSort s)
eval env (EInt n)       []  = return (VInt n)
eval env (EFloat d)     []  = return (VFlt d)
eval env (K t)          []  = return (VStr t)
eval env Empty          []  = return VEmpty
eval env (App t1 t2)    vs  = do tnk <- newThunk env t2
                                 eval env t1 (tnk : vs)
eval env (Abs b x t)    []  = return (VClosure env (Abs b x t))
eval env (Abs b x t) (v:vs) = eval ((x,v):env) t vs
eval env (Meta i)       vs  = do tnk <- newHole i
                                 return (VMeta tnk env vs)
eval env (ImplArg t)    []  = eval env t []
eval env (Prod b x t1 t2)[] = do v1 <- eval env t1 []
                                 return (VProd b x v1 (VClosure env t2))
eval env (Typed t ty)   vs  = eval env t vs
eval env (RecType lbls) []  = do lbls <- mapM (\(lbl,ty) -> fmap ((,) lbl) (eval env ty [])) lbls
                                 return (VRecType (sortRec lbls))
eval env (R as)         []  = do as <- mapM (\(lbl,(_,t)) -> fmap ((,) lbl) (newThunk env t)) as
                                 return (VR as)
eval env (P t lbl)      vs  = do v <- eval env t []
                                 case v of
                                   VR as -> case lookup lbl as of
                                              Nothing  -> evalError ("Missing value for label" <+> pp lbl $$
                                                                     "in" <+> pp (P t lbl))
                                              Just tnk -> do v <- force tnk
                                                             apply v vs
                                   v     -> return (VP v lbl vs)
eval env (ExtR t1 t2)   []  = do v1 <- eval env t1 []
                                 v2 <- eval env t2 []
                                 case (v1,v2) of
                                   (VR       as1,VR       as2) -> return (VR       (foldl (\as (lbl,v) -> update lbl v as) as1 as2))
                                   (VRecType as1,VRecType as2) -> return (VRecType (foldl (\as (lbl,v) -> update lbl v as) as1 as2))
                                   _                           -> return (VExtR v1 v2)
eval env (Table t1 t2)  []  = do v1 <- eval env t1 []
                                 v2 <- eval env t2 []
                                 return (VTable v1 v2)
eval env (T (TTyped ty) cs)[]=do vty <- eval env ty []
                                 return (VT vty env cs)
eval env (T (TWild ty) cs) []=do vty <- eval env ty []
                                 return (VT vty env cs)
eval env (V ty ts)      []  = do vty <- eval env ty []
                                 tnks <- mapM (newThunk env) ts
                                 return (VV vty tnks)
eval env (S t1 t2)      vs  = do v1   <- eval env t1 []
                                 tnk2 <- newThunk env t2
                                 let v0 = VS v1 tnk2 vs
                                 case v1 of
                                   VT _  env cs -> patternMatch v0 (map (\(p,t) -> (env,[p],tnk2:vs,t)) cs)
                                   VV vty tnks  -> do ty <- value2term (map fst env) vty
                                                      vtableSelect v0 ty tnks tnk2 vs
                                   v1           -> return v0
eval env (Let (x,(_,t1)) t2) vs = do tnk <- newThunk env t1
                                     eval ((x,tnk):env) t2 vs
eval env (Q q@(m,id))   vs
  | m == cPredef            = do vs' <- mapM force vs
                                 mb_res <- evalPredef id vs'
                                 case mb_res of
                                   Const res -> return res
                                   RunTime   -> return (VApp q vs)
                                   NonExist  -> return (VApp (cPredef,cNonExist) [])
  | otherwise               = do t <- getResDef q
                                 eval env t vs
eval env (QC q)         vs  = return (VApp q vs)
eval env (C t1 t2)      []  = do v1 <- eval env t1 []
                                 v2 <- eval env t2 []
                                 case (v1,v2) of
                                   (VEmpty,VEmpty) -> return VEmpty
                                   (v1,    VEmpty) -> return v1
                                   (VEmpty,v2    ) -> return v2
                                   _               -> return (VC v1 v2)
eval env t@(Glue t1 t2) []  = do v1 <- eval env t1 []
                                 v2 <- eval env t2 []
                                 let glue v =
                                      case value2string' v False [] [] of
                                        Const (b,ws,qs) -> let b' = case v1 of
                                                                      VEmpty -> b
                                                                      _      -> True
                                                           in case value2string' v1 b' ws qs of
                                                                Const (b,ws,qs) -> Just (bind b ws (foldl (\v q->VC v (VApp q [])) (string2value' ws) qs))
                                                                NonExist        -> Just (VApp (cPredef,cNonExist) [])
                                                                RunTime         -> Nothing
                                        NonExist     -> Just (VApp (cPredef,cNonExist) [])
                                        RunTime      -> Nothing
                                     bind True  (_:_) v = VC (VApp (cPredef,cBIND) []) v
                                     bind _     _     v = v
                                 case (case v2 of
                                        (VAlts d vas) -> do d <- glue d
                                                            vas <- mapM (\(v,ss) -> glue v >>= \v -> return (v,ss)) vas
                                                            return (VAlts d vas)
                                        _             -> do glue v2) of
                                   Just v  -> return v
                                   Nothing -> return (VGlue v1 v2)
eval env (EPatt min max p) [] = return (VPatt min max p)
eval env (EPattType t)  []  = do v <- eval env t []
                                 return (VPattType v)
eval env (ELincat c ty) []  = do v <- eval env ty []
                                 let lbl = lockLabel c
                                     lv  = VRecType []
                                 case v of
                                   (VRecType as) -> return (VRecType (update lbl lv as))
                                   _             -> return (VExtR v (VRecType [(lbl,lv)]))
eval env (ELin c t)     []  = do v <- eval env t []
                                 let lbl = lockLabel c
                                 tnk <- newEvaluatedThunk (VR [])
                                 case v of
                                   (VR as) -> return (VR (update lbl tnk as))
                                   _       -> return (VExtR v (VR [(lbl,tnk)]))
eval env (FV ts)        vs  = msum [eval env t vs | t <- ts]
eval env (Alts d as)    []  = do vd <- eval env d []
                                 vas <- forM as $ \(t,s) -> do
                                           vt <- eval env t []
                                           vs <- eval env s []
                                           return (vt,vs)
                                 return (VAlts vd vas)
eval env (Strs ts)      []  = do vs <- mapM (\t -> eval env t []) ts
                                 return (VStrs vs)
eval env (TSymCat d r rs) []= do rs <- forM rs $ \(i,(pv,ty)) ->
                                         case lookup pv env of
                                           Just tnk -> return (i,(tnk,ty))
                                           Nothing  -> evalError ("Variable" <+> pp pv <+> "is not in scope")
                                 return (VSymCat d r rs)
eval env (TSymVar d r)  []  = do return (VSymVar d r)
eval env t              vs  = evalError ("Cannot reduce term" <+> pp t)

apply (VMeta m env vs0)             vs  = return (VMeta m env   (vs0++vs))
apply (VSusp m env k vs0)           vs  = return (VSusp m env k (vs0++vs))
apply (VApp f  vs0)                 vs  = return (VApp f (vs0++vs))
apply (VGen i  vs0)                 vs  = return (VGen i (vs0++vs))
apply (VClosure env (Abs b x t)) (v:vs) = eval ((x,v):env) t vs
apply v                             []  = return v

evalPredef id [v]
  | id == cLength = case value2string v of
                      Const s -> return (Const (VInt (genericLength s)))
                      _       -> return RunTime
evalPredef id [v1,v2]
  | id == cTake   = return (fmap string2value (liftA2 genericTake (value2int v1) (value2string v2)))
evalPredef id [v1,v2]
  | id == cDrop   = return (fmap string2value (liftA2 genericDrop (value2int v1) (value2string v2)))
evalPredef id [v1,v2]
  | id == cTk     = return (fmap string2value (liftA2 genericTk (value2int v1) (value2string v2)))
  where
    genericTk n = reverse . genericDrop n . reverse
evalPredef id [v1,v2]
  | id == cDp     = return (fmap string2value (liftA2 genericDp (value2int v1) (value2string v2)))
  where
    genericDp n = reverse . genericTake n . reverse
evalPredef id [v]
  | id == cIsUpper= return (fmap toPBool (liftA (all isUpper) (value2string v)))
evalPredef id [v]
  | id == cToUpper= return (fmap string2value (liftA (map toUpper) (value2string v)))
evalPredef id [v]
  | id == cToLower= return (fmap string2value (liftA (map toLower) (value2string v)))
evalPredef id [v1,v2]
  | id == cEqStr  = return (fmap toPBool (liftA2 (==) (value2string v1) (value2string v2)))
evalPredef id [v1,v2]
  | id == cOccur  = return (fmap toPBool (liftA2 occur (value2string v1) (value2string v2)))
evalPredef id [v1,v2]
  | id == cOccurs  = return (fmap toPBool (liftA2 occurs (value2string v1) (value2string v2)))
evalPredef id [v1,v2]
  | id == cEqInt  = return (fmap toPBool (liftA2 (==) (value2int v1) (value2int v2)))
evalPredef id [v1,v2]
  | id == cLessInt= return (fmap toPBool (liftA2 (<) (value2int v1) (value2int v2)))
evalPredef id [v1,v2]
  | id == cPlus   = return (fmap VInt (liftA2 (+) (value2int v1) (value2int v2)))
evalPredef id [v]
  | id == cError  = case value2string v of
                      Const msg -> fail msg
                      _         -> fail "Indescribable error appeared"
evalPredef id vs  = return RunTime

toPBool True  = VApp (cPredef,cPTrue)  []
toPBool False = VApp (cPredef,cPFalse) []

occur s1 []          = False
occur s1 s2@(_:tail) = check s1 s2
  where
    check xs        []  = False
    check []        ys  = True
    check (x:xs) (y:ys)
      | x == y          = check xs ys
    check _      _      = occur s1 tail

occurs cs s2 = any (\c -> elem c s2) cs

update lbl v []              = [(lbl,v)]
update lbl v (a@(lbl',_):as)
  | lbl==lbl'                = (lbl,v) : as
  | otherwise                = a : update lbl v as


patternMatch v0 []                      = return v0
patternMatch v0 ((env0,ps,args0,t):eqs) = match env0 ps eqs args0
  where
    match env []              eqs      args  = eval env t args
    match env (PT ty p   :ps) eqs      args  = match env (p:ps) eqs args
    match env (PAlt p1 p2:ps) eqs      args  = match env (p1:ps) ((env,p2:ps,args,t):eqs) args
    match env (PM q      :ps) eqs      args  = do t <- getResDef q
                                                  v <- eval [] t []
                                                  case v of
                                                    VPatt _ _ p -> match env (p:ps) eqs args
                                                    _ -> evalError $ hang "Expected pattern macro:" 4
                                                                          (pp t)
    match env (PV v      :ps) eqs (arg:args) = match ((v,arg):env) ps eqs args
    match env (PAs v p   :ps) eqs (arg:args) = match ((v,arg):env) (p:ps) eqs (arg:args)
    match env (PW        :ps) eqs (arg:args) = match env ps eqs args
    match env (PTilde _  :ps) eqs (arg:args) = match env ps eqs args
    match env (p         :ps) eqs (arg:args) = do
      v <- force arg
      match' env p ps eqs arg v args

    match' env p ps eqs arg v args = do
      case (p,v) of
        (p,       VMeta i envi   vs) -> susp i envi (\v -> apply v vs >>= \v -> match' env p ps eqs arg v args)
        (p,       VGen  i vs       ) -> return v0
        (p,       VSusp i envi k vs) -> susp i envi (\v -> k v >>= \v -> apply v vs >>= \v -> match' env p ps eqs arg v args)
        (PP q qs, VApp r tnks)
          | q == r        -> match env (qs++ps) eqs (tnks++args)
        (PR pas, VR as)   -> matchRec env (reverse pas) as ps eqs args
        (PString s1, VStr s2)
          | s1 == s2      -> match env ps eqs args
        (PString s1, VEmpty)
          | null s1       -> match env ps eqs args
        (PSeq min1 max1 p1 min2 max2 p2,v)
                          -> case value2string v of
                               Const s -> do let n  = length s
                                                 lo = min1 `max` (n-fromMaybe n max2)
                                                 hi = (n-min2) `min` fromMaybe n max1
                                                 (ds,cs) = splitAt lo s
                                             eqs <- matchStr env (p1:p2:ps) eqs (hi-lo) (reverse ds) cs args
                                             patternMatch v0 eqs
                               RunTime -> return v0
                               NonExist-> patternMatch v0 eqs
        (PRep minp maxp p, v)
                          -> case value2string v of
                               Const s -> do let n = length s `div` (max minp 1)
                                             eqs <- matchRep env n minp maxp p minp maxp p ps ((env,PString []:ps,(arg:args),t) : eqs) (arg:args)
                                             patternMatch v0 eqs
                               RunTime -> return v0
                               NonExist-> patternMatch v0 eqs
        (PChar, VStr [_]) -> match env ps eqs args
        (PChars cs, VStr [c])
          | elem c cs     -> match env ps eqs args
        (PInt n, VInt m)
          | n == m        -> match env ps eqs args
        (PFloat n, VFlt m)
          | n == m        -> match env ps eqs args
        _                 -> patternMatch v0 eqs

    matchRec env []            as ps eqs args = match env ps eqs args
    matchRec env ((lbl,p):pas) as ps eqs args =
      case lookup lbl as of
        Just tnk -> matchRec env pas as (p:ps) eqs (tnk:args)
        Nothing  -> evalError ("Missing value for label" <+> pp lbl)

    matchStr env ps eqs i ds []     args = do
      arg1 <- newEvaluatedThunk (string2value (reverse ds))
      arg2 <- newEvaluatedThunk (string2value [])
      return ((env,ps,arg1:arg2:args,t) : eqs)
    matchStr env ps eqs 0 ds cs     args = do
      arg1 <- newEvaluatedThunk (string2value (reverse ds))
      arg2 <- newEvaluatedThunk (string2value cs)
      return ((env,ps,arg1:arg2:args,t) : eqs)
    matchStr env ps eqs i ds (c:cs) args = do
      arg1 <- newEvaluatedThunk (string2value (reverse ds))
      arg2 <- newEvaluatedThunk (string2value (c:cs))
      eqs  <- matchStr env ps eqs (i-1 :: Int) (c:ds) cs args
      return ((env,ps,arg1:arg2:args,t) : eqs)

    matchRep env 0 minp maxp p minq maxq q ps eqs args = do
      return eqs
    matchRep env n minp maxp p minq maxq q ps eqs args = do
      matchRep env (n-1) minp maxp p (minp+minq) (liftM2 (+) maxp maxq) (PSeq minp maxp p minq maxq q) ps ((env,q:ps,args,t) : eqs) args


vtableSelect v0 ty tnks tnk2 vs = do
  v2 <- force tnk2
  (i,_) <- value2index v2 ty
  v <- force (tnks !! i)
  apply v vs
  where
    value2index (VR as) (RecType lbls) = compute lbls
      where
        compute []              = return (0,1)
        compute ((lbl,ty):lbls) = do
          case lookup lbl as of
            Just tnk -> do v <- force tnk
                           (r, cnt ) <- value2index v ty
                           (r',cnt') <- compute lbls
                           return (r*cnt'+r',cnt*cnt')
            Nothing  -> evalError ("Missing value for label" <+> pp lbl $$
                                   "among" <+> hsep (punctuate (pp ',') (map fst as)))
    value2index (VApp q tnks) ty = do
      (r ,ctxt,cnt ) <- getIdxCnt q
      (r',     cnt') <- compute ctxt tnks
      return (r+r',cnt)
      where
        getIdxCnt q = do
          (_,ResValue (L _ ty) idx) <- getInfo q
          let (ctxt,QC p) = typeFormCnc ty
          (_,ResParam _ (Just (_,cnt))) <- getInfo p
          return (idx,ctxt,cnt)

        compute []              []         = return (0,1)
        compute ((_,_,ty):ctxt) (tnk:tnks) = do
          v <- force tnk
          (r, cnt ) <- value2index v ty
          (r',cnt') <- compute ctxt tnks
          return (r*cnt'+r',cnt*cnt')
    value2index (VInt n)          ty
      | Just max <- isTypeInts ty    = return (fromIntegral n,fromIntegral max+1)
    value2index (VMeta i envi vs) ty = do
      v <- susp i envi (\v -> apply v vs)
      value2index v ty
    value2index (VSusp i envi k vs) ty = do
      v <- susp i envi (\v -> k v >>= \v -> apply v vs)
      value2index v ty
    value2index v ty = do t <- value2term [] v
                          evalError ("the parameter:" <+> ppTerm Unqualified 0 t $$
                                     "cannot be evaluated at compile time.")


susp i env ki = EvalM $ \gr k mt d r msgs -> do
  s <- readSTRef i
  case s of
    Narrowing id (QC q) -> case lookupOrigInfo gr q of
                             Ok (m,ResParam (Just (L _ ps)) _) -> bindParam gr k mt d r msgs s m ps
                             Bad msg -> return (Fail (pp msg) msgs)
    Narrowing id ty
      | Just max <- isTypeInts ty
                        -> bindInt gr k mt d r msgs s 0 max
    Evaluated _ v       -> case ki v of
                             EvalM f -> f gr k mt d r msgs
    _                   -> k (VSusp i env ki []) mt d r msgs
  where
    bindParam gr k mt d r msgs s m []             = return (Success r msgs)
    bindParam gr k mt d r msgs s m ((p, ctxt):ps) = do
      (mt',tnks) <- mkArgs mt ctxt
      let v = VApp (m,p) tnks
      writeSTRef i (Evaluated (length env) v)
      res <- case ki v of
               EvalM f -> f gr k mt' d r msgs
      writeSTRef i s
      case res of
        Fail msg  msgs -> return (Fail msg msgs)
        Success r msgs -> bindParam gr k mt d r msgs s m ps

    mkArgs mt []              = return (mt,[])
    mkArgs mt ((_,_,ty):ctxt) = do
      let i = case Map.maxViewWithKey mt of
                Just ((i,_),_) -> i+1
                _              -> 0
      tnk  <- newSTRef (Narrowing i ty)
      (mt,tnks) <- mkArgs (Map.insert i tnk mt) ctxt
      return (mt,tnk:tnks)

    bindInt gr k mt d r msgs s iv max
      | iv <= max = do
         let v = VInt iv
         writeSTRef i (Evaluated (length env) v)
         res <- case ki v of
                  EvalM f -> f gr k mt d r msgs
         writeSTRef i s
         case res of
           Fail msg  msgs -> return (Fail msg msgs)
           Success r msgs -> bindInt gr k mt d r msgs s (iv+1) max
      | otherwise = return (Success r msgs)


value2term xs (VApp q tnks) =
  foldM (\e1 tnk -> fmap (App e1) (tnk2term xs tnk)) (if fst q == cPredef then Q q else QC q) tnks
value2term xs (VMeta m env tnks) = do
  res <- zonk m tnks
  case res of
    Right i -> foldM (\e1 tnk -> fmap (App e1) (tnk2term xs tnk)) (Meta i) tnks
    Left  v -> value2term xs v
value2term xs (VSusp j env k vs) = do
  v <- k (VGen maxBound vs)
  value2term xs v
value2term xs (VGen j tnks) =
  foldM (\e1 tnk -> fmap (App e1) (tnk2term xs tnk)) (Vr (reverse xs !! j)) tnks
value2term xs (VClosure env (Abs b x t)) = do
  tnk <- newEvaluatedThunk (VGen (length xs) [])
  v <- eval ((x,tnk):env) t []
  let x' = mkFreshVar xs x
  t <- value2term (x':xs) v
  return (Abs b x' t)
value2term xs (VProd b x v1 (VClosure env t2))
  | x == identW = do t1 <- value2term xs v1
                     v2 <- eval env t2 []
                     t2 <- value2term xs v2
                     return (Prod b x t1 t2)
  | otherwise   = do t1 <- value2term xs v1
                     tnk <- newEvaluatedThunk (VGen (length xs) [])
                     v2 <- eval ((x,tnk):env) t2 []
                     t2 <- value2term (x:xs) v2
                     return (Prod b (mkFreshVar xs x) t1 t2)
value2term xs (VRecType lbls) = do
  lbls <- mapM (\(lbl,v) -> fmap ((,) lbl) (value2term xs v)) lbls
  return (RecType lbls)
value2term xs (VR as) = do
  as <- mapM (\(lbl,tnk) -> fmap (\t -> (lbl,(Nothing,t))) (tnk2term xs tnk)) as
  return (R as)
value2term xs (VP v lbl tnks) = do
  t <- value2term xs v
  foldM (\e1 tnk -> fmap (App e1) (tnk2term xs tnk)) (P t lbl) tnks
value2term xs (VExtR v1 v2) = do
  t1 <- value2term xs v1
  t2 <- value2term xs v2
  return (ExtR t1 t2) 
value2term xs (VTable v1 v2) = do
  t1 <- value2term xs v1
  t2 <- value2term xs v2
  return (Table t1 t2)
value2term xs (VT vty env cs)= do
  ty <- value2term xs vty
  cs <- forM cs $ \(p,t) -> do
          (_,xs',env') <- pattVars (length xs,xs,env) p
          v <- eval env' t []
          t <- value2term xs' v
          return (p,t)
  return (T (TTyped ty) cs)
value2term xs (VV vty tnks)= do ty <- value2term xs vty
                                ts <- mapM (tnk2term xs) tnks
                                return (V ty ts)
value2term xs (VS v1 tnk2 tnks) = do t1 <- value2term xs v1
                                     t2 <- tnk2term xs tnk2
                                     foldM (\e1 tnk -> fmap (App e1) (tnk2term xs tnk)) (S t1 t2) tnks
value2term xs (VSort s) = return (Sort s)
value2term xs (VStr tok) = return (K tok)
value2term xs (VInt n) = return (EInt n)
value2term xs (VFlt n) = return (EFloat n)
value2term xs VEmpty = return Empty
value2term xs (VC v1 v2) = do
  t1 <- value2term xs v1
  t2 <- value2term xs v2
  return (C t1 t2)
value2term xs (VGlue v1 v2) = do
  t1 <- value2term xs v1
  t2 <- value2term xs v2
  return (Glue t1 t2)
value2term xs (VPatt min max p) = return (EPatt min max p)
value2term xs (VPattType v) = do t <- value2term xs v
                                 return (EPattType t)
value2term xs (VAlts vd vas) = do
  d <- value2term xs vd
  as <- forM vas $ \(vt,vs) -> do
           t <- value2term xs vt
           s <- value2term xs vs
           return (t,s)
  return (Alts d as)
value2term xs (VStrs vs) = do
  ts <- mapM (value2term xs) vs
  return (Strs ts)

pattVars st (PP _ ps)    = foldM pattVars st ps
pattVars st (PV x)       = case st of
                             (i,xs,env) -> do tnk <- newEvaluatedThunk (VGen i [])
                                              return (i+1,x:xs,(x,tnk):env)
pattVars st (PR as)      = foldM (\st (_,p) -> pattVars st p) st as
pattVars st (PT ty p)    = pattVars st p
pattVars st (PAs x p)    = do st <- case st of
                                      (i,xs,env) -> do tnk <- newEvaluatedThunk (VGen i [])
                                                       return (i+1,x:xs,(x,tnk):env)
                              pattVars st p
pattVars st (PImplArg p) = pattVars st p
pattVars st (PSeq _ _ p1 _ _ p2) = do st <- pattVars st p1
                                      pattVars st p2
pattVars st _            = return st

data ConstValue a
  = Const a
  | RunTime
  | NonExist

instance Functor ConstValue where
  fmap f (Const c) = Const (f c)
  fmap f RunTime   = RunTime
  fmap f NonExist  = NonExist

instance Applicative ConstValue where
  pure = Const

  (Const f) <*> (Const x) = Const (f x)
  NonExist  <*> _         = NonExist
  _         <*> NonExist  = NonExist
  RunTime   <*>  _        = RunTime
  _         <*>  RunTime  = RunTime

#if MIN_VERSION_base(4,10,0)
  liftA2 f (Const a) (Const b) = Const (f a b)
  liftA2 f NonExist  _         = NonExist
  liftA2 f _         NonExist  = NonExist
  liftA2 f RunTime   _         = RunTime
  liftA2 f _         RunTime   = RunTime
#endif

value2string v = fmap (\(_,ws,_) -> unwords ws) (value2string' v False [] [])

value2string' (VStr w1)   True (w2:ws) qs = Const (False,(w1++w2):ws,qs)
value2string' (VStr w)    _    ws      qs = Const (False,w       :ws,qs)
value2string' VEmpty      b    ws      qs = Const (b,ws,qs)
value2string' (VC v1 v2)  b    ws      qs =
  case value2string' v2 b ws qs of
    Const (b,ws,qs) -> value2string' v1 b ws qs
    res             -> res
value2string' (VApp q []) b    ws      qs
  | q == (cPredef,cNonExist)              = NonExist
value2string' (VApp q []) b    ws      qs
  | q == (cPredef,cSOFT_SPACE)            = if null ws
                                              then Const (b,ws,q:qs)
                                              else Const (b,ws,qs)
value2string' (VApp q []) b     ws     qs
  | q == (cPredef,cBIND) || q == (cPredef,cSOFT_BIND) 
                                          = if null ws
                                              then Const (True,ws,q:qs)
                                              else Const (True,ws,qs)
value2string' (VApp q []) b     ws     qs
  | q == (cPredef,cCAPIT) = capit ws
  where
    capit []            = Const (b,[],q:qs)
    capit ((c:cs) : ws) = Const (b,(toUpper c : cs) : ws,qs)
    capit ws            = Const (b,ws,qs)
value2string' (VApp q []) b     ws     qs
  | q == (cPredef,cALL_CAPIT) = all_capit ws
  where
    all_capit []       = Const (b,[],q:qs)
    all_capit (w : ws) = Const (b,map toUpper w : ws,qs)
value2string' (VAlts vd vas) b  ws     qs =
  case ws of
    []    -> value2string' vd b ws qs
    (w:_) -> pre vd vas w b ws qs
  where
    pre vd []                 w            = value2string' vd
    pre vd ((v,VStrs ss):vas) w
      | or [startsWith s w | VStr s <- ss] = value2string' v
      | otherwise                          = pre vd vas w
value2string' _ _ _ _ = RunTime

startsWith []          _ = True
startsWith (x:xs) (y:ys)
  | x == y               = startsWith xs ys
startsWith      _      _ = False


string2value s = string2value' (words s)

string2value' []     = VEmpty
string2value' [w]    = VStr w
string2value' (w:ws) = VC (VStr w) (string2value' ws)

value2int (VInt n) = Const n
value2int _        = RunTime

-----------------------------------------------------------------------
-- * Evaluation monad

type MetaThunks s = Map.Map MetaId (Thunk s)
type Cont s r = MetaThunks s -> Int -> r -> [Message] -> ST s (CheckResult r [Message])
newtype EvalM s a = EvalM (forall r . Grammar -> (a -> Cont s r) -> Cont s r)

instance Functor (EvalM s) where
  fmap f (EvalM g) = EvalM (\gr k -> g gr (k . f))

instance Applicative (EvalM s) where
  pure x = EvalM (\gr k -> k x)
  (EvalM f) <*> (EvalM x) = EvalM (\gr k -> f gr (\f -> x gr (\x -> k (f x))))

instance Monad (EvalM s) where
  (EvalM f) >>= g = EvalM (\gr k -> f gr (\x -> case g x of
                                                  EvalM g -> g gr k))
#if !(MIN_VERSION_base(4,13,0))
  -- Monad(fail) will be removed in GHC 8.8+
  fail = Fail.fail
#endif

instance Fail.MonadFail (EvalM s) where
  fail msg = EvalM (\gr k _ _ r msgs -> return (Fail (pp msg) msgs))

instance Alternative (EvalM s) where
  empty = EvalM (\gr k _ _ r msgs -> return (Success r msgs))
  (EvalM f) <|> (EvalM g) = EvalM $ \gr k mt b r msgs -> do
     res <- f gr k mt b r msgs
     case res of
       Fail msg  msgs -> return (Fail msg msgs)
       Success r msgs -> g gr k mt b r msgs

instance MonadPlus (EvalM s) where

runEvalM :: Grammar -> (forall s . EvalM s a) -> Check [a]
runEvalM gr f = Check $ \(es,ws) ->
  case runST (case f of
                EvalM f -> f gr (\x mt _ xs ws -> return (Success (x:xs) ws)) Map.empty maxBound [] ws) of
    Fail msg   ws -> Fail msg (es,ws)
    Success xs ws -> Success (reverse xs) (es,ws)

runEvalOneM :: Grammar -> (forall s . EvalM s a) -> Check a
runEvalOneM gr f = Check $ \(es,ws) ->
  case runST (case f of
                EvalM f -> f gr (\x mt _ xs ws -> return (Success (x:xs) ws)) Map.empty maxBound [] ws) of
    Fail msg      ws -> Fail msg (es,ws)
    Success []    ws -> Fail (pp "The evaluation produced no results") (es,ws)
    Success (x:_) ws -> Success x (es,ws)

evalError :: Message -> EvalM s a
evalError msg = EvalM (\gr k _ _ r msgs -> return (Fail msg msgs))
  
evalWarn :: Message -> EvalM s ()
evalWarn msg = EvalM (\gr k mt d r msgs -> k () mt d r (msg:msgs))

getResDef :: QIdent -> EvalM s Term
getResDef q = EvalM $ \gr k mt d r msgs -> do
  case lookupResDef gr q of
    Ok t    -> k t mt d r msgs
    Bad msg -> return (Fail (pp msg) msgs)

getInfo :: QIdent -> EvalM s (ModuleName,Info)
getInfo q = EvalM $ \gr k mt d r msgs -> do
  case lookupOrigInfo gr q of
    Ok res  -> k res mt d r msgs
    Bad msg -> return (Fail (pp msg) msgs)

getResType :: QIdent -> EvalM s Type
getResType q = EvalM $ \gr k mt d r msgs -> do
  case lookupResType gr q of
    Ok t    -> k t mt d r msgs
    Bad msg -> return (Fail (pp msg) msgs)

getAllParamValues :: Type -> EvalM s [Term]
getAllParamValues ty = EvalM $ \gr k mt d r msgs ->
  case allParamValues gr ty of
    Ok ts   -> k ts mt d r msgs
    Bad msg -> return (Fail (pp msg) msgs)

newThunk env t = EvalM $ \gr k mt d r msgs -> do
 tnk <- newSTRef (Unevaluated env t)
 k tnk mt d r msgs

newEvaluatedThunk v = EvalM $ \gr k mt d r msgs -> do
 tnk <- newSTRef (Evaluated maxBound v)
 k tnk mt d r msgs

newHole i = EvalM $ \gr k mt d r msgs ->
  if i == 0
    then do tnk <- newSTRef (Hole i)
            k tnk mt d r msgs
    else case Map.lookup i mt of
           Just tnk -> k tnk mt d r msgs
           Nothing  -> do tnk <- newSTRef (Hole i)
                          k tnk (Map.insert i tnk mt) d r msgs

newResiduation scope ty = EvalM $ \gr k mt d r msgs -> do
  tnk <- newSTRef (Residuation 0 scope ty)
  k tnk mt d r msgs

newNarrowing i ty = EvalM $ \gr k mt d r msgs ->
  if i == 0
    then do tnk <- newSTRef (Narrowing i ty)
            k tnk mt d r msgs
    else case Map.lookup i mt of
           Just tnk -> k tnk mt d r msgs
           Nothing  -> do tnk <- newSTRef (Narrowing i ty)
                          k tnk (Map.insert i tnk mt) d r msgs

withVar d0 (EvalM f) = EvalM $ \gr k mt d1 r msgs ->
                                  let !d = min d0 d1
                                  in f gr k mt d r msgs

getVariables :: EvalM s [(LVar,LIndex)]
getVariables = EvalM $ \gr k mt d ws r -> do
  ps <- metas2params gr (Map.elems mt)
  k ps mt d ws r
  where
    metas2params gr []         = return []
    metas2params gr (tnk:tnks) = do
      st <- readSTRef tnk
      case st of
        Narrowing i ty -> do let cnt = case allParamValues gr ty of
                                         Ok ts   -> length ts
                                         Bad msg -> error msg
                             params <- metas2params gr tnks
                             if cnt > 1
                               then return ((i-1,cnt):params)
                               else return params
        _              -> metas2params gr tnks

getRef tnk = EvalM $ \gr k mt d ws r -> readSTRef tnk >>= \st -> k st mt d ws r
setRef tnk st = EvalM $ \gr k mt d ws r -> writeSTRef tnk st >>= \st -> k () mt d ws r

force tnk = EvalM $ \gr k mt d r msgs -> do
  s <- readSTRef tnk
  case s of
    Unevaluated env t -> case eval env t [] of
                           EvalM f -> f gr (\v mt b r msgs -> do let d = length env
                                                                 writeSTRef tnk (Evaluated d v)
                                                                 r <- k v mt d r msgs
                                                                 writeSTRef tnk s
                                                                 return r) mt d r msgs
    Evaluated d v     -> k v mt d r msgs
    Hole _            -> k (VMeta tnk [] []) mt d r msgs
    Residuation _ _ _ -> k (VMeta tnk [] []) mt d r msgs
    Narrowing _ _     -> k (VMeta tnk [] []) mt d r msgs

tnk2term xs tnk = EvalM $ \gr k mt d r msgs ->
  let join f g = do res <- f
                    case res of
                      Fail msg  msgs -> return (Fail msg msgs)
                      Success r msgs -> g r msgs

      flush []  k1 mt r msgs = k1 mt r msgs
      flush [x] k1 mt r msgs = join (k x mt d r msgs) (k1 mt)
      flush xs  k1 mt r msgs = join (k (FV (reverse xs)) mt d r msgs) (k1 mt)

      acc d0 x mt d (r,!c,xs) msgs
        | d < d0    = flush xs (\mt r msgs -> join (k x mt d r msgs) (\r msgs -> return (Success (r,c+1,[]) msgs))) mt r msgs
        | otherwise = return (Success (r,c+1,x:xs) msgs)

  in do s <- readSTRef tnk
        case s of
          Unevaluated env t -> do let d0 = length env
                                  res <- case eval env t [] of
                                           EvalM f -> f gr (\v mt d msgs r -> do writeSTRef tnk (Evaluated d0 v)
                                                                                 r <- case value2term xs v of
                                                                                        EvalM f -> f gr (acc d0) mt d msgs r
                                                                                 writeSTRef tnk s
                                                                                 return r) mt maxBound (r,0,[]) msgs
                                  case res of
                                    Fail msg msgs         -> return (Fail msg msgs)
                                    Success (r,0,xs) msgs -> k (FV []) mt d r msgs
                                    Success (r,c,xs) msgs -> flush xs (\mt msgs r -> return (Success msgs r)) mt r msgs
          Evaluated d0 v    -> do res <- case value2term xs v of
                                           EvalM f -> f gr (acc d0) mt maxBound (r,0,[]) msgs
                                  case res of
                                    Fail msg msgs         -> return (Fail msg msgs)
                                    Success (r,0,xs) msgs -> k (FV []) mt d r msgs
                                    Success (r,c,xs) msgs -> flush xs (\mt r msgs -> return (Success r msgs)) mt r msgs
          Hole i            -> k (Meta i) mt d r msgs
          Residuation i _ _ -> k (Meta i) mt d r msgs
          Narrowing i _     -> k (Meta i) mt d r msgs

zonk tnk vs = EvalM $ \gr k mt d r msgs -> do
  s <- readSTRef tnk
  case s of
    Evaluated _ v     -> case apply v vs of
                           EvalM f -> f gr (k . Left) mt d r msgs
    Hole i            -> k (Right i) mt d r msgs
    Residuation i _ _ -> k (Right i) mt d r msgs
    Narrowing i _     -> k (Right i) mt d r msgs
