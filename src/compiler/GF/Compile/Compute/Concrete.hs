{-# LANGUAGE RankNTypes, CPP #-}

-- | Functions for computing the values of terms in the concrete syntax, in
-- | preparation for PMCFG generation.
module GF.Compile.Compute.Concrete
           ( normalForm
           , Value(..), Thunk, ThunkState(..), Env, showValue
           , EvalM, runEvalM, evalError
           , eval, apply, force, value2term, patternMatch
           , newThunk, newEvaluatedThunk
           , newResiduation, newNarrowing, getVariables
           , getRef
           , getResDef, getInfo, getAllParamValues
           ) where

import Prelude hiding ((<>)) -- GHC 8.4.1 clash with Text.PrettyPrint

import GF.Grammar hiding (Env, VGen, VApp, VRecType)
import GF.Grammar.Lookup(lookupResDef,lookupOrigInfo,allParamValues)
import GF.Grammar.Predef
import GF.Grammar.Lockfield(lockLabel)
import GF.Grammar.Printer
import GF.Data.Str(Str,glueStr,str2strings,str,sstr,plusStr,strTok)
import GF.Data.Operations(Err(..),err,errIn,maybeErr,mapPairsM)
import GF.Data.Utilities(mapFst,mapSnd)
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
  fmap mkFV (runEvalM gr (eval [] t [] >>= value2term 0))
  where
    mkFV [t] = t
    mkFV ts  = FV ts


data ThunkState s
  = Unevaluated (Env s) Term
  | Evaluated (Value s)
  | Residuation {-# UNPACK #-} !MetaId
  | Narrowing   {-# UNPACK #-} !MetaId Type

type Thunk s = STRef s (ThunkState s)
type Env s = [(Ident,Thunk s)]

data Value s
  = VApp QIdent [Thunk s]
  | VMeta (Thunk s) (Env s) [Thunk s]
  | VSusp (Thunk s) (Env s) (Value s -> EvalM s (Value s)) [Thunk s]
  | VGen  {-# UNPACK #-} !Int [Thunk s]
  | VClosure (Env s) Term
  | VProd BindType Ident (Value s) (Env s) Term
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
  | VC [Value s]
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
showValue (VProd _ _ _ _ _) = "VProd"
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
showValue (VStr _) = "VStr"
showValue (VC _) = "VC"
showValue (VGlue _ _) = "VGlue"
showValue (VPatt _ _ _) = "VPatt"
showValue (VPattType _) = "VPattType"
showValue (VAlts _ _) = "VAlts"
showValue (VStrs _) = "VStrs"
showValue (VSymCat _ _ _) = "VSymCat"

eval env (Vr x)         vs  = case lookup x env of
                                Just tnk -> do v <- force tnk
                                               apply v vs
                                Nothing  -> evalError ("Variable" <+> pp x <+> "is not in scope")
eval env (Sort s)       []  = return (VSort s)
eval env (EInt n)       []  = return (VInt n)
eval env (EFloat d)     []  = return (VFlt d)
eval env (K t)          []
  | null t                  = return (VC [])
  | otherwise               = return (VStr t)
eval env Empty          []  = return (VC [])
eval env (App t1 t2)    vs  = do tnk <- newThunk env t2
                                 eval env t1 (tnk : vs)
eval env (Abs b x t)    []  = return (VClosure env (Abs b x t))
eval env (Abs b x t) (v:vs) = eval ((x,v):env) t vs
eval env (Meta i)       vs  = do tnk <- newResiduation i
                                 return (VMeta tnk env vs)
eval env (ImplArg t)    []  = eval env t []
eval env (Prod b x t1 t2)[] = do v1 <- eval env t1 []
                                 return (VProd b x v1 env t2)
eval env (Typed t ty)   vs  = eval env t vs
eval env (RecType lbls) []  = do lbls <- mapM (\(lbl,ty) -> fmap ((,) lbl) (eval env ty [])) lbls
                                 return (VRecType lbls)
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
eval env t@(S t1 t2)    vs  = do v1   <- eval env t1 []
                                 tnk2 <- newThunk env t2
                                 let v0 = VS v1 tnk2 vs
                                 case v1 of
                                   VT _  env cs -> patternMatch v0 (map (\(p,t) -> (env,[p],tnk2:vs,t)) cs)
                                   VV vty tnks  -> do t2 <- force tnk2 >>= value2term (length env)
                                                      ty <- value2term (length env) vty
                                                      ts <- getAllParamValues ty
                                                      case lookup t2 (zip ts tnks) of
                                                        Just tnk -> do v <- force tnk
                                                                       apply v vs
                                                        Nothing  -> return v0
                                   v1      -> return v0
eval env (Let (x,(_,t1)) t2) vs = do tnk <- newThunk env t1
                                     eval ((x,tnk):env) t2 vs
eval env (Q q@(m,id))   vs
  | m == cPredef            = do vs' <- mapM force vs
                                 mb_res <- evalPredef id vs'
                                 case mb_res of
                                   Const res -> return res
                                   RunTime   -> return (VApp q vs)
                                   NonExist  -> return (VApp (cPredef,cNonExist) vs)
  | otherwise               = do t <- getResDef q
                                 eval env t vs
eval env (QC q)         vs  = return (VApp q vs)
eval env (C t1 t2)      []  = do v1 <- eval env t1 []
                                 v2 <- eval env t2 []
                                 case (v1,v2) of
                                   (VC vs1,VC vs2) -> return (VC (vs1++vs2))
                                   (VC vs1,v2    ) -> return (VC (vs1++[v2]))
                                   (v1,    VC vs2) -> return (VC ([v1]++vs2))
                                   (v1,    v2    ) -> return (VC [v1,v2])
eval env t@(Glue t1 t2) []  = do v1 <- eval env t1 []
                                 v2 <- eval env t2 []
                                 case liftA2 (++) (value2string v1) (value2string v2) of
                                   Const s  -> return (string2value s)
                                   RunTime  -> return (VGlue v1 v2)
                                   NonExist -> return (VApp (cPredef,cNonExist) [])
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


patternMatch v0 []                      = fail "No matching pattern found"
patternMatch v0 ((env0,ps,args0,t):eqs) = match env0 ps eqs args0
  where
    match env []              eqs      args  = eval env t args
    match env (PT ty p   :ps) eqs      args  = match env (p:ps) eqs args
    match env (PAlt p1 p2:ps) eqs      args  = match env (p1:ps) ((env,p2:ps,args,t):eqs) args
    match env (PM q      :ps) eqs      args  = do t <- getResDef q
                                                  case t of
                                                    EPatt _ _ p -> match env (p:ps) eqs args
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
        (p,       VMeta i envi   vs) -> susp i envi (\v -> apply v vs >>= \v -> match' env p ps eqs arg v args) []
        (p,       VGen  i vs       ) -> return v0
        (p,       VSusp i envi k vs) -> susp i envi (\v -> k v >>= \v -> apply v vs >>= \v -> match' env p ps eqs arg v args) []
        (PP q qs, VApp r tnks)
          | q == r        -> match env (qs++ps) eqs (tnks++args)
        (PR pas, VR as)   -> matchRec env pas as ps eqs args
        (PString s1, VStr s2)
          | s1 == s2      -> match env ps eqs args
        (PString s1, VC [])
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

    susp i env ki vs = EvalM $ \gr k mt r -> do
      s <- readSTRef i
      case s of
        Narrowing id (QC q) -> case lookupOrigInfo gr q of
                                 Ok (m,ResParam (Just (L _ ps)) _) -> bindParam gr k mt r s m ps
                                 Bad msg -> return (Fail (pp msg))
        Narrowing id ty
          | Just max <- isTypeInts ty
                            -> bindInt gr k mt r s 0 max
        Evaluated v         -> case ki v of
                                 EvalM f -> f gr k mt r
        _                   -> k (VSusp i env ki vs) mt r
      where
        bindParam gr k mt r s m []             = return (Success r)
        bindParam gr k mt r s m ((p, ctxt):ps) = do
          (mt',tnks) <- mkArgs mt ctxt
          let v = VApp (m,p) tnks
          writeSTRef i (Evaluated v)
          res <- case ki v of
                   EvalM f -> f gr k mt' r
          writeSTRef i s
          case res of
            Fail msg  -> return (Fail msg)
            Success r -> bindParam gr k mt r s m ps

        mkArgs mt []              = return (mt,[])
        mkArgs mt ((_,_,ty):ctxt) = do
          let i = case Map.maxViewWithKey mt of
                    Just ((i,_),_) -> i+1
                    _              -> 0
          tnk  <- newSTRef (Narrowing i ty)
          (mt,tnks) <- mkArgs (Map.insert i tnk mt) ctxt
          return (mt,tnk:tnks)

        bindInt gr k mt r s iv max
          | iv < max = do
             let v = VInt iv
             writeSTRef i (Evaluated v)
             res <- case ki v of
                      EvalM f -> f gr k mt r
             writeSTRef i s
             case res of
               Fail msg  -> return (Fail msg)
               Success r -> bindInt gr k mt r s (iv+1) max
          | otherwise = return (Success r)

value2term i (VApp q tnks) =
  foldM (\e1 tnk -> fmap (App e1) (force tnk >>= value2term i)) (if fst q == cPredef then Q q else QC q) tnks
value2term i (VMeta m env tnks) = do
  res <- zonk m tnks
  case res of
    Right i -> foldM (\e1 tnk -> fmap (App e1) (force tnk >>= value2term i)) (Meta i) tnks
    Left  v -> value2term i v
value2term i (VSusp j env k vs) = do
  v <- k (VGen maxBound vs)
  value2term i v
value2term i (VGen j tnks) =
  foldM (\e1 tnk -> fmap (App e1) (force tnk >>= value2term i)) (Vr (identS ('v':show j))) tnks
value2term i (VClosure env (Abs b x t)) = do
  tnk <- newEvaluatedThunk (VGen i [])
  v <- eval ((x,tnk):env) t []
  t <- value2term (i+1) v
  return (Abs b (identS ('v':show i)) t)
value2term i (VProd b x v1 env t2)
  | x == identW = do t1 <- value2term i v1
                     v2 <- eval env t2 []
                     t2 <- value2term i v2
                     return (Prod b x t1 t2)
  | otherwise   = do t1 <- value2term i v1
                     tnk <- newEvaluatedThunk (VGen i [])
                     v2 <- eval ((x,tnk):env) t2 []
                     t2 <- value2term (i+1) v2
                     return (Prod b (identS ('v':show i)) t1 t2)
value2term i (VRecType lbls) = do
  lbls <- mapM (\(lbl,v) -> fmap ((,) lbl) (value2term i v)) lbls
  return (RecType lbls)
value2term i (VR as) = do
  as <- mapM (\(lbl,tnk) -> fmap (\t -> (lbl,(Nothing,t))) (force tnk >>= value2term i)) as
  return (R as)
value2term i (VP v lbl tnks) = do
  t <- value2term i v
  foldM (\e1 tnk -> fmap (App e1) (force tnk >>= value2term i)) (P t lbl) tnks
value2term i (VExtR v1 v2) = do
  t1 <- value2term i v1
  t2 <- value2term i v2
  return (ExtR t1 t2) 
value2term i (VTable v1 v2) = do
  t1 <- value2term i v1
  t2 <- value2term i v2
  return (Table t1 t2)
value2term i (VT vty _ cs)= do ty <- value2term i vty
                               return (T (TTyped ty) cs)
value2term i (VV vty tnks)= do ty <- value2term i vty
                               ts <- mapM (\tnk -> force tnk >>= value2term i) tnks
                               return (V ty ts)
value2term i (VS v1 tnk2 tnks) = do t1 <- value2term i v1
                                    t2 <- force tnk2 >>= value2term i
                                    foldM (\e1 tnk -> fmap (App e1) (force tnk >>= value2term i)) (S t1 t2) tnks
value2term i (VSort s) = return (Sort s)
value2term i (VStr tok) = return (K tok)
value2term i (VInt n) = return (EInt n)
value2term i (VFlt n) = return (EFloat n)
value2term i (VC vs) = do
  ts <- mapM (value2term i) vs
  case ts of
    []     -> return Empty
    (t:ts) -> return (foldl C t ts)
value2term i (VGlue v1 v2) = do
  t1 <- value2term i v1
  t2 <- value2term i v2
  return (Glue t1 t2)
value2term i (VPatt min max p) = return (EPatt min max p)
value2term i (VPattType v) = do t <- value2term i v
                                return (EPattType t)
value2term i (VAlts vd vas) = do
  d <- value2term i vd
  as <- forM vas $ \(vt,vs) -> do
           t <- value2term i vt
           s <- value2term i vs
           return (t,s)
  return (Alts d as)
value2term i (VStrs vs) = do
  ts <- mapM (value2term i) vs
  return (Strs ts)

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

  liftA2 f (Const a) (Const b) = Const (f a b)
  liftA2 f NonExist  _         = NonExist
  liftA2 f _         NonExist  = NonExist
  liftA2 f RunTime   _         = RunTime
  liftA2 f _         RunTime   = RunTime

value2string =
  fmap (\(_,_,ws) -> unwords (reverse ws)) .
  value2string (Const (False,id,[]))
  where
    value2string (Const (True,f,(w0:ws))) (VStr w) = Const (False,id,(w0++f w):ws)
    value2string (Const (_,   f,    ws )) (VStr w) = Const (False,id,(    f w):ws)
    value2string st               (VC vs)  = foldl value2string st vs
    value2string st (VApp q [])
      | q == (cPredef,cNonExist) = NonExist
    value2string st (VApp q [])
      | q == (cPredef,cSOFT_SPACE) = st
    value2string (Const (b,f,ws)) (VApp q [])
      | q == (cPredef,cBIND) || q == (cPredef,cSOFT_BIND) = Const (True,f,ws)
    value2string (Const (b,f,ws)) (VApp q [])
      | q == (cPredef,cCAPIT) = Const (b,f . capit,ws)
      where
        capit []     = []
        capit (c:cs) = toUpper c : cs
    value2string (Const (b,f,ws)) (VApp q [])
      | q == (cPredef,cALL_CAPIT) = Const (b,f . all_capit,ws)
      where
        all_capit = map toUpper
--  value2string (b,f,ws) (VAlts vd vas) =
    value2string (Const _) _  = RunTime
    value2string st        _  = st

string2value s =
  case words s of
    []  -> VC []
    [w] -> VStr w
    ws  -> VC (map VStr ws)

value2int (VInt n) = Const n
value2int _        = RunTime

-----------------------------------------------------------------------
-- * Evaluation monad

type MetaThunks s = Map.Map MetaId (Thunk s)
type Cont s r = MetaThunks s -> r -> ST s (CheckResult r)
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
  fail msg = EvalM (\gr k _ r -> return (Fail (pp msg)))

instance Alternative (EvalM s) where
  empty = EvalM (\gr k _ r -> return (Success r))
  (EvalM f) <|> (EvalM g) = EvalM $ \gr k mt r -> do
     res <- f gr k mt r
     case res of
       Fail msg  -> return (Fail msg)
       Success r -> g gr k mt r

instance MonadPlus (EvalM s) where

runEvalM :: Grammar -> (forall s . EvalM s a) -> Check [a]
runEvalM gr f =
  case runST (case f of
                EvalM f -> f gr (\x mt xs -> return (Success (x:xs))) Map.empty []) of
    Fail msg   -> checkError msg
    Success xs -> return (reverse xs)

evalError :: Doc -> EvalM s a
evalError msg = EvalM (\gr k _ r -> return (Fail msg))
  
getResDef :: QIdent -> EvalM s Term
getResDef q = EvalM $ \gr k mt r -> do
  case lookupResDef gr q of
    Ok t    -> k t mt r
    Bad msg -> return (Fail (pp msg))

getInfo :: QIdent -> EvalM s (ModuleName,Info)
getInfo q = EvalM $ \gr k mt r -> do
  case lookupOrigInfo gr q of
    Ok res  -> k res mt r
    Bad msg -> return (Fail (pp msg))

getAllParamValues :: Type -> EvalM s [Term]
getAllParamValues ty = EvalM $ \gr k mt r ->
  case allParamValues gr ty of
    Ok ts   -> k ts mt r
    Bad msg -> return (Fail (pp msg))

newThunk env t = EvalM $ \gr k mt r -> do
 tnk <- newSTRef (Unevaluated env t)
 k tnk mt r

newEvaluatedThunk v = EvalM $ \gr k mt r -> do
 tnk <- newSTRef (Evaluated v)
 k tnk mt r

newResiduation i = EvalM $ \gr k mt r ->
  if i == 0
    then do tnk <- newSTRef (Residuation i)
            k tnk mt r
    else case Map.lookup i mt of
           Just tnk -> k tnk mt r
           Nothing  -> do tnk <- newSTRef (Residuation i)
                          k tnk (Map.insert i tnk mt) r

newNarrowing i ty = EvalM $ \gr k mt r ->
  if i == 0
    then do tnk <- newSTRef (Narrowing i ty)
            k tnk mt r
    else case Map.lookup i mt of
           Just tnk -> k tnk mt r
           Nothing  -> do tnk <- newSTRef (Narrowing i ty)
                          k tnk (Map.insert i tnk mt) r

getVariables :: EvalM s [(LVar,LIndex)]
getVariables = EvalM $ \gr k mt r -> do
  ps <- metas2params gr (Map.elems mt)
  k ps mt r
  where
    metas2params gr []         = return []
    metas2params gr (tnk:tnks) = do
      st <- readSTRef tnk
      case st of
        Narrowing i ty -> do let range = case allParamValues gr ty of
                                           Ok ts   -> length ts
                                           Bad msg -> error msg
                             params <- metas2params gr tnks
                             return ((i-1,range):params)
        _              -> metas2params gr tnks

getRef tnk = EvalM $ \gr k mt r -> readSTRef tnk >>= \st -> k st mt r

force tnk = EvalM $ \gr k mt r -> do 
  s <- readSTRef tnk
  case s of
    Unevaluated env t -> case eval env t [] of
                           EvalM f -> f gr (\v mt r -> do writeSTRef tnk (Evaluated v)
                                                          r <- k v mt r
                                                          writeSTRef tnk s
                                                          return r) mt r
    Evaluated v       -> k v mt r
    Residuation _     -> k (VMeta tnk [] []) mt r
    Narrowing _ _     -> k (VMeta tnk [] []) mt r

zonk tnk vs = EvalM $ \gr k mt r -> do
  s <- readSTRef tnk
  case s of
    Evaluated v   -> case apply v vs of
                       EvalM f -> f gr (k . Left) mt r
    Residuation i -> k (Right i) mt r
    Narrowing i _ -> k (Right i) mt r
