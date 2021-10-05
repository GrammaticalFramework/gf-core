{-# LANGUAGE RankNTypes, CPP #-}

-- | Functions for computing the values of terms in the concrete syntax, in
-- | preparation for PMCFG generation.
module GF.Compile.Compute.Concrete
           (normalForm,
            Value(..), Env, value2term, eval
           ) where
import Prelude hiding ((<>)) -- GHC 8.4.1 clash with Text.PrettyPrint

import GF.Grammar hiding (Env, VGen, VApp, VRecType)
import GF.Grammar.Lookup(lookupResDef,allParamValues)
import GF.Grammar.Predef
import GF.Grammar.Lockfield(isLockLabel,lockRecType) --unlockRecord,lockLabel
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
import Control.Applicative
import qualified Control.Monad.Fail as Fail
import qualified Data.Map as Map
import GF.Text.Pretty

-- * Main entry points

normalForm :: Grammar -> L Ident -> Term -> Check Term
normalForm gr loc t =
  fmap mkFV (runEvalM gr (eval [] t [] >>= value2term 0))
  where
    mkFV [t] = t
    mkFV ts  = FV ts


data ThunkState s
  = Unevaluated (Env s) Term
  | Evaluated (Value s)
  | Unbound {-# UNPACK #-} !MetaId

type Thunk s = STRef s (ThunkState s)
type Env s = [(Ident,Thunk s)]

data Value s
  = VApp QIdent [Thunk s]
  | VMeta (Thunk s) (Env s) [Thunk s]
  | VSusp (Thunk s) (Env s) [Thunk s] (Thunk s -> EvalM s (Value s))
  | VGen  {-# UNPACK #-} !Int [Thunk s]
  | VClosure (Env s) Term
  | VProd BindType Ident (Value s) (Env s) Term
  | VRecType [(Label, Value s)]
  | VR [(Label, Thunk s)]
  | VP (Value s) Label [Thunk s]
  | VTable (Value s) (Value s)
  | VT TInfo [Case]
  | VV Type [Thunk s]
  | VS (Value s) (Thunk s) [Thunk s]
  | VSort Ident
  | VInt Integer
  | VFlt Double
  | VStr String
  | VC [Value s]
  | VPatt Int (Maybe Int) Patt
  | VPattType (Value s)


eval env (Vr x)         vs  = case lookup x env of
                                Just tnk -> force tnk vs
                                Nothing  -> evalError ("Variable" <+> pp x <+> "is not in scope")
eval env (Sort s)       []  = return (VSort s)
eval env (EInt n)       []  = return (VInt n)
eval env (EFloat d)     []  = return (VFlt d)
eval env (K t)          []  = return (VStr t)
eval env Empty          []  = return (VC [])
eval env (App t1 t2)    vs  = do tnk <- newThunk env t2
                                 eval env t1 (tnk : vs)
eval env (Abs b x t)    []  = return (VClosure env (Abs b x t))
eval env (Abs b x t) (v:vs) = eval ((x,v):env) t vs
eval env (Meta i)       vs  = do tnk <- newMeta i
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
                                                                     "in record" <+> pp t)
                                              Just tnk -> force tnk vs
                                   v     -> return (VP v lbl vs)
eval env (Table t1 t2)  []  = do v1 <- eval env t1 []
                                 v2 <- eval env t2 []
                                 return (VTable v1 v2)
eval env (T i cs)       []  = return (VT i cs)
eval env (V ty ts)      []  = do tnks <- mapM (newThunk env) ts
                                 return (VV ty tnks)
eval env t@(S t1 t2)    vs  = do v1   <- eval env t1 []
                                 tnk2 <- newThunk env t2
                                 let v0 = VS v1 tnk2 vs
                                 case v1 of
                                   VT _ cs -> patternMatch v0 (map (\(p,t) -> (env,[p],tnk2:vs,t)) cs)
                                   v1      -> return v0
eval env (Let (x,(_,t1)) t2) vs = do tnk <- newThunk env t1
                                     eval ((x,tnk):env) t2 vs
eval env (Q q@(m,id))   vs
  | m == cPredef            = do vs' <- mapM (flip force []) vs
                                 mb_res <- evalPredef id vs'
                                 case mb_res of
                                   Just res -> return res
                                   Nothing  -> return (VApp q vs)
  | otherwise               = do t <- lookupGlobal q
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
                                 case liftM2 (++) (value2string v1) (value2string v2) of
                                   Just s  -> return (string2value s)
                                   Nothing -> evalError ("Cannot reduce term" <+> pp t)
eval env (EPatt min max p) [] = return (VPatt min max p)
eval env (EPattType t)  []  = do v <- eval env t []
                                 return (VPattType v)
eval env (FV ts)        vs  = msum [eval env t vs | t <- ts]
eval env t              vs  = evalError ("Cannot reduce term" <+> pp t)

apply v                             []  = return v
apply (VApp f  vs0)                 vs  = return (VApp f (vs0++vs))
apply (VMeta m env vs0)             vs  = return (VMeta m env (vs0++vs))
apply (VGen i  vs0)                 vs  = return (VGen i (vs0++vs))
apply (VClosure env (Abs b x t)) (v:vs) = eval ((x,v):env) t vs

evalPredef id [v]
  | id == cLength = return (fmap VInt (liftM genericLength (value2string v)))
evalPredef id [v1,v2]
  | id == cTake   = return (fmap string2value (liftM2 genericTake (value2int v1) (value2string v2)))
evalPredef id [v1,v2]
  | id == cDrop   = return (fmap string2value (liftM2 genericDrop (value2int v1) (value2string v2)))
evalPredef id [v1,v2]
  | id == cTk     = return (fmap string2value (liftM2 genericTk (value2int v1) (value2string v2)))
  where
    genericTk n = reverse . genericTake n . reverse
evalPredef id [v1,v2]
  | id == cDp     = return (fmap string2value (liftM2 genericDp (value2int v1) (value2string v2)))
  where
    genericDp n = reverse . genericDrop n . reverse
evalPredef id [v]
  | id == cIsUpper= return (fmap toPBool (liftM (all isUpper) (value2string v)))
evalPredef id [v]
  | id == cToUpper= return (fmap string2value (liftM (map toUpper) (value2string v)))
evalPredef id [v]
  | id == cToLower= return (fmap string2value (liftM (map toLower) (value2string v)))
evalPredef id [v1,v2]
  | id == cEqStr  = return (fmap toPBool (liftM2 (==) (value2string v1) (value2string v2)))
evalPredef id [v1,v2]
  | id == cOccur  = return (fmap toPBool (liftM2 occur (value2string v1) (value2string v2)))
evalPredef id [v1,v2]
  | id == cOccurs  = return (fmap toPBool (liftM2 occurs (value2string v1) (value2string v2)))
evalPredef id [v1,v2]
  | id == cEqInt  = return (fmap toPBool (liftM2 (==) (value2int v1) (value2int v2)))
evalPredef id [v1,v2]
  | id == cLessInt= return (fmap toPBool (liftM2 (<) (value2int v1) (value2int v2)))
evalPredef id [v1,v2]
  | id == cPlus   = return (fmap VInt (liftM2 (+) (value2int v1) (value2int v2)))
evalPredef id [v]
  | id == cError  = case value2string v of
                      Just msg -> fail msg
                      Nothing  -> return Nothing
evalPredef id vs  = return Nothing

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

patternMatch v0 []                      = fail "No matching pattern found"
patternMatch v0 ((env0,ps,args0,t):eqs) = match env0 ps eqs args0
  where
    match env []              eqs      args  = eval env t args
    match env (PT ty p   :ps) eqs      args  = match env (p:ps) eqs args
    match env (PAlt p1 p2:ps) eqs      args  = match env (p1:ps) ((env,p2:ps,args,t):eqs) args
    match env (PM q      :ps) eqs      args  = do t <- lookupGlobal q
                                                  case t of
                                                    EPatt _ _ p -> match env (p:ps) eqs args
                                                    _ -> evalError $ hang "Expected pattern macro:" 4
                                                                          (pp t)
    match env (PV v      :ps) eqs (arg:args) = match ((v,arg):env) ps eqs args
    match env (PAs v p   :ps) eqs (arg:args) = match ((v,arg):env) (p:ps) eqs (arg:args)
    match env (PW        :ps) eqs (arg:args) = match env ps eqs args
    match env (PTilde _  :ps) eqs (arg:args) = match env ps eqs args
    match env (p         :ps) eqs (arg:args) = do
      v <- force arg []
      case (p,v) of
        (p,       VMeta i envi vs  ) -> return (VSusp i envi vs (\tnk -> match env (p:ps) eqs (tnk:args)))
        (p,       VGen  i vs       ) -> return v0
        (p,       VSusp i envi vs k) -> return (VSusp i envi vs (\tnk -> match env (p:ps) eqs (tnk:args)))
        (PP q qs, VApp r tnks)
          | q == r        -> match env (qs++ps) eqs (tnks++args)
        (PR pas, VR as)   -> matchRec env pas as ps eqs args
        (PString s1, VStr s2)
          | s1 == s2      -> match env ps eqs args
        (PString s1, VC [])
          | null s1       -> match env ps eqs args
        (PSeq min1 max1 p1 min2 max2 p2,v)
                          -> case value2string v of
                               Just s  -> do let n  = length s
                                                 lo = min1 `max` (n-fromMaybe n max2)
                                                 hi = (n-min2) `min` fromMaybe n max1
                                                 (ds,cs) = splitAt lo s
                                             eqs <- matchStr env (p1:p2:ps) eqs (hi-lo) (reverse ds) cs args
                                             patternMatch v0 eqs
                               Nothing -> return v0
        (PRep minp maxp p, v)
                          -> case value2string v of
                               Just s  -> do let n = length s `div` (max minp 1)
                                             eqs <- matchRep env n minp maxp p minp maxp p ps ((env,PString []:ps,(arg:args),t) : eqs) (arg:args)
                                             patternMatch v0 eqs
                               Nothing -> return v0
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

value2term i (VApp q tnks) =
  foldM (\e1 tnk -> fmap (App e1) (force tnk [] >>= value2term i)) (QC q) tnks
value2term i (VMeta m env tnks) = do
  res <- zonk m tnks
  case res of
    Right i -> foldM (\e1 tnk -> fmap (App e1) (force tnk [] >>= value2term i)) (Meta i) tnks
    Left  v -> value2term i v
value2term i (VGen j tnks) =
  foldM (\e1 tnk -> fmap (App e1) (force tnk [] >>= value2term i)) (Vr (identS ('v':show j))) tnks
value2term i (VClosure env (Abs b x t)) = do
  tnk <- newGen i
  v <- eval ((x,tnk):env) t []
  t <- value2term (i+1) v
  return (Abs b (identS ('v':show i)) t)
value2term i (VProd b x v1 env t2) = do
  t1 <- value2term i v1
  tnk <- newGen i
  v2 <- eval ((x,tnk):env) t2 []
  t2 <- value2term (i+1) v2
  return (Prod b x t1 t2)
value2term i (VRecType lbls) = do
  lbls <- mapM (\(lbl,v) -> fmap ((,) lbl) (value2term i v)) lbls
  return (RecType lbls)
value2term i (VR as) = do
  as <- mapM (\(lbl,tnk) -> fmap (\t -> (lbl,(Nothing,t))) (force tnk [] >>= value2term i)) as
  return (R as)
value2term i (VP v lbl tnks) = do
  t <- value2term i v
  foldM (\e1 tnk -> fmap (App e1) (force tnk [] >>= value2term i)) (P t lbl) tnks
value2term i (VTable v1 v2) = do
  t1 <- value2term i v1
  t2 <- value2term i v2
  return (Table t1 t2)
value2term i (VT ti cs) = return (T ti cs)
value2term i (VV ty tnks) = do ts <- mapM (\tnk -> force tnk [] >>= value2term i) tnks
                               return (V ty ts)
value2term i (VS v1 tnk2 tnks) = do t1 <- value2term i v1
                                    t2 <- force tnk2 [] >>= value2term i
                                    foldM (\e1 tnk -> fmap (App e1) (force tnk [] >>= value2term i)) (S t1 t2) tnks
value2term i (VSort s) = return (Sort s)
value2term i (VStr tok) = return (K tok)
value2term i (VInt n) = return (EInt n)
value2term i (VFlt n) = return (EFloat n)
value2term i (VC vs) = do
  ts <- mapM (value2term i) vs
  case ts of
    []     -> return Empty
    (t:ts) -> return (foldl C t ts)
value2term i (VPatt min max p) = return (EPatt min max p)
value2term i (VPattType v) = do t <- value2term i v
                                return (EPattType t)

value2string (VStr s) = Just s
value2string (VC vs)  = fmap unwords (mapM value2string vs)
value2string _        = Nothing

string2value s =
  case words s of
    []  -> VC []
    [w] -> VStr w
    ws  -> VC (map VStr ws)

value2int (VInt n) = Just n
value2int _        = Nothing

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
  
lookupGlobal :: QIdent -> EvalM s Term
lookupGlobal q = EvalM $ \gr k mt r -> do
  case lookupResDef gr q of
    Ok t    -> k t mt r
    Bad msg -> return (Fail (pp msg))

newThunk env t = EvalM $ \gr k mt r -> do
 tnk <- newSTRef (Unevaluated env t)
 k tnk mt r

newEvaluatedThunk v = EvalM $ \gr k mt r -> do
 tnk <- newSTRef (Evaluated v)
 k tnk mt r

newMeta i = EvalM $ \gr k mt r ->
  if i == 0
    then do tnk <- newSTRef (Unbound i)
            k tnk mt r
    else case Map.lookup i mt of
           Just tnk -> k tnk mt r
           Nothing  -> do tnk <- newSTRef (Unbound i)
                          k tnk (Map.insert i tnk mt) r

newGen i = EvalM $ \gr k mt r -> do
 tnk <- newSTRef (Evaluated (VGen i []))
 k tnk mt r

force tnk vs = EvalM $ \gr k mt r -> do 
  s <- readSTRef tnk
  case s of
    Unevaluated env t -> case eval env t vs of
                           EvalM f -> f gr (\v mt r -> do writeSTRef tnk (Evaluated v)
                                                          r <- k v mt r
                                                          writeSTRef tnk s
                                                          return r) mt r
    Evaluated v       -> case apply v vs of
                           EvalM f -> f gr k mt r

zonk tnk vs = EvalM $ \gr k mt r -> do
  s <- readSTRef tnk
  case s of
    Evaluated v -> case apply v vs of
                     EvalM f -> f gr (k . Left) mt r
    Unbound i   -> k (Right i) mt r
