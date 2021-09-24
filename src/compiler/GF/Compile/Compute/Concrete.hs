{-# LANGUAGE RankNTypes #-}

-- | Functions for computing the values of terms in the concrete syntax, in
-- | preparation for PMCFG generation.
module GF.Compile.Compute.Concrete
           (normalForm,
            Value(..), Env, value2term, eval
           ) where
import Prelude hiding ((<>)) -- GHC 8.4.1 clash with Text.PrettyPrint

import GF.Grammar hiding (Env, VGen, VApp, VRecType)
import GF.Grammar.Lookup(lookupResDef,allParamValues)
import GF.Grammar.Predef(cPredef,cErrorType,cTok,cStr,cTrace,cPBool)
import GF.Grammar.PatternMatch(matchPattern,measurePatt)
import GF.Grammar.Lockfield(isLockLabel,lockRecType) --unlockRecord,lockLabel
import GF.Compile.Compute.Predef(predef,predefName,delta)
import GF.Data.Str(Str,glueStr,str2strings,str,sstr,plusStr,strTok)
import GF.Data.Operations(Err(..),err,errIn,maybeErr,mapPairsM)
import GF.Data.Utilities(mapFst,mapSnd)
import GF.Infra.Option
import Data.STRef
import Control.Monad
import Control.Monad.ST
import Control.Applicative
import qualified Data.Map as Map

-- * Main entry points

normalForm :: Grammar -> L Ident -> Term -> Term
normalForm gr loc t =
  case runEvalM gr (eval [] t [] >>= value2term 0) of
    [t] -> t
    ts  -> FV ts


data ThunkState s
  = Unevaluated (Env s) Term
  | Evaluated (Value s)
  | Unbound {-# UNPACK #-} !MetaId

type Thunk s = STRef s (ThunkState s)
type Env s = [(Ident,Thunk s)]

data Value s
  = VApp QIdent [Thunk s]
  | VMeta (Thunk s) (Env s) [Thunk s]
  | VGen  {-# UNPACK #-} !Int [Thunk s]
  | VClosure (Env s) Term
  | VR [(Label, Thunk s)]
  | VP (Value s) Label
  | VT TInfo [Case]
  | VV Type [Thunk s]
  | VS (Value s) (Value s)
  | VSort Ident
  | VInt Integer
  | VFlt Double
  | VStr String
  | VC [Value s]


eval env (Vr x)         vs  = case lookup x env of
                                Just tnk -> force tnk vs
                                Nothing  -> error "Unknown variable"
eval env (Sort s)       vs  = return (VSort s)
eval env (EInt n)       vs  = return (VInt n)
eval env (EFloat d)     vs  = return (VFlt d)
eval env (K t)          vs  = return (VStr t)
eval env Empty          vs  = return (VC [])
eval env (App t1 t2)    vs  = do tnk <- newThunk env t2
                                 eval env t1 (tnk : vs)
eval env (Abs b x t)    []  = return (VClosure env (Abs b x t))
eval env (Abs b x t) (v:vs) = eval ((x,v):env) t vs
eval env (Meta i)       vs  = do tnk <- newMeta i
                                 return (VMeta tnk env vs)
eval env (ImplArg t)    vs  = eval env t vs
eval env (Typed t ty)   vs  = eval env t vs
eval env (R as)         vs  = do as <- mapM (\(lbl,(_,t)) -> fmap ((,) lbl) (newThunk env t)) as
                                 return (VR as)
eval env (P t lbl)      vs  = do v <- eval env t []
                                 case v of
                                   VR as -> case lookup lbl as of
                                              Nothing  -> error ("Missing value for label "++show lbl)
                                              Just tnk -> force tnk vs
                                   v     -> return (VP v lbl)
eval env (T i cs)       vs  = return (VT i cs)
eval env (V ty ts)      vs  = do tnks <- mapM (newThunk env) ts
                                 return (VV ty tnks)
eval env (S t1 t2)      vs  = do v1   <- eval env t1 []
                                 tnk2 <- newThunk env t2
                                 case v1 of
                                   VT _ cs -> do (env,t) <- patternMatch env cs tnk2
                                                 eval env t vs
                                   v1      -> do v2 <- force tnk2 []
                                                 return (VS v1 v2)
eval env (Let (x,(_,t1)) t2) vs = do tnk <- newThunk env t1
                                     eval ((x,tnk):env) t2 vs
eval env (Q q)               vs = do t <- lookupGlobal q
                                     eval env t vs
eval env (QC q)         vs  = return (VApp q vs)
eval env (C t1 t2)      vs  = do v1 <- eval env t1 vs
                                 v2 <- eval env t2 vs
                                 case (v1,v2) of
                                   (VC vs1,VC vs2) -> return (VC (vs1++vs2))
                                   (VC vs1,v2    ) -> return (VC (vs1++[v2]))
                                   (v1,    VC vs2) -> return (VC ([v1]++vs2))
                                   (v1,    v2    ) -> return (VC [v1,v2])
eval env (FV ts)        vs  = msum [eval env t vs | t <- ts]
eval env t vs = error (show t)

apply v                             []  = return v
apply (VApp f  vs0)                 vs  = return (VApp f (vs0++vs))
apply (VMeta m env vs0)             vs  = return (VMeta m env (vs0++vs))
apply (VGen i  vs0)                 vs  = return (VGen i (vs0++vs))
apply (VClosure env (Abs b x t)) (v:vs) = eval ((x,v):env) t vs

patternMatch env []         tnk = error "No matching pattern found"
patternMatch env ((p,t):cs) tnk = do
  res <- match env p tnk
  case res of
    Nothing  -> patternMatch env cs tnk
    Just env -> return (env,t)
  where
    match env (PP q ps) tnk = do v <- force tnk []
                                 case v of
                                   VApp r tnks | q == r -> match' env ps tnks
                                   _                    -> return Nothing
    match env (PV v)    tnk = return (Just ((v,tnk):env))
    match env PW        tnk = return (Just env)
    match env (PAs v p) tnk = match ((v,tnk):env) p tnk

    match' env []     []         =
      return (Just env)
    match' env (p:ps) (tnk:tnks) = do
      res <- match env p tnk
      case res of
        Nothing  -> return Nothing
        Just env -> match' env ps tnks

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
value2term i (VR as) = do
  as <- mapM (\(lbl,tnk) -> fmap (\t -> (lbl,(Nothing,t))) (force tnk [] >>= value2term i)) as
  return (R as)
value2term i (VP v lbl) = do
  t <- value2term i v
  return (P t lbl)
value2term i (VT ti cs) = return (T ti cs)
value2term i (VV ty tnks) = do ts <- mapM (\tnk -> force tnk [] >>= value2term i) tnks
                               return (V ty ts)
value2term i (VSort s) = return (Sort s)
value2term i (VStr tok) = return (K tok)
value2term i (VInt n) = return (EInt n)
value2term i (VFlt n) = return (EFloat n)
value2term i (VC vs) = do
  ts <- mapM (value2term i) vs
  case ts of
    []     -> return Empty
    (t:ts) -> return (foldl C t ts)

-----------------------------------------------------------------------
-- * Evaluation monad

type MetaThunks s = Map.Map MetaId (Thunk s)

newtype EvalM s a = EvalM (forall r . Grammar -> (a -> MetaThunks s -> r -> ST s r) -> MetaThunks s -> r -> ST s r)

instance Functor (EvalM s) where
  fmap f (EvalM g) = EvalM (\gr k -> g gr (k . f))

instance Applicative (EvalM s) where
  pure x = EvalM (\gr k -> k x)
  (EvalM f) <*> (EvalM x) = EvalM (\gr k -> f gr (\f -> x gr (\x -> k (f x))))

instance Monad (EvalM s) where
  (EvalM f) >>= g = EvalM (\gr k -> f gr (\x -> case g x of
                                                  EvalM g -> g gr k))


instance Alternative (EvalM s) where
  empty = EvalM (\gr k _ -> return)
  (EvalM f) <|> (EvalM g) = EvalM (\gr k mt r -> f gr k mt r >>= \r -> g gr k mt r)

instance MonadPlus (EvalM s) where


runEvalM :: Grammar -> (forall s . EvalM s a) -> [a]
runEvalM gr f = reverse $
                runST (case f of
                         EvalM f -> f gr (\x mt xs -> return (x:xs)) Map.empty [])

lookupGlobal :: QIdent -> EvalM s Term
lookupGlobal q = EvalM $ \gr k mt r -> do
  case lookupResDef gr q of
    Ok t    -> k t mt r
    Bad msg -> error msg

newThunk env t = EvalM $ \gr k mt r -> do
 tnk <- newSTRef (Unevaluated env t)
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
