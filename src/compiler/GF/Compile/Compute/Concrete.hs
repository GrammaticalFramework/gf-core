{-# LANGUAGE RankNTypes #-}

-- | Functions for computing the values of terms in the concrete syntax, in
-- | preparation for PMCFG generation.
module GF.Compile.Compute.Concrete
           (GlobalEnv, GLocation, resourceValues, geLoc, geGrammar,
            normalForm,
            Value(..), Env, value2term, eval
           ) where
import Prelude hiding ((<>)) -- GHC 8.4.1 clash with Text.PrettyPrint

import GF.Grammar hiding (Env, VGen, VApp, VRecType)
import GF.Grammar.Lookup(lookupResDefLoc,allParamValues)
import GF.Grammar.Predef(cPredef,cErrorType,cTok,cStr,cTrace,cPBool)
import GF.Grammar.PatternMatch(matchPattern,measurePatt)
import GF.Grammar.Lockfield(isLockLabel,lockRecType) --unlockRecord,lockLabel
import GF.Compile.Compute.Value hiding (Error)
import GF.Compile.Compute.Predef(predef,predefName,delta)
import GF.Data.Str(Str,glueStr,str2strings,str,sstr,plusStr,strTok)
import GF.Data.Operations(Err,err,errIn,maybeErr,mapPairsM)
import GF.Data.Utilities(mapFst,mapSnd)
import GF.Infra.Option
import Data.STRef
import Control.Monad
import Control.Monad.ST
import Control.Applicative
import qualified Data.Map as Map

-- * Main entry points

normalForm :: GlobalEnv -> L Ident -> Term -> Term
normalForm ge loc t =
  case runEvalM (eval [] t [] >>= value2term 0) of
    [t] -> t
    ts  -> FV ts

eval env (Vr x)         vs  = case lookup x env of
                                Just tnk -> force tnk vs
                                Nothing  -> error "Unknown variable"
eval env (Con f)        vs  = return (VApp f vs)
eval env (K t)          vs  = return (VStr t)
eval env (App t1 t2)    vs  = do tnk <- newThunk env t2
                                 eval env t1 (tnk : vs)
eval env (Abs b x t)    []  = return (VClosure env (Abs b x t))
eval env (Abs b x t) (v:vs) = eval ((x,v):env) t vs
eval env (Meta i)       vs  = do tnk <- newMeta i
                                 return (VMeta tnk env vs)
eval env (Typed t ty)   vs  = eval env t vs
eval env (C t1 t2)      vs  = do tnk1 <- newThunk env t1
                                 tnk2 <- newThunk env t2
                                 return (VC tnk1 tnk2)
eval env (FV ts)        vs  = msum [eval env t vs | t <- ts]

apply v                             []  = return v
apply (VApp f  vs0)                 vs  = return (VApp f (vs0++vs))
apply (VMeta m env vs0)             vs  = return (VMeta m env (vs0++vs))
apply (VGen i  vs0)                 vs  = return (VGen i (vs0++vs))
apply (VClosure env (Abs b x t)) (v:vs) = eval ((x,v):env) t vs


value2term i (VApp f tnks) =
  foldM (\e1 tnk -> fmap (App e1) (force tnk [] >>= value2term i)) (Con f) tnks
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
value2term i (VStr tok) = return (K tok)
value2term i (VC tnk1 tnk2) = do
  t1 <- force tnk1 [] >>= value2term i
  t2 <- force tnk2 [] >>= value2term i
  return (C t1 t2)


-----------------------------------------------------------------------
-- * Environments

data GlobalEnv = GE Grammar Options GLocation
type GLocation = L Ident

geLoc     (GE _  _ loc) = loc
geGrammar (GE gr _ _  ) = gr

-- | Convert operators once, not every time they are looked up
resourceValues :: Options -> SourceGrammar -> GlobalEnv
resourceValues opts gr = GE gr opts (L NoLoc identW)


-----------------------------------------------------------------------
-- * Evaluation monad

type MetaThunks s = Map.Map MetaId (Thunk s)

newtype EvalM s a = EvalM (forall r . (a -> MetaThunks s -> r -> ST s r) -> MetaThunks s -> r -> ST s r)

instance Functor (EvalM s) where
  fmap f (EvalM g) = EvalM (\k -> g (k . f))

instance Applicative (EvalM s) where
  pure x = EvalM (\k -> k x)
  (EvalM f) <*> (EvalM x) = EvalM (\k -> f (\f -> x (\x -> k (f x))))

instance Monad (EvalM s) where
  (EvalM f) >>= g = EvalM (\k -> f (\x -> case g x of
                                            EvalM g -> g k))

instance Alternative (EvalM s) where
  empty = EvalM (\k _ -> return)
  (EvalM f) <|> (EvalM g) = EvalM (\k mt r -> f k mt r >>= \r -> g k mt r)

instance MonadPlus (EvalM s) where


runEvalM :: (forall s . EvalM s a) -> [a]
runEvalM f = reverse $
             runST (case f of
                      EvalM f -> f (\x mt xs -> return (x:xs)) Map.empty [])

newThunk env t = EvalM $ \k mt r -> do
 tnk <- newSTRef (Unevaluated env t)
 k tnk mt r

newMeta i = EvalM $ \k mt r ->
  if i == 0
    then do tnk <- newSTRef (Unbound i)
            k tnk mt r
    else case Map.lookup i mt of
           Just tnk -> k tnk mt r
           Nothing  -> do tnk <- newSTRef (Unbound i)
                          k tnk (Map.insert i tnk mt) r

newGen i = EvalM $ \k mt r -> do
 tnk <- newSTRef (Evaluated (VGen i []))
 k tnk mt r

force tnk vs = EvalM $ \k mt r -> do 
  s <- readSTRef tnk
  case s of
    Unevaluated env t -> case eval env t vs of
                           EvalM f -> f (\v mt r -> do writeSTRef tnk (Evaluated v)
                                                       r <- k v mt r
                                                       writeSTRef tnk s
                                                       return r) mt r
    Evaluated v       -> case apply v vs of
                           EvalM f -> f k mt r

zonk tnk vs = EvalM $ \k mt r -> do
  s <- readSTRef tnk
  case s of
    Evaluated v -> case apply v vs of
                     EvalM f -> f (k . Left) mt r
    Unbound i   -> k (Right i) mt r
