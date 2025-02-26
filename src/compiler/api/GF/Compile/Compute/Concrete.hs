{-# LANGUAGE RankNTypes, BangPatterns, CPP, ExistentialQuantification, LambdaCase #-}

-- | Functions for computing the values of terms in the concrete syntax, in
-- | preparation for PMCFG generation.
module GF.Compile.Compute.Concrete
           ( normalForm, normalFlatForm, normalStringForm
           , Value(..), Thunk, ThunkState(..), Env, Scope, showValue, isOpen, isTermOpen
           , PredefImpl, Predef(..), PredefCombinator, ($\)
           , pdForce, pdClosedArgs, pdArity, pdStandard
           , MetaThunks, Constraint, PredefTable, Globals(..), ConstValue(..)
           , EvalM(..), runEvalM, runEvalOneM, reset, try, evalError, evalWarn
           , eval, apply, force, value2term, patternMatch, stdPredef
           , unsafeIOToEvalM
           , newThunk, newEvaluatedThunk
           , newResiduation, newNarrowing, getVariables
           , getRef, setRef
           , getResDef, getInfo, getResType, getOverload
           , getAllParamValues
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
import GF.Data.Utilities(splitAt',(<||>),anyM)
import GF.Infra.CheckM
import GF.Infra.Option
import Data.STRef
import Data.Maybe(fromMaybe)
import Data.List
import Data.Char
import Control.Monad
import Control.Monad.ST
import Control.Monad.ST.Unsafe
import Control.Applicative hiding (Const)
import qualified Control.Monad.Fail as Fail
import Data.Functor ((<&>))
import qualified Data.Map as Map
import GF.Text.Pretty
import PGF2.Transactions(LIndex)

-- * Main entry points

-- | The term is fully evaluated. Variants are only expanded if necessary for the evaluation.
normalForm :: Globals -> Term -> Check Term
normalForm globals t =
  fmap mkFV (runEvalM globals (eval [] t [] >>= value2term False []))
  where
    mkFV [t] = t
    mkFV ts  = FV ts

-- | The result is a list of terms and contains all variants. Each term by itself does not contain any variants.
normalFlatForm :: Globals -> Term -> Check [Term]
normalFlatForm globals t =
  runEvalM globals (eval [] t [] >>= value2term True [])

normalStringForm :: Globals -> Term -> Check [String]
normalStringForm globals t =
  fmap toStrs (runEvalM globals (fmap value2string (eval [] t [])))
  where
    toStrs []            = []
    toStrs (Const s:cfs) = s : toStrs cfs
    toStrs (_      :cfs) = toStrs cfs

type Sigma s = Value s
type Constraint s = Value s

data ThunkState s
  = Unevaluated (Env s) Term
  | Evaluated {-# UNPACK #-} !Int (Value s)
  | Hole        {-# UNPACK #-} !MetaId
  | Narrowing   {-# UNPACK #-} !MetaId Type
  | Residuation {-# UNPACK #-} !MetaId (Scope s) (Maybe (Constraint s))

type Thunk s = STRef s (ThunkState s)
type Env s = [(Ident,Thunk s)]
type Scope s = [(Ident,Value s)]

data Value s
  = VApp QIdent [Thunk s]
  | VMeta (Thunk s) [Thunk s]
  | VSusp (Thunk s) (Value s -> EvalM s (Value s)) [Thunk s]
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
  | VMarkup Ident [(Ident,Value s)] [Value s]
    -- These two constructors are only used internally
    -- in the PMCFG generator.
  | VSymCat Int LIndex [(LIndex, (Thunk s, Type))]
  | VSymVar Int Int
    -- These two constructors are only used internally
    -- in the type checker.
  | VCRecType [(Label, Bool, Constraint s)]
  | VCInts (Maybe Integer) (Maybe Integer)

showValue (VApp q tnks) = "(VApp "++unwords (show q : map (const "_") tnks) ++ ")"
showValue (VMeta _ _) = "VMeta"
showValue (VSusp _ _ _) = "VSusp"
showValue (VGen i _) = "(VGen "++show i++")"
showValue (VClosure _ _) = "VClosure"
showValue (VProd _ x v1 v2) = "VProd ("++show x++") ("++showValue v1++") ("++showValue v2++")"
showValue (VRecType _) = "VRecType"
showValue (VR lbls) = "(VR {"++unwords (map (\(lbl,_) -> show lbl) lbls)++"})"
showValue (VP v l _) = "(VP "++showValue v++" "++show l++")"
showValue (VExtR _ _) = "VExtR"
showValue (VTable v1 v2) = "VTable ("++showValue v1++") ("++showValue v2++")"
showValue (VT _ _ cs) = "(VT "++show cs++")"
showValue (VV _ _) = "VV"
showValue (VS v _ _) = "(VS "++showValue v++")"
showValue (VSort s) = "(VSort "++show s++")"
showValue (VInt _) = "VInt"
showValue (VFlt _) = "VFlt"
showValue (VStr s) = "(VStr "++show s++")"
showValue VEmpty = "VEmpty"
showValue (VC v1 v2) = "(VC "++showValue v1++" "++showValue v2++")"
showValue (VGlue _ _) = "VGlue"
showValue (VPatt _ _ _) = "VPatt"
showValue (VPattType _) = "VPattType"
showValue (VAlts _ _) = "VAlts"
showValue (VStrs _) = "VStrs"
showValue (VSymCat _ _ _) = "VSymCat"

isOpen :: [Ident] -> Value s -> EvalM s Bool
isOpen bound (VGen x args) = pure (x >= length bound) <||> anyM (isThunkOpen bound) args
isOpen bound (VApp t args) = anyM (force >=> isOpen bound) args
isOpen bound (VMeta v args) = isThunkOpen bound v <||> anyM (isThunkOpen bound) args
isOpen bound (VSusp j k args) = anyM (isThunkOpen bound) args <||> (k (VGen maxBound args) >>= isOpen bound)
isOpen bound (VClosure env (Abs b x t)) = isTermOpen (x : bound ++ (fst <$> env)) t
isOpen bound (VProd b x d cod) = isOpen bound d <||> case cod of
                                                       VClosure env t -> isOpen (x:bound) cod
                                                       _              -> isOpen bound cod
isOpen bound (VRecType fs) = anyM (isOpen bound . snd) fs
isOpen bound (VR fs) = anyM (isThunkOpen bound . snd) fs
isOpen bound (VP v f args) = isOpen bound v <||> anyM (isThunkOpen bound) args
isOpen bound (VExtR v v') = isOpen bound v <||> isOpen bound v'
isOpen bound (VTable d cod) = isOpen bound d <||> isOpen bound cod
isOpen bound (VT ty env cs) = isOpen bound ty <||> anyM (isTermOpen (bound ++ (fst <$> env)) . snd) cs
isOpen bound (VV ty cs) = isOpen bound ty <||> anyM (isThunkOpen bound) cs
isOpen bound (VS v x args) = isOpen bound v <||> isThunkOpen bound x <||> anyM (isThunkOpen bound) args
isOpen bound (VC v v') = isOpen bound v <||> isOpen bound v'
isOpen bound (VGlue v v') = isOpen bound v <||> isOpen bound v'
isOpen bound (VPattType ty) = isOpen bound ty
isOpen bound (VAlts d as) = isOpen bound d <||> anyM (\(x,y) -> isOpen bound x <||> isOpen bound y) as
isOpen bound (VStrs vs) = anyM (isOpen bound) vs
isOpen bound (VMarkup tag as vs) = anyM (isOpen bound) vs <||> anyM (isOpen bound . snd) as
isOpen _ _ = return False

isTermOpen :: [Ident] -> Term -> EvalM s Bool
isTermOpen bound (Vr x) = return $ x `notElem` bound
isTermOpen bound (App f x) = isTermOpen bound f <||> isTermOpen bound x
isTermOpen bound (Abs b x t) = isTermOpen (x:bound) t
isTermOpen bound (Meta n) = EvalM $ \gl k mt d r msgs ->
  case Map.lookup n mt of
    Just tnk -> case isThunkOpen bound tnk of EvalM f -> f gl k mt d r msgs
    Nothing  -> k True mt d r msgs -- Treat unknown metas as unbound
isTermOpen bound (ImplArg t) = isTermOpen bound t
isTermOpen bound (Prod b x d cod) = isTermOpen bound d <||> isTermOpen (x:bound) cod
isTermOpen bound (Typed t ty) = isTermOpen bound t
isTermOpen bound (Example t s) = isTermOpen bound t
isTermOpen bound (RecType fs) = anyM (isTermOpen bound . snd) fs
isTermOpen bound (R fs) = anyM (isTermOpen bound . snd . snd) fs
isTermOpen bound (P t f) = isTermOpen bound t
isTermOpen bound (ExtR t t') = isTermOpen bound t <||> isTermOpen bound t'
isTermOpen bound (Table d cod) = isTermOpen bound d <||> isTermOpen bound cod
isTermOpen bound (T (TTyped ty) cs) = isTermOpen bound ty <||> anyM (isTermOpen bound . snd) cs
isTermOpen bound (T (TWild ty) cs) = isTermOpen bound ty <||> anyM (isTermOpen bound . snd) cs
isTermOpen bound (T _ cs) = anyM (isTermOpen bound . snd) cs
isTermOpen bound (V ty cs) = isTermOpen bound ty <||> anyM (isTermOpen bound) cs
isTermOpen bound (S t x) = isTermOpen bound t <||> isTermOpen bound x
isTermOpen bound (Let (x,(ty,d)) t) = isTermOpen bound d <||> isTermOpen (x:bound) t
isTermOpen bound (C t t') = isTermOpen bound t <||> isTermOpen bound t'
isTermOpen bound (Glue t t') = isTermOpen bound t <||> isTermOpen bound t'
isTermOpen bound (EPattType ty) = isTermOpen bound ty
isTermOpen bound (ELincat c ty) = isTermOpen bound ty
isTermOpen bound (ELin c t) = isTermOpen bound t
isTermOpen bound (FV ts) = anyM (isTermOpen bound) ts
isTermOpen bound (Markup tag as ts) = anyM (isTermOpen bound) ts <||> anyM (isTermOpen bound . snd) as
isTermOpen bound (Reset c t) = isTermOpen bound t
isTermOpen bound (Alts d as) = isTermOpen bound d <||> anyM (\(x,y) -> isTermOpen bound x <||> isTermOpen bound y) as
isTermOpen bound (Strs ts) = anyM (isTermOpen bound) ts
isTermOpen _ _ = return False

isThunkOpen :: [Ident] -> Thunk s -> EvalM s Bool
isThunkOpen bound tnk = EvalM $ \gl k mt d r msgs -> do
  c <- readSTRef tnk <&> \case
    Unevaluated env t            -> isTermOpen (bound ++ (fst <$> env)) t
    Evaluated i v                -> isOpen bound v
    Hole i                       -> return True -- Treat open metas as unbound
    Narrowing i ty               -> return True -- Ditto
    Residuation i scope (Just v) -> isOpen bound v
    Residuation i scope Nothing  -> return True -- Ditto
  case c of EvalM f -> f gl k mt d r msgs

eval env (Vr x)         vs  = do (tnk,depth) <- lookup x env
                                 withVar depth $ do
                                   v <- force tnk
                                   apply v vs
                              where
                                lookup x []   = evalError ("Variable" <+> pp x <+> "is not in scope")
                                lookup x ((y,tnk):env)
                                  | x == y    = return (tnk,length env)
                                  | otherwise = lookup x env
eval env (Sort s)       []
  | s == cTok               = return (VSort cStr)
  | otherwise               = return (VSort s)
eval env (EInt n)       []  = return (VInt n)
eval env (EFloat d)     []  = return (VFlt d)
eval env (K t)          []  = return (VStr t)
eval env Empty          []  = return VEmpty
eval env (App t1 t2)    vs  = do tnk <- newThunk env t2
                                 eval env t1 (tnk : vs)
eval env (Abs b x t)    []  = return (VClosure env (Abs b x t))
eval env (Abs b x t) (v:vs) = eval ((x,v):env) t vs
eval env (Meta i)       vs  = do tnk <- newHole i
                                 return (VMeta tnk vs)
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
                                   VV vty tnks  -> do ty <- value2term True (map fst env) vty
                                                      vtableSelect v0 ty tnks tnk2 vs
                                   v1           -> return v0
eval env (Let (x,(_,t1)) t2) vs = do tnk <- newThunk env t1
                                     eval ((x,tnk):env) t2 vs
eval env (Q q@(m,id)) vs
  | m == cPredef            = evalPredef id vs   
  | otherwise               = do t <- getResDef q
                                 eval env t vs
eval env (QC q)         vs  = return (VApp q vs)
eval env (C t1 t2)      []  = do v1 <- eval env t1 []
                                 v2 <- eval env t2 []
                                 case (v1,v2) of
                                   (v1,    VEmpty) -> return v1
                                   (VEmpty,v2    ) -> return v2
                                   _               -> return (VC v1 v2)
eval env t@(Glue t1 t2) []  = do v1 <- eval env t1 []
                                 v2 <- eval env t2 []
                                 let glue VEmpty        v             = v
                                     glue (VC v1 v2)    v             = VC v1 (glue v2 v)
                                     glue (VApp q [])   v
                                       | q == (cPredef,cNonExist)     = VApp q []
                                     glue v             VEmpty        = v
                                     glue v             (VC v1 v2)    = VC (glue v v1) v2
                                     glue v             (VApp q [])
                                       | q == (cPredef,cNonExist)     = VApp q []
                                     glue (VStr s1)     (VStr s2)     = VStr (s1++s2)
                                     glue v             (VAlts d vas) = VAlts (glue v d) [(glue v v',ss) | (v',ss) <- vas]
                                     glue (VAlts d vas) (VStr s)      = pre d vas s
                                     glue (VAlts d vas) v             = glue d v
                                     glue v1          v2              = VGlue v1 v2

                                     pre vd []                 s             = glue vd (VStr s)
                                     pre vd ((v,VStrs ss):vas) s
                                      | or [startsWith s' s | VStr s' <- ss] = glue v (VStr s)
                                      | otherwise                            = pre vd vas s

                                 return (glue v1 v2)
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
eval env (Markup tag as ts) [] =
                              do as <- mapM (\(id,t) -> eval env t [] >>= \v -> return (id,v)) as
                                 vs <- mapM (\t -> eval env t []) ts
                                 return (VMarkup tag as vs)
eval env (Reset c t) [] = do let limit All       = id
                                 limit (Limit n) = fmap (genericTake n)
                             vs <- limit c (reset (eval env t []))
                             return (VMarkup identW [] vs)
eval env (TSymCat d r rs) []= do rs <- forM rs $ \(i,(pv,ty)) ->
                                         case lookup pv env of
                                           Just tnk -> return (i,(tnk,ty))
                                           Nothing  -> evalError ("Variable" <+> pp pv <+> "is not in scope")
                                 return (VSymCat d r rs)
eval env (TSymVar d r)  []  = do return (VSymVar d r)
eval env t              vs  = evalError ("Cannot reduce term" <+> pp t)

apply v                             []  = return v
apply (VMeta m vs0)                 vs  = return (VMeta m   (vs0++vs))
apply (VSusp m k vs0)               vs  = return (VSusp m k (vs0++vs))
apply (VApp f@(m,p) vs0)            vs
  | m == cPredef                        = evalPredef p (vs0++vs)
  | otherwise                           = return (VApp f (vs0++vs))
apply (VGen i  vs0)                 vs  = return (VGen i (vs0++vs))
apply (VClosure env (Abs b x t)) (v:vs) = eval ((x,v):env) t vs


stdPredef :: PredefTable s
stdPredef = Map.fromList
  [(cLength, pdStandard 1 $\ \[v] -> case value2string v of
                                       Const s -> return (Const (VInt (genericLength s)))
                                       _       -> return RunTime)
  ,(cTake,   pdStandard 2 $\ \[v1,v2] -> return (fmap string2value (liftA2 genericTake (value2int v1) (value2string v2))))
  ,(cDrop,   pdStandard 2 $\ \[v1,v2] -> return (fmap string2value (liftA2 genericDrop (value2int v1) (value2string v2))))
  ,(cTk,     pdStandard 2 $\ \[v1,v2] -> return (fmap string2value (liftA2 genericTk (value2int v1) (value2string v2))))
  ,(cDp,     pdStandard 2 $\ \[v1,v2] -> return (fmap string2value (liftA2 genericDp (value2int v1) (value2string v2))))
  ,(cIsUpper,pdStandard 1 $\ \[v]     -> return (fmap toPBool (liftA (all isUpper) (value2string v))))
  ,(cToUpper,pdStandard 1 $\ \[v]     -> return (fmap string2value (liftA (map toUpper) (value2string v))))
  ,(cToLower,pdStandard 1 $\ \[v]     -> return (fmap string2value (liftA (map toLower) (value2string v))))
  ,(cEqStr,  pdStandard 2 $\ \[v1,v2] -> return (fmap toPBool (liftA2 (==) (value2string v1) (value2string v2))))
  ,(cOccur,  pdStandard 2 $\ \[v1,v2] -> return (fmap toPBool (liftA2 occur (value2string v1) (value2string v2))))
  ,(cOccurs, pdStandard 2 $\ \[v1,v2] -> return (fmap toPBool (liftA2 occurs (value2string v1) (value2string v2))))
  ,(cEqInt,  pdStandard 2 $\ \[v1,v2] -> return (fmap toPBool (liftA2 (==) (value2int v1) (value2int v2))))
  ,(cLessInt,pdStandard 2 $\ \[v1,v2] -> return (fmap toPBool (liftA2 (<) (value2int v1) (value2int v2))))
  ,(cPlus,   pdStandard 2 $\ \[v1,v2] -> return (fmap VInt (liftA2 (+) (value2int v1) (value2int v2))))
  ,(cError,  pdStandard 1 $\ \[v]     -> case value2string v of
                           Const msg -> fail msg
                           _         -> fail "Indescribable error appeared")
  ]
  where
    genericTk n = reverse . genericDrop n . reverse
    genericDp n = reverse . genericTake n . reverse

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
        (p,       VMeta i   vs) -> susp i (\v -> apply v vs >>= \v -> match' env p ps eqs arg v args)
        (p,       VGen  i   vs) -> return v0
        (p,       VSusp i k vs) -> susp i (\v -> k v >>= \v -> apply v vs >>= \v -> match' env p ps eqs arg v args)
        (PP q qs, VApp r tnks)
          | q == r        -> match env (qs++ps) eqs (tnks++args)
        (PR pas, VR as)   -> matchRec env (reverse pas) as ps eqs args
        (PString s1, VStr s2)
          | s1 == s2      -> match env ps eqs args
        (PString s1, VEmpty)
          | null s1       -> match env ps eqs args
        (PSeq min1 max1 p1 min2 max2 p2,v)
                          -> case value2string v of
                               Const s -> let n  = length s
                                              lo = min1 `max` (n-fromMaybe n max2)
                                              hi = (n-min2) `min` fromMaybe n max1
                                              (ds,cs) = splitAt lo s
                                          in if lo <= hi
                                               then do eqs <- matchStr env (p1:p2:ps) eqs (hi-lo) (reverse ds) cs args
                                                       patternMatch v0 eqs
                                               else patternMatch v0 eqs
                               RunTime -> return v0
                               NonExist-> patternMatch v0 eqs
        (PRep minp maxp p, v)
                          -> case value2string v of
                               Const s -> do let n = length s `div` (max minp 1)
                                             eqs <- matchRep env n minp maxp p minp maxp p ps ((env,PString []:ps,(arg:args),t) : eqs) (arg:args)
                                             patternMatch v0 eqs
                               RunTime -> return v0
                               NonExist-> patternMatch v0 eqs
        (PChar, VStr [c]) -> match env ps eqs args
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
    value2index (VMeta i vs) ty = do
      v <- susp i (\v -> apply v vs)
      value2index v ty
    value2index (VSusp i k vs) ty = do
      v <- susp i (\v -> k v >>= \v -> apply v vs)
      value2index v ty
    value2index v ty = do t <- value2term True [] v
                          evalError ("the parameter:" <+> ppTerm Unqualified 0 t $$
                                     "cannot be evaluated at compile time.")


susp i ki = EvalM $ \globals@(Gl gr _) k e mt d r msgs -> do
  s <- readSTRef i
  case s of
    Narrowing id (QC q) -> case lookupOrigInfo gr q of
                             Ok (m,ResParam (Just (L _ ps)) _) -> bindParam globals k e mt d r msgs s m ps
                             Bad msg -> return (Fail (pp msg) msgs)
    Narrowing id ty
      | Just max <- isTypeInts ty
                        -> bindInt globals k e mt d r msgs s 0 max
    Evaluated _ v       -> case ki v of
                             EvalM f -> f globals k e mt d r msgs
    _                   -> k (VSusp i ki []) mt d r msgs
  where
    bindParam gr k e mt d r msgs s m []             = return (Success r msgs)
    bindParam gr k e mt d r msgs s m ((p, ctxt):ps) = do
      (mt',tnks) <- mkArgs mt ctxt
      let v = VApp (m,p) tnks
      writeSTRef i (Evaluated 0 v)
      res <- case ki v of
               EvalM f -> f gr k e mt' d r msgs
      writeSTRef i s
      case res of
        Fail msg  msgs -> return (Fail msg msgs)
        Success r msgs -> bindParam gr k e mt d r msgs s m ps

    mkArgs mt []              = return (mt,[])
    mkArgs mt ((_,_,ty):ctxt) = do
      let i = case Map.maxViewWithKey mt of
                Just ((i,_),_) -> i+1
                _              -> 0
      tnk  <- newSTRef (Narrowing i ty)
      (mt,tnks) <- mkArgs (Map.insert i tnk mt) ctxt
      return (mt,tnk:tnks)

    bindInt gr k e mt d r msgs s iv max
      | iv <= max = do
         let v = VInt iv
         writeSTRef i (Evaluated 0 v)
         res <- case ki v of
                  EvalM f -> f gr k e mt d r msgs
         writeSTRef i s
         case res of
           Fail msg  msgs -> return (Fail msg msgs)
           Success r msgs -> bindInt gr k e mt d r msgs s (iv+1) max
      | otherwise = return (Success r msgs)


value2term flat xs (VApp q tnks) =
  foldM (\e1 tnk -> fmap (App e1) (tnk2term flat xs tnk)) (if fst q == cPredef then Q q else QC q) tnks
value2term flat xs (VMeta m vs) = do
  s <- getRef m
  case s of
    Evaluated _ v       -> do v <- apply v vs
                              value2term flat xs v
    Unevaluated env t   -> do v <- eval env t vs
                              value2term flat xs v
    Hole i              -> foldM (\e1 tnk -> fmap (App e1) (tnk2term flat xs tnk)) (Meta i) vs
    Residuation i _ ctr -> case ctr of
                             Just ctr -> value2term flat xs ctr
                             Nothing  -> foldM (\e1 tnk -> fmap (App e1) (tnk2term flat xs tnk)) (Meta i) vs
    Narrowing i _       -> foldM (\e1 tnk -> fmap (App e1) (tnk2term flat xs tnk)) (Meta i) vs
value2term flat xs (VSusp j k vs) = do
  v <- k (VGen maxBound vs)
  value2term flat xs v
value2term flat xs (VGen j tnks) =
  foldM (\e1 tnk -> fmap (App e1) (tnk2term flat xs tnk)) (Vr (reverse xs !! j)) tnks
value2term flat xs (VClosure env (Abs b x t)) = do
  tnk <- newEvaluatedThunk (VGen (length xs) [])
  v <- eval ((x,tnk):env) t []
  let x' = mkFreshVar xs x
  t <- value2term flat (x':xs) v
  return (Abs b x' t)
value2term flat xs (VProd b x v1 v2)
  | x == identW = do t1 <- value2term flat xs v1
                     v2 <- case v2 of
                             VClosure env t2 -> eval env t2 []
                             v2              -> return v2
                     t2 <- value2term flat xs v2
                     return (Prod b x t1 t2)
  | otherwise   = do t1 <- value2term flat xs v1
                     tnk <- newEvaluatedThunk (VGen (length xs) [])
                     v2 <- case v2 of
                             VClosure env t2 -> eval ((x,tnk):env) t2 []
                             v2              -> return v2
                     t2 <- value2term flat (x:xs) v2
                     return (Prod b (mkFreshVar xs x) t1 t2)
value2term flat xs (VRecType lbls) = do
  lbls <- mapM (\(lbl,v) -> fmap ((,) lbl) (value2term flat xs v)) lbls
  return (RecType lbls)
value2term flat xs (VR as) = do
  as <- mapM (\(lbl,tnk) -> fmap (\t -> (lbl,(Nothing,t))) (tnk2term flat xs tnk)) as
  return (R as)
value2term flat xs (VP v lbl tnks) = do
  t <- value2term flat xs v
  foldM (\e1 tnk -> fmap (App e1) (tnk2term flat xs tnk)) (P t lbl) tnks
value2term flat xs (VExtR v1 v2) = do
  t1 <- value2term flat xs v1
  t2 <- value2term flat xs v2
  return (ExtR t1 t2) 
value2term flat xs (VTable v1 v2) = do
  t1 <- value2term flat xs v1
  t2 <- value2term flat xs v2
  return (Table t1 t2)
value2term flat xs (VT vty env cs)= do
  ty <- value2term flat xs vty
  cs <- forM cs $ \(p,t) -> do
          (_,xs',env') <- pattVars (length xs,xs,env) p
          v <- eval env' t []
          t <- value2term flat xs' v
          return (p,t)
  return (T (TTyped ty) cs)
value2term flat xs (VV vty tnks)= do
  ty <- value2term flat xs vty
  ts <- mapM (tnk2term flat xs) tnks
  return (V ty ts)
value2term flat xs (VS v1 tnk2 tnks) = do
  t1 <- value2term flat xs v1
  t2 <- tnk2term flat xs tnk2
  foldM (\e1 tnk -> fmap (App e1) (tnk2term flat xs tnk)) (S t1 t2) tnks
value2term flat xs (VSort s) = return (Sort s)
value2term flat xs (VStr tok) = return (K tok)
value2term flat xs (VInt n) = return (EInt n)
value2term flat xs (VFlt n) = return (EFloat n)
value2term flat xs VEmpty = return Empty
value2term flat xs (VC v1 v2) = do
  t1 <- value2term flat xs v1
  t2 <- value2term flat xs v2
  return (C t1 t2)
value2term flat xs (VGlue v1 v2) = do
  t1 <- value2term flat xs v1
  t2 <- value2term flat xs v2
  return (Glue t1 t2)
value2term flat xs (VPatt min max p) = return (EPatt min max p)
value2term flat xs (VPattType v) = do
  t <- value2term flat xs v
  return (EPattType t)
value2term flat xs (VAlts vd vas) = do
  d <- value2term flat xs vd
  as <- forM vas $ \(vt,vs) -> do
           t <- value2term flat xs vt
           s <- value2term flat xs vs
           return (t,s)
  return (Alts d as)
value2term flat xs (VStrs vs) = do
  ts <- mapM (value2term flat xs) vs
  return (Strs ts)
value2term flat xs (VMarkup tag as vs) = do
  as <- mapM (\(id,v) -> value2term flat xs v >>= \t -> return (id,t)) as
  ts <- mapM (value2term flat xs) vs
  return (Markup tag as ts)
value2term flat xs (VCInts (Just i) Nothing) = return (App (Q (cPredef,cInts)) (EInt i))
value2term flat xs (VCInts Nothing (Just j)) = return (App (Q (cPredef,cInts)) (EInt j))
value2term flat xs (VCRecType lctrs) = do
  ltys <- mapM (\(l,o,ctr) -> value2term flat xs ctr >>= \ty -> return (l,ty)) lctrs
  return (RecType ltys)
value2term flat xs (VSymCat d r rs) = return (TSymCat d r [(i,(identW,ty)) | (i,(_,ty)) <- rs])
value2term flat xs v = error (showValue v)

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

instance Foldable ConstValue where
  foldr f a (Const x) = f x a
  foldr f a RunTime   = a
  foldr f a NonExist  = a

instance Traversable ConstValue where
  traverse f (Const x) = Const <$> f x
  traverse f RunTime   = pure RunTime
  traverse f NonExist  = pure NonExist

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
-- * Global/built-in definitions

type PredefImpl a s = [a] -> EvalM s (ConstValue (Value s))
newtype Predef a s = Predef { runPredef :: PredefImpl a s }
type PredefCombinator a b s = Predef a s -> Predef b s

infix 0 $\\

($\) :: PredefCombinator a b s -> PredefImpl a s -> Predef b s
k $\ f = k (Predef f)

pdForce :: PredefCombinator (Value s) (Thunk s) s
pdForce def = Predef $ \args -> do
  argValues <- mapM force args
  runPredef def argValues

pdClosedArgs :: PredefCombinator (Value s) (Value s) s
pdClosedArgs def = Predef $ \args -> do
  open <- anyM (isOpen []) args
  if open then return RunTime else runPredef def args

pdArity :: Int -> PredefCombinator (Thunk s) (Thunk s) s
pdArity n def = Predef $ \args ->
  case splitAt' n args of
    Nothing                  -> return RunTime
    Just (usedArgs, remArgs) -> do
      res <- runPredef def usedArgs
      forM res $ \v -> apply v remArgs

pdStandard :: Int -> PredefCombinator (Value s) (Thunk s) s
pdStandard n = pdArity n . pdForce . pdClosedArgs

-----------------------------------------------------------------------
-- * Evaluation monad

type MetaThunks s = Map.Map MetaId (Thunk s)
type Do s r = [Message] -> ST s (CheckResult r [Message])
type Cont s r = MetaThunks s -> Int -> r -> Do s r
type PredefTable s = Map.Map Ident (Predef (Thunk s) s)
data Globals = Gl Grammar (forall s . PredefTable s)
newtype EvalM s a = EvalM (forall r . Globals -> (a -> Cont s r) -> (Message -> Do s r) -> Cont s r)

instance Functor (EvalM s) where
  fmap f (EvalM g) = EvalM (\gr k e -> g gr (k . f) e)

instance Applicative (EvalM s) where
  pure x = EvalM (\gr k e -> k x)
  (EvalM f) <*> (EvalM x) = EvalM (\gr k e -> f gr (\f -> x gr (\x -> k (f x)) e) e)

instance Monad (EvalM s) where
  (EvalM f) >>= g = EvalM (\gr k e -> f gr (\x -> case g x of
                                                    EvalM g -> g gr k e) e)

instance Fail.MonadFail (EvalM s) where
  fail msg = EvalM (\gr k e _ _ r -> e (pp msg))

instance Alternative (EvalM s) where
  empty = EvalM (\gr k e _ _ r msgs -> return (Success r msgs))
  (EvalM f) <|> (EvalM g) = EvalM $ \gr k e mt b r msgs -> do
     res <- f gr k e mt b r msgs
     case res of
       Fail msg  msgs -> return (Fail msg msgs)
       Success r msgs -> g gr k e mt b r msgs

instance MonadPlus (EvalM s) where

runEvalM :: Globals -> (forall s . EvalM s a) -> Check [a]
runEvalM gr f = Check $ \(es,ws) ->
  case runST (case f of
                EvalM f -> f gr (\x mt _ xs ws -> return (Success (x:xs) ws)) (\msg ws -> return (Fail msg ws)) Map.empty maxBound [] ws) of
    Fail msg   ws -> Fail msg (es,ws)
    Success xs ws -> Success (reverse xs) (es,ws)

runEvalOneM :: Globals -> (forall s . EvalM s (Term,Type)) -> Check (Term,Type)
runEvalOneM gr f = Check $ \(es,ws) ->
  case runST (case f of
                EvalM f -> f gr (\x mt _ xs ws -> return (Success (x:xs) ws)) (\msg ws -> return (Fail msg ws)) Map.empty maxBound [] ws) of
    Fail msg      ws -> Fail msg (es,ws)
    Success []    ws -> Fail (pp "The evaluation produced no results") (es,ws)
    Success xs ws -> Success (FV (map fst xs),snd (head xs)) (es,ws)

reset :: EvalM s a -> EvalM s [a]
reset (EvalM f) = EvalM $ \gl k e mt d r ws -> do
  res <- f gl (\x mt d xs ws -> return (Success (x:xs) ws)) (\msg ws -> return (Fail msg ws)) mt d [] ws
  case res of
    Fail msg   ws -> e msg ws
    Success xs ws -> k (reverse xs) mt d r ws

try :: EvalM s a -> EvalM s a -> EvalM s a
try (EvalM f) (EvalM g) = EvalM (\gl k e mt d r ws -> f gl k (\msg _ -> g gl k e mt d r ws) mt d r ws)

evalError :: Message -> EvalM s a
evalError msg = EvalM (\gr k e _ _ r ws -> e msg ws)
  
evalWarn :: Message -> EvalM s ()
evalWarn msg = EvalM (\gr k e mt d r msgs -> k () mt d r (msg:msgs))

evalPredef :: Ident -> [Thunk s] -> EvalM s (Value s)
evalPredef id args = do
  res <- EvalM $ \globals@(Gl _ predef) k e mt d r msgs ->
    case Map.lookup id predef <&> \def -> runPredef def args of
      Just (EvalM f) -> f globals k e mt d r msgs
      Nothing        -> k RunTime mt d r msgs
  case res of
    Const res -> return res
    RunTime   -> return $ VApp (cPredef,id) args
    NonExist  -> return $ VApp (cPredef,cNonExist) []

getResDef :: QIdent -> EvalM s Term
getResDef q = EvalM $ \(Gl gr _) k e mt d r msgs -> do
  case lookupResDef gr q of
    Ok t    -> k t mt d r msgs
    Bad msg -> e (pp msg) msgs

getInfo :: QIdent -> EvalM s (ModuleName,Info)
getInfo q = EvalM $ \(Gl gr _) k e mt d r msgs -> do
  case lookupOrigInfo gr q of
    Ok res  -> k res mt d r msgs
    Bad msg -> e (pp msg) msgs

getResType :: QIdent -> EvalM s Type
getResType q = EvalM $ \(Gl gr _) k e mt d r msgs -> do
  case lookupResType gr q of
    Ok t    -> k t mt d r msgs
    Bad msg -> e (pp msg) msgs

getOverload :: Term -> QIdent -> EvalM s (Term,Type)
getOverload t q = EvalM $ \(Gl gr _) k e mt d r msgs -> do
  case lookupOverloadTypes gr q of
    Ok ttys -> let err = "Overload resolution failed" $$
                         "of term   " <+> pp t $$
                         "with types" <+> vcat [ppTerm Terse 0 ty | (_,ty) <- ttys]

                   go r []         = return (Success r msgs)
                   go r (tty:ttys) = do res <- k tty mt d r msgs
                                        case res of
                                          Fail _ _       -> go r ttys
                                          Success r msgs -> go r ttys

               in go r ttys
    Bad msg -> e (pp msg) msgs

getAllParamValues :: Type -> EvalM s [Term]
getAllParamValues ty = EvalM $ \(Gl gr _) k e mt d r msgs ->
  case allParamValues gr ty of
    Ok ts   -> k ts mt d r msgs
    Bad msg -> e (pp msg) msgs

newThunk env t = EvalM $ \gr k e mt d r msgs -> do
 tnk <- newSTRef (Unevaluated env t)
 k tnk mt d r msgs

newEvaluatedThunk v = EvalM $ \gr k e mt d r msgs -> do
 tnk <- newSTRef (Evaluated maxBound v)
 k tnk mt d r msgs

newHole i = EvalM $ \gr k e mt d r msgs ->
  if i == 0
    then do tnk <- newSTRef (Hole i)
            k tnk mt d r msgs
    else case Map.lookup i mt of
           Just tnk -> k tnk mt d r msgs
           Nothing  -> do tnk <- newSTRef (Hole i)
                          k tnk (Map.insert i tnk mt) d r msgs

newResiduation scope = EvalM $ \gr k e mt d r msgs -> do
  let i = Map.size mt + 1
  tnk <- newSTRef (Residuation i scope Nothing)
  k (i,tnk) (Map.insert i tnk mt) d r msgs

newNarrowing ty = EvalM $ \gr k e mt d r msgs -> do
  let i = Map.size mt + 1
  tnk <- newSTRef (Narrowing i ty)
  k (i,tnk) (Map.insert i tnk mt) d r msgs

withVar d0 (EvalM f) = EvalM $ \gr k e mt d1 r msgs ->
                                  let !d = min d0 d1
                                  in f gr k e mt d r msgs

getVariables :: EvalM s [(LVar,LIndex)]
getVariables = EvalM $ \(Gl gr _) k e mt d ws r -> do
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

getRef tnk = EvalM $ \gr k e mt d r msgs -> readSTRef tnk >>= \st -> k st mt d r msgs
setRef tnk st = EvalM $ \gr k e mt d r msgs -> do
  old <- readSTRef tnk
  writeSTRef tnk st
  res <- k () mt d r msgs
  writeSTRef tnk old
  return res

force tnk = EvalM $ \gr k e mt d r msgs -> do
  s <- readSTRef tnk
  case s of
    Unevaluated env t -> case eval env t [] of
                           EvalM f -> f gr (\v mt b r msgs -> do let d = length env
                                                                 writeSTRef tnk (Evaluated d v)
                                                                 r <- k v mt d r msgs
                                                                 writeSTRef tnk s
                                                                 return r) e mt d r msgs
    Evaluated d v     -> k v mt d r msgs
    Hole _            -> k (VMeta tnk []) mt d r msgs
    Residuation _ _ _ -> k (VMeta tnk []) mt d r msgs
    Narrowing _ _     -> k (VMeta tnk []) mt d r msgs

tnk2term True  xs tnk = force tnk >>= value2term True xs
tnk2term False xs tnk = EvalM $ \gr k e mt d r msgs ->
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

      err msg msgs = return (Fail msg msgs)

  in do s <- readSTRef tnk
        case s of
          Unevaluated env t -> do let d0 = length env
                                  res <- case eval env t [] of
                                           EvalM f -> f gr (\v mt d msgs r -> do writeSTRef tnk (Evaluated d0 v)
                                                                                 r <- case value2term False xs v of
                                                                                        EvalM f -> f gr (acc d0) err mt d msgs r
                                                                                 writeSTRef tnk s
                                                                                 return r) err mt maxBound (r,0,[]) msgs
                                  case res of
                                    Fail msg msgs         -> return (Fail msg msgs)
                                    Success (r,0,xs) msgs -> k (FV []) mt d r msgs
                                    Success (r,c,xs) msgs -> flush xs (\mt msgs r -> return (Success msgs r)) mt r msgs
          Evaluated d0 v    -> do res <- case value2term False xs v of
                                           EvalM f -> f gr (acc d0) err mt maxBound (r,0,[]) msgs
                                  case res of
                                    Fail msg msgs         -> return (Fail msg msgs)
                                    Success (r,0,xs) msgs -> k (FV []) mt d r msgs
                                    Success (r,c,xs) msgs -> flush xs (\mt r msgs -> return (Success r msgs)) mt r msgs
          Hole i            -> k (Meta i) mt d r msgs
          Residuation i _ _ -> k (Meta i) mt d r msgs
          Narrowing i _     -> k (Meta i) mt d r msgs

scopeEnv   scope = zipWithM (\x i -> newEvaluatedThunk (VGen i []) >>= \tnk -> return (x,tnk)) (reverse scope) [0..]


unsafeIOToEvalM :: IO a -> EvalM s a
unsafeIOToEvalM f = EvalM (\gr k e mt d r msgs -> unsafeIOToST f >>= \x -> k x mt d r msgs)

