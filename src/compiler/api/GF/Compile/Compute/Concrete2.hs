{-# LANGUAGE RankNTypes, BangPatterns, GeneralizedNewtypeDeriving #-}

module GF.Compile.Compute.Concrete2
           (Env, Scope, Value(..), Constraint, ConstValue(..), Globals(..), PredefTable, EvalM,
            runEvalM, stdPredef, globals, pdArity,
            normalForm, normalFlatForm,
            eval, apply, value2term, value2termM, patternMatch, vtableSelect,
            newResiduation, getMeta, setMeta, MetaState(..), variants, try,
            evalError, evalWarn, ppValue, Choice, unit, split, split4, mapC, mapCM) where

import Prelude hiding ((<>)) -- GHC 8.4.1 clash with Text.PrettyPrint
import GF.Infra.Ident
import GF.Infra.CheckM
import GF.Data.Operations(Err(..))
import GF.Data.Utilities(splitAt',(<||>),anyM)
import GF.Grammar.Lookup(lookupResDef,lookupOrigInfo)
import GF.Grammar.Grammar
import GF.Grammar.Macros
import GF.Grammar.Predef
import GF.Grammar.Printer hiding (ppValue)
import GF.Grammar.Lockfield(lockLabel)
import GF.Text.Pretty hiding (empty)
import Control.Monad
import Control.Applicative hiding (Const)
import qualified Control.Applicative as A
import qualified Data.Map as Map
import Data.Maybe (fromMaybe,fromJust)
import Data.List
import Data.Char
import Debug.Trace

type Env  = [(Ident,Value)]
type Scope = [(Ident,Value)]
type Predef a = Globals -> Choice -> [Value] -> ConstValue a
type PredefCombinator a = Predef a -> Predef a
type PredefTable = Map.Map Ident (Predef Value)
data Globals = Gl Grammar PredefTable

data Value
  = VApp QIdent [Value]
  | VMeta {-# UNPACK #-} !MetaId [Value]
  | VSusp {-# UNPACK #-} !MetaId (Value -> Value) [Value]
  | VGen  {-# UNPACK #-} !Int [Value]
  | VClosure Env Choice Term
  | VProd BindType Ident Value Value
  | VRecType [(Label, Value)]
  | VR [(Label, Value)]
  | VP Value Label [Value]
  | VExtR Value Value
  | VTable Value Value
  | VT Value Env Choice [Case]
  | VV Value [Value]
  | VS Value Value [Value]
  | VSort Ident
  | VInt Integer
  | VFlt Double
  | VStr String
  | VEmpty
  | VC Value Value
  | VGlue Value Value
  | VPatt Int (Maybe Int) Patt
  | VPattType Value
  | VFV Choice [Value]
  | VAlts Value [(Value, Value)]
  | VStrs [Value]
  | VMarkup Ident [(Ident,Value)] [Value]
  | VSymCat Int LIndex [(LIndex, (Value, Type))]
  | VError Doc
    -- These two constructors are only used internally
    -- in the type checker.
  | VCRecType [(Label, Bool, Value)]
  | VCInts (Maybe Integer) (Maybe Integer)

data ConstValue a
  = Const a
  | CSusp MetaId (Value -> ConstValue a)
  | CFV Choice [ConstValue a]
  | RunTime
  | NonExist

instance Functor ConstValue where
  fmap f (Const c) = Const (f c)
  fmap f (CFV i vs) = CFV i (map (fmap f) vs)
  fmap f (CSusp i k) = CSusp i (fmap f . k)
  fmap f RunTime   = RunTime
  fmap f NonExist  = NonExist

instance Applicative ConstValue where
  pure = Const

  (Const f)     <*> (Const x)     = Const (f x)
  (CFV s vs)    <*> v2            = CFV s [v1 <*> v2 | v1 <- vs]
  v1            <*> (CFV s vs)    = CFV s [v1 <*> v2 | v2 <- vs]
  (CSusp i k)   <*> v2            = CSusp i (\v -> k v <*> v2)
  v1            <*> (CSusp i k)   = CSusp i (\v -> v1 <*> k v)
  NonExist      <*> _             = NonExist
  _             <*> NonExist      = NonExist
  RunTime       <*>  _            = RunTime
  _             <*>  RunTime      = RunTime

normalForm :: Globals -> Term -> Check Term
normalForm g t = value2term g [] (bubble (eval g [] unit t []))

normalFlatForm :: Globals -> Term -> Check [Term]
normalFlatForm g t = runEvalM g (value2termM True [] (eval g [] unit t []))

eval :: Globals -> Env -> Choice -> Term -> [Value] -> Value
eval g env s (Vr x)         vs  = case lookup x env of
                                    Nothing -> VError ("Variable" <+> pp x <+> "is not in scope")
                                    Just v  -> apply g v vs
eval g env s (Sort sort)    []
  | sort == cTok                = VSort cStr
  | otherwise                   = VSort sort
eval g env s (EInt n)       []  = VInt n
eval g env s (EFloat d)     []  = VFlt d
eval g env s (K t)          []  = VStr t
eval g env s Empty          []  = VEmpty
eval g env s (App t1 t2)    vs  = let (s1,s2) = split s
                                  in eval g env s1 t1 (eval g env s2 t2 [] : vs)
eval g env s (Abs b x t)    []  = VClosure env s (Abs b x t)
eval g env s (Abs b x t) (v:vs) = eval g ((x,v):env) s t vs
eval g env s (Meta i)       vs  = VMeta i vs
eval g env s (ImplArg t)    []  = eval g env s t []
eval g env s (Prod b x t1 t2)[] = let (s1,s2) = split s
                                  in VProd b x (eval g env s1 t1 []) (VClosure env s2 t2)
eval g env s (Typed t ty)   vs  = eval g env s t vs
eval g env s (RecType lbls) []  = VRecType (mapC (\s (lbl,ty) -> (lbl, eval g env s ty [])) s lbls)
eval g env s (R as)         []  = VR (mapC (\s (lbl,(ty,t)) -> (lbl, eval g env s t [])) s as)
eval g env s (P t lbl)      vs  = let project (VR as)        = case lookup lbl as of
                                                                 Nothing -> VError ("Missing value for label" <+> pp lbl $$
                                                                                    "in" <+> pp (P t lbl))
                                                                 Just v  -> apply g v vs
                                      project (VFV s fvs)    = VFV s (map project fvs)
                                      project (VMeta i vs)   = VSusp i (\v -> project (apply g v vs)) []
                                      project (VSusp i k vs) = VSusp i (\v -> project (apply g (k v) vs)) []
                                      project v              = VP v lbl vs
                                  in project (eval g env s t [])
eval g env s (ExtR t1 t2)   []  = let (s1,s2) = split s

                                      extend (VR       as1) (VR       as2)   = VR       (foldl (\as (lbl,v) -> update lbl v as) as1 as2)
                                      extend (VRecType as1) (VRecType as2)   = VRecType (foldl (\as (lbl,v) -> update lbl v as) as1 as2)
                                      extend (VFV i fvs)    v2               = VFV i [extend v1 v2 | v1 <- fvs]
                                      extend v1             (VFV i fvs)      = VFV i [extend v1 v2 | v2 <- fvs]
                                      extend (VMeta i vs)   v2               = VSusp i (\v -> extend (apply g v vs) v2) []
                                      extend v1             (VMeta i vs)     = VSusp i (\v -> extend v1 (apply g v vs)) []
                                      extend (VSusp i k vs) v2               = VSusp i (\v -> extend (apply g (k v) vs) v2) []
                                      extend v1             (VSusp i k vs)   = VSusp i (\v -> extend v1 (apply g (k v) vs)) []
                                      extend v1             v2               = VExtR v1 v2

                                  in extend (eval g env s1 t1 []) (eval g env s2 t2 [])
eval g env s (Table t1 t2)  []  = let (!s1,!s2) = split s
                                  in VTable (eval g env s1 t1 []) (eval g env s2 t2 [])
eval g env s (T (TTyped ty) cs)[]=let (!s1,!s2) = split s
                                  in VT (eval g env s1 ty []) env s2 cs
eval g env s (T (TWild ty) cs) []=let (!s1,!s2) = split s
                                  in VT (eval g env s1 ty []) env s2 cs
eval g env s (V ty ts)      []  = let (!s1,!s2) = split s
                                  in VV (eval g env s1 ty []) (mapC (\s t -> eval g env s t []) s2 ts)
eval g env s (S t1 t2)      vs  = let (!s1,!s2) = split s
                                      v1 = eval g env s1 t1 []
                                      v2 = eval g env s2 t2 []
                                      v0 = VS v1 v2 vs

                                      select (VT _  env s cs) = patternMatch g s v0 (map (\(p,t) -> (env,[p],v2:vs,t)) cs)
                                      select (VV vty tvs)     = case value2termM False (map fst env) vty of
                                                                  EvalM f -> case f g (\x state xs ws -> Success (x:xs) ws) empty [] [] of
                                                                               Fail   msg  ws -> VError msg
                                                                               Success tys ws -> case tys of
                                                                                                   [ty] -> vtableSelect g v0 ty tvs v2 vs
                                                                                                   tys  -> vtableSelect g v0 (FV (reverse tys)) tvs v2 vs
                                      select (VFV i fvs)      = VFV i [select v1 | v1 <- fvs]
                                      select (VMeta i vs)     = VSusp i (\v -> select (apply g v vs)) []
                                      select (VSusp i k vs)   = VSusp i (\v -> select (apply g (k v) vs)) []
                                      select v1               = v0
                                          
                                      empty = State Map.empty Map.empty

                                in select v1
eval g env s (Let (x,(_,t1)) t2) vs = let (!s1,!s2) = split s
                                      in eval g ((x,eval g env s1 t1 []):env) s2 t2 vs
eval g env c (Q q@(m,id))  vs
  | m == cPredef              = case Map.lookup id predef of
                                  Nothing -> VApp q vs
                                  Just fn -> let valueOf (Const res) = res
                                                 valueOf (CFV i vs)  = VFV i (map valueOf vs)
                                                 valueOf (CSusp i k) = VSusp i (valueOf . k) []
                                                 valueOf RunTime     = VApp q vs
                                                 valueOf NonExist    = VApp (cPredef,cNonExist) []
                                             in valueOf (fn g c vs)
  | otherwise                 = case lookupResDef gr q of
                                  Ok t    -> eval g env c t vs
                                  Bad msg -> error msg
  where
    Gl gr predef = g
eval g env s (QC q)         vs  = VApp q vs
eval g env s (C t1 t2)      []  = let (!s1,!s2) = split s

                                      concat v1           VEmpty = v1
                                      concat VEmpty           v2 = v2
                                      concat (VFV i fvs)      v2 = VFV i [concat v1 v2 | v1 <- fvs]
                                      concat v1      (VFV i fvs) = VFV i [concat v1 v2 | v2 <- fvs]
                                      concat (VMeta i vs)     v2 = VSusp i (\v -> concat (apply g v vs) v2) []
                                      concat v1     (VMeta i vs) = VSusp i (\v -> concat v1 (apply g v vs)) []
                                      concat (VSusp i k vs)   v2 = VSusp i (\v -> concat (apply g (k v) vs) v2) []
                                      concat v1   (VSusp i k vs) = VSusp i (\v -> concat v1 (apply g (k v) vs)) []
                                      concat v1               v2 = VC v1 v2

                                  in concat (eval g env s1 t1 []) (eval g env s2 t2 [])
eval g env s (Glue t1 t2)   []  = let (!s1,!s2) = split s

                                      glue VEmpty        v             = v
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
                                      glue (VFV i fvs)   v2            = VFV i [glue v1 v2 | v1 <- fvs]
                                      glue v1            (VFV i fvs)   = VFV i [glue v1 v2 | v2 <- fvs]
                                      glue (VMeta i vs)  v2            = VSusp i (\v -> glue (apply g v vs) v2) []
                                      glue v1            (VMeta i vs)  = VSusp i (\v -> glue v1 (apply g v vs)) []
                                      glue (VSusp i k vs) v2           = VSusp i (\v -> glue (apply g (k v) vs) v2) []
                                      glue v1            (VSusp i k vs)= VSusp i (\v -> glue v1 (apply g (k v) vs)) []
                                      glue v1            v2            = VGlue v1 v2

                                      pre vd []                 s              = glue vd (VStr s)
                                      pre vd ((v,VStrs ss):vas) s
                                        | or [startsWith s' s | VStr s' <- ss] = glue v (VStr s)
                                        | otherwise                            = pre vd vas s

                                  in glue (eval g env s1 t1 []) (eval g env s2 t2 [])
eval g env s (EPatt min max p) [] = VPatt min max p
eval g env s (EPattType t)  []  = VPattType (eval g env s t [])
eval g env s (ELincat c ty) []  = let lbl = lockLabel c
                                      lty = RecType []
                                  in eval g env s (ExtR ty (RecType [(lbl,lty)])) []
eval g env s (ELin c t)     []  = let lbl = lockLabel c
                                      lt  = R []
                                  in eval g env s (ExtR t (R [(lbl,(Nothing,lt))])) []
eval g env s (FV ts)        vs  = VFV s (mapC (\s t -> eval g env s t vs) s ts)
eval g env s (Alts d as)    []  = let (!s1,!s2) = split s
                                      vd  = eval g env s1 d []
                                      vas = mapC (\s (t1,t2) -> let (!s1,!s2) = split s
                                                              in (eval g env s1 t1 [],eval g env s2 t2 [])) s2 as
                                  in VAlts vd vas
eval g env c (Strs ts)      []  = VStrs (mapC (\c t -> eval g env c t []) c ts)
eval g env c (Markup tag as ts) [] =
                              let (c1,c2) = split c
                                  vas = mapC (\c (id,t) -> (id,eval g env c t [])) c1 as
                                  vs  = mapC (\c t -> eval g env c t []) c2 ts
                              in (VMarkup tag vas vs)
eval g env c (Reset ctl t) [] =
                              let limit All       = id
                                  limit (Limit n) = fmap (genericTake n)
                              in (VMarkup identW [] [eval g env c t []])
eval g env c (TSymCat d r rs) []= VSymCat d r [(i,(fromJust (lookup pv env),ty)) | (i,(pv,ty)) <- rs]
eval g env c t              vs  = VError ("Cannot reduce term" <+> pp t)

stdPredef :: Globals -> PredefTable
stdPredef g = Map.fromList
  [(cLength, pdArity 1 $ \g c [v] -> fmap (VInt . genericLength) (value2string g v))
  ,(cTake,   pdArity 2 $ \g c [v1,v2] -> fmap string2value (liftA2 genericTake (value2int g v1) (value2string g v2)))
  ,(cDrop,   pdArity 2 $ \g c [v1,v2] -> fmap string2value (liftA2 genericDrop (value2int g v1) (value2string g v2)))
  ,(cTk,     pdArity 2 $ \g c [v1,v2] -> fmap string2value (liftA2 genericTk (value2int g v1) (value2string g v2)))
  ,(cDp,     pdArity 2 $ \g c [v1,v2] -> fmap string2value (liftA2 genericDp (value2int g v1) (value2string g v2)))
  ,(cIsUpper,pdArity 1 $ \g c [v]     -> fmap toPBool (liftA (all isUpper) (value2string g v)))
  ,(cToUpper,pdArity 1 $ \g c [v]     -> fmap string2value (liftA (map toUpper) (value2string g v)))
  ,(cToLower,pdArity 1 $ \g c [v]     -> fmap string2value (liftA (map toLower) (value2string g v)))
  ,(cEqStr,  pdArity 2 $ \g c [v1,v2] -> fmap toPBool (liftA2 (==) (value2string g v1) (value2string g v2)))
  ,(cOccur,  pdArity 2 $ \g c [v1,v2] -> fmap toPBool (liftA2 occur (value2string g v1) (value2string g v2)))
  ,(cOccurs, pdArity 2 $ \g c [v1,v2] -> fmap toPBool (liftA2 occurs (value2string g v1) (value2string g v2)))
  ,(cEqInt,  pdArity 2 $ \g c [v1,v2] -> fmap toPBool (liftA2 (==) (value2int g v1) (value2int g v2)))
  ,(cLessInt,pdArity 2 $ \g c [v1,v2] -> fmap toPBool (liftA2 (<) (value2int g v1) (value2int g v2)))
  ,(cPlus,   pdArity 2 $ \g c [v1,v2] -> fmap VInt (liftA2 (+) (value2int g v1) (value2int g v2)))
  ,(cError,  pdArity 1 $ \g c [v]     -> fmap (VError . pp) (value2string g v))
  ]
  where
    genericTk n = reverse . genericDrop n . reverse
    genericDp n = reverse . genericTake n . reverse

apply g (VMeta i vs0)                   vs  = VMeta i   (vs0++vs)
apply g (VSusp i k vs0)                 vs  = VSusp i k (vs0++vs)
apply g (VApp f  vs0)                   vs  = VApp f (vs0++vs)
apply g (VGen i  vs0)                   vs  = VGen i (vs0++vs)
apply g (VFV i fvs)                     vs  = VFV i [apply g v vs | v <- fvs]
apply g (VClosure env s (Abs b x t)) (v:vs) = eval g ((x,v):env) s t vs
apply g v                               []  = v

bubble v = snd (bubble v)
  where
    bubble (VApp f vs) = liftL (VApp f) vs
    bubble (VMeta metaid vs) = liftL (VMeta metaid) vs
    bubble (VSusp metaid k vs) = liftL (VSusp metaid k) vs
    bubble (VGen i vs) = liftL (VGen i) vs
    bubble (VClosure env c t) = liftL' (\env -> VClosure env c t) env
    bubble (VProd bt x v1 v2) = lift2 (VProd bt x) v1 v2
    bubble (VRecType as) = liftL' VRecType as
    bubble (VR as) = liftL' VR as
    bubble (VP v l vs) = lift1L (\v vs -> VP v l vs) v vs
    bubble (VExtR v1 v2) = lift2 VExtR v1 v2
    bubble (VTable v1 v2) = lift2 VTable v1 v2
    bubble (VT v env c cs) = lift1L' (\v env -> VT v env c cs) v env
    bubble (VV v vs) = lift1L VV v vs
    bubble (VS v1 v2 vs) = lift2L VS v1 v2 vs
    bubble v@(VSort _) = lift0 v
    bubble v@(VInt _) = lift0 v
    bubble v@(VFlt _) = lift0 v
    bubble v@(VStr _) = lift0 v
    bubble v@VEmpty = lift0 v
    bubble (VC v1 v2) = lift2 VC v1 v2
    bubble (VGlue v1 v2) = lift2 VGlue v1 v2
    bubble v@(VPatt _ _ _) = lift0 v
    bubble (VPattType v) = lift1 VPattType v
    bubble (VFV c vs) =
      let (union,vs') = mapAccumL descend Map.empty vs
      in (Map.insert c (length vs,1) union, addVariants (VFV c vs') union)
    bubble (VAlts v vs) = lift1L2 VAlts v vs
    bubble (VStrs vs) = liftL VStrs vs
    bubble (VSymCat d i0 vs) =
      let (union,vs') = mapAccumL descendC Map.empty vs
      in (union, addVariants (VSymCat d i0 vs') union)
    bubble v@(VError _) = lift0 v
    bubble v@(VCRecType lbls) =
      let (union,lbls') = mapAccumL descendR Map.empty lbls
      in (union, addVariants (VCRecType lbls') union)
    bubble v@(VCInts _ _) = lift0 v

    lift0 v = (Map.empty, v)

    lift1 f v =
      let (union,v') = bubble v
      in (union,f v')

    liftL f vs =
      let (union,vs') = mapAccumL descend Map.empty vs
      in (union, addVariants (f vs') union)

    liftL' f vs =
      let (union,vs') = mapAccumL descend' Map.empty vs
      in (union, addVariants (f vs') union)

    lift1L f v vs =
      let (choices,v') = bubble v
          (union, vs') = mapAccumL descend (unitfy choices) vs
      in (union, addVariants (f v' vs') union)

    lift1L' f v vs =
      let (choices,v') = bubble v
          (union, vs') = mapAccumL descend' (unitfy choices) vs
      in (union, addVariants (f v' vs') union)

    lift1L2 f v vs =
      let (choices,v') = bubble v
          (union, vs') = mapAccumL descend2 (unitfy choices) vs
      in (union, addVariants (f v' vs') union)

    lift2L f v1 v2 vs =
      let (choices1,v1') = bubble v1
          (choices2,v2') = bubble v2
          union = mergeChoices2 choices1 choices2
          (union', vs') = mapAccumL descend union vs
      in (union', addVariants (f v1' v2' vs') union')

    lift2 f v1 v2 =
      let (choices1,v1') = bubble v1
          (choices2,v2') = bubble v2
          union = mergeChoices2 choices1 choices2
      in (union, addVariants (f v1' v2') union)

    descend union v =
      let (choices,v') = bubble v
      in (mergeChoices1 union choices,v')

    descend' :: Map.Map Choice (Int,Int) -> (a,Value) -> (Map.Map Choice (Int,Int),(a,Value))
    descend' union (x,v) =
      let (choices,v') = bubble v
      in (mergeChoices1 union choices,(x,v'))

    descend2 union (v1,v2) =
      let (choices1,v1') = bubble v1
          (choices2,v2') = bubble v2
      in (mergeChoices1 (mergeChoices1 union choices1) choices2,(v1',v2'))

    descendC union (i,(v,ty)) =
      let (choices,v') = bubble v
      in (mergeChoices1 union choices,(i,(v',ty)))

    descendR union (l,b,v) =
      let (choices,v') = bubble v
      in (mergeChoices1 union choices,(l,b,v'))

    addVariants v = Map.foldrWithKey addVariant v
      where
        addVariant c (n,cnt) v
          | cnt > 1   = VFV c (replicate n v)
          | otherwise = v

    unitfy = fmap (\(n,_) -> (n,1))
    mergeChoices1 = Map.mergeWithKey (\c (n,cnt) _ -> Just (n,cnt+1)) id unitfy
    mergeChoices2 = Map.mergeWithKey (\c (n,cnt) _ -> Just (n,2)) unitfy unitfy

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

patternMatch g s v0 []                      = v0
patternMatch g s v0 ((env0,ps,args0,t):eqs) = match env0 ps eqs args0
  where
    match env []              eqs      args  = eval g env s t args
    match env (PT ty p   :ps) eqs      args  = match env (p:ps) eqs args
    match env (PAlt p1 p2:ps) eqs      args  = match env (p1:ps) ((env,p2:ps,args,t):eqs) args
    match env (PM q      :ps) eqs      args  = case lookupResDef gr q of
                                                 Ok t    -> case eval g [] unit t [] of
                                                              VPatt _ _ p -> match env (p:ps) eqs args
                                                              _ -> error $ render (hang "Expected pattern macro:" 4
                                                                                        (pp t))
                                                 Bad msg -> error msg
                                               where
                                                 Gl gr _ = g
    match env (PV v      :ps) eqs (arg:args) = match ((v,arg):env) ps eqs args
    match env (PAs v p   :ps) eqs (arg:args) = match ((v,arg):env) (p:ps) eqs (arg:args)
    match env (PW        :ps) eqs (arg:args) = match env ps eqs args
    match env (PTilde _  :ps) eqs (arg:args) = match env ps eqs args
    match env (p         :ps) eqs (arg:args) = match' env p ps eqs arg args

    match' env p ps eqs arg args =
      case (p,arg) of
        (p,       VMeta i   vs) -> VSusp i (\v -> match' env p ps eqs (apply g v vs) args) []
        (p,     VGen    i   vs) -> v0
        (p,       VSusp i k vs) -> VSusp i (\v -> match' env p ps eqs (apply g (k v) vs) args) []
        (p,           VFV s vs) -> VFV s [match' env p ps eqs arg args | arg <- vs]
        (PP q qs,    VApp r vs)
          | q == r              -> match env (qs++ps) eqs (vs++args)
        (PR pas,  VR as)        -> matchRec env (reverse pas) as ps eqs args
        (PString s1, VStr s2)
          | s1 == s2            -> match env ps eqs args
        (PString s1, VEmpty)
          | null s1             -> match env ps eqs args
        (PSeq min1 max1 p1 min2 max2 p2,v)
                          -> case value2string g v of
                               Const str -> let n  = length str
                                                lo = min1 `max` (n-fromMaybe n max2)
                                                hi = (n-min2) `min` fromMaybe n max1
                                                (ds,cs) = splitAt lo str

                                                eqs' = matchStr env (p1:p2:ps) eqs (hi-lo) (reverse ds) cs args

                                            in patternMatch g s v0 eqs'
                               RunTime   -> v0
                               NonExist  -> patternMatch g s v0 eqs
        (PRep minp maxp p, v)
                          -> case value2string g v of
                               Const str -> let n = length (str::String) `div` (max minp 1)
                                                eqs' = matchRep env n minp maxp p minp maxp p ps ((env,PString []:ps,(arg:args),t) : eqs) (arg:args)
                                            in patternMatch g s v0 eqs'
                               RunTime   -> v0
                               NonExist  -> patternMatch g s v0 eqs
        (PChar, VStr [_]) -> match env ps eqs args
        (PChars cs, VStr [c])
          | elem c cs     -> match env ps eqs args
        (PInt n, VInt m)
          | n == m        -> match env ps eqs args
        (PFloat n, VFlt m)
          | n == m        -> match env ps eqs args
        _                 -> patternMatch g s v0 eqs

    matchRec env []            as ps eqs args = match env ps eqs args
    matchRec env ((lbl,p):pas) as ps eqs args =
      case lookup lbl as of
        Just tnk -> matchRec env pas as (p:ps) eqs (tnk:args)
        Nothing  -> VError ("Missing value for label" <+> pp lbl)

    matchStr env ps eqs i ds []     args =
      (env,ps,(string2value (reverse ds)):(string2value []):args,t) : eqs
    matchStr env ps eqs 0 ds cs     args =
      (env,ps,(string2value (reverse ds)):(string2value cs):args,t) : eqs
    matchStr env ps eqs i ds (c:cs) args =
      (env,ps,(string2value (reverse ds)):(string2value (c:cs)):args,t) :
      matchStr env ps eqs (i-1 :: Int) (c:ds) cs args

    matchRep env 0 minp maxp p minq maxq q ps eqs args = eqs
    matchRep env n minp maxp p minq maxq q ps eqs args =
      matchRep env (n-1) minp maxp p (minp+minq) (liftM2 (+) maxp maxq) (PSeq minp maxp p minq maxq q) ps ((env,q:ps,args,t) : eqs) args

vtableSelect g v0 ty cs v2 vs =
  apply g (select (value2index v2 ty)) vs
  where
    select (Const (i,_)) = cs !! i
    select (CSusp i k)   = VSusp i (\v -> select (k v)) []
    select (CFV s vs)    = VFV s (map select vs)
    select _             = VError ("the parameter:" <+> ppValue Unqualified 0 v2 $$
                                   "cannot be evaluated at compile time.")

    value2index (VMeta i vs)      ty = CSusp i (\v -> value2index (apply g v vs) ty)
    value2index (VSusp i k vs)    ty = CSusp i (\v -> value2index (apply g (k v) vs) ty)
    value2index (VR as) (RecType lbls) = compute lbls
      where
        compute []              = pure (0,1)
        compute ((lbl,ty):lbls) =
          case lookup lbl as of
            Just v  -> liftA2 (\(r, cnt) (r',cnt') -> (r*cnt'+r',cnt*cnt'))
                              (value2index v ty)
                              (compute lbls)
            Nothing -> error (show ("Missing value for label" <+> pp lbl $$
                                    "among" <+> hsep (punctuate (pp ',') (map fst as))))
    value2index (VApp q tnks) ty =
      let (r ,ctxt,cnt ) = getIdxCnt q
      in fmap (\(r', cnt') -> (r+r',cnt)) (compute ctxt tnks)
      where
        getIdxCnt q =
          let (_,ResValue (L _ ty) idx) = getInfo q
              (ctxt,QC p) = typeFormCnc ty
              (_,ResParam _ (Just (_,cnt))) = getInfo p
          in (idx,ctxt,cnt)

        compute []              []     = pure (0,1)
        compute ((_,_,ty):ctxt) (v:vs) =
          liftA2 (\(r, cnt) (r',cnt') -> (r*cnt'+r',cnt*cnt'))
                 (value2index v ty)
                 (compute ctxt vs)

        getInfo :: QIdent -> (ModuleName,Info)
        getInfo q =
          case lookupOrigInfo gr q of
            Ok res  -> res
            Bad msg -> error msg

        Gl gr _ = g
    value2index (VInt n)          ty
      | Just max <- isTypeInts ty    = Const (fromIntegral n,fromIntegral max+1)
    value2index (VFV i vs)        ty = CFV i [value2index v ty | v <- vs]
    value2index v ty = RunTime


value2term :: Globals -> [Ident] -> Value -> Check Term
value2term g xs v = do
  res <- runEvalM g (value2termM False xs v)
  case res of
    [t] -> return t
    ts  -> return (FV ts)

type Constraint = Value
data MetaState
  = Bound Scope Value
  | Narrowing   Type
  | Residuation Scope (Maybe Constraint)
data State
  = State 
      { choices  :: Map.Map Choice Int
      , metaVars :: Map.Map MetaId MetaState
      }
type Cont r = State -> r -> [Message] -> CheckResult r [Message]
newtype EvalM a = EvalM (forall r . Globals -> (a -> Cont r) -> Cont r)

instance Functor EvalM where
  fmap f (EvalM m) = EvalM (\g k -> m g (k . f))

instance Applicative EvalM where
  pure x = EvalM (\g k -> k x)
  (EvalM f) <*> (EvalM h) = EvalM (\g k -> f g (\fn -> h g (\x -> k (fn x))))

instance Alternative EvalM where
  empty = EvalM (\g k _ r msgs -> Success r msgs)
  (EvalM f) <|> (EvalM g) = EvalM $ \gl k state r msgs ->
     case f gl k state r msgs of
       Fail msg  msgs -> Fail msg msgs
       Success r msgs -> g gl k state r msgs

instance Monad EvalM where
  (EvalM f) >>= h = EvalM (\g k -> f g (\x -> case h x of {EvalM h -> h g k}))

instance MonadFail EvalM where
  fail msg = EvalM (\g k _ _ msgs -> Fail (pp msg) msgs)

instance MonadPlus EvalM where

evalError msg = EvalM (\g k _ _ msgs -> Fail msg msgs)

evalWarn msg = EvalM (\g k state r msgs -> k () state r (msg:msgs))

runEvalM :: Globals -> EvalM a -> Check [a]
runEvalM g (EvalM f) = Check $ \(es,ws) ->
  case f g (\x state xs ws -> Success (x:xs) ws) empty [] ws of
    Fail   msg ws -> Fail msg (es,ws)
    Success xs ws -> Success (reverse xs) (es,ws)
  where
    empty = State Map.empty Map.empty

globals :: EvalM Globals
globals = EvalM (\g k -> k g)

variants :: Choice -> [a] -> EvalM a
variants c xs = EvalM (\g k state@(State choices metas) r msgs ->
  case Map.lookup c choices of
    Just j  -> k (xs !! j) state r msgs
    Nothing -> backtrack 0 xs k choices metas r msgs)
  where
    backtrack j []     k choices metas r msgs = Success r msgs
    backtrack j (x:xs) k choices metas r msgs =
      case k x (State (Map.insert c j choices) metas) r msgs of
        Fail    msg msgs -> Fail msg msgs
        Success r   msgs -> backtrack (j+1) xs k choices metas r msgs

variants' :: Choice -> (a -> EvalM Term) -> [a] -> EvalM Term
variants' c f xs = EvalM (\g k state@(State choices metas) r msgs ->
  case Map.lookup c choices of
    Just j  -> case f (xs !! j) of
                 EvalM f -> f g k state r msgs
    Nothing -> case backtrack g 0 xs choices metas [] msgs of
                 Fail    msg msgs -> Fail msg msgs
                 Success ts  msgs -> k (FV (reverse ts)) state r msgs)
  where
    backtrack g j []     choices metas ts msgs = Success ts msgs
    backtrack g j (x:xs) choices metas ts msgs =
      case f x of
        EvalM f -> case f g (\t st ts msgs -> Success (t:ts) msgs) (State (Map.insert c j choices) metas) ts msgs of
                     Fail    msg msgs -> Fail msg msgs
                     Success ts  msgs -> backtrack g (j+1) xs choices metas ts msgs

try :: (a -> EvalM b) -> [a] -> Message -> EvalM b
try f xs msg = EvalM (\g k state r msgs ->
  let (res,msgs') = backtrack g xs state [] msgs
  in case res of
       []  -> Fail msg msgs'
       res -> continue g k res r msgs')
  where
    backtrack g []     state res msgs = (res,msgs)
    backtrack g (x:xs) state res msgs =
      case f x of
        EvalM f -> case f g (\x state res msgs -> Success ((x,state):res) msgs) state res msgs of
                     Fail msg _       -> backtrack g xs state res msgs
                     Success res msgs -> backtrack g xs state res msgs

    continue g k []              r msgs = Success r msgs
    continue g k ((x,state):res) r msgs =
      case k x state r msgs of
        Fail msg msgs  -> Fail msg msgs
        Success r msgs -> continue g k res r msgs

newResiduation :: Scope -> EvalM MetaId
newResiduation scope = EvalM (\g k (State choices metas) r msgs ->
  let meta_id = Map.size metas+1
  in k meta_id (State choices (Map.insert meta_id (Residuation scope Nothing) metas)) r msgs)

getMeta :: MetaId -> EvalM MetaState
getMeta i = EvalM (\g k state r msgs ->
  case Map.lookup i (metaVars state) of
    Just ms -> k ms state r msgs
    Nothing -> Fail ("Metavariable ?"<>pp i<+>"is not defined") msgs) 

setMeta :: MetaId -> MetaState -> EvalM ()
setMeta i ms = EvalM (\g k (State choices metas) r msgs ->
  let state' = State choices (Map.insert i ms metas)
  in k () state' r msgs)

value2termM :: Bool -> [Ident] -> Value -> EvalM Term
value2termM flat xs (VApp q vs) =
  foldM (\t v -> fmap (App t) (value2termM flat xs v)) (if fst q == cPredef then Q q else QC q) vs
value2termM flat xs (VMeta i vs) = do
  mv <- getMeta i
  case mv of
    Bound scope v -> do g <- globals
                        value2termM flat (map fst scope) (apply g v vs)
    Residuation _ mb_ctr ->
      case mb_ctr of
        Just ctr -> do g <- globals
                       value2termM flat xs (apply g ctr vs)
        Nothing  -> foldM (\t v -> fmap (App t) (value2termM flat xs v)) (Meta i) vs
value2termM flat xs (VSusp j k vs) =
  let v = k (VGen maxBound vs)
  in value2termM flat xs v
value2termM flat xs (VGen j tnks) =
  foldM (\e1 tnk -> fmap (App e1) (value2termM flat xs tnk)) (Vr (reverse xs !! j)) tnks
value2termM flat xs (VClosure env s (Abs b x t)) = do
  g <- globals
  let v  = eval g ((x,VGen (length xs) []):env) s t []
      x' = mkFreshVar xs x
  t <- value2termM flat (x':xs) v
  return (Abs b x' t)
value2termM flat xs (VProd b x v1 v2)
  | x == identW = do t1 <- value2termM flat xs v1
                     v2 <- case v2 of
                             VClosure env s t2 -> do g <- globals
                                                     return (eval g env s t2 [])
                             v2                -> return v2
                     t2 <- value2termM flat xs v2
                     return (Prod b x t1 t2)
  | otherwise   = do t1 <- value2termM flat xs v1
                     v2 <- case v2 of
                             VClosure env s t2 -> do g <- globals
                                                     return (eval g ((x,VGen (length xs) []):env) s t2 [])
                             v2                -> return v2
                     t2 <- value2termM flat (x:xs) v2
                     return (Prod b (mkFreshVar xs x) t1 t2)
value2termM flat xs (VRecType lbls) = do
  lbls <- mapM (\(lbl,v) -> fmap ((,) lbl) (value2termM flat xs v)) lbls
  return (RecType lbls)
value2termM flat xs (VR as) = do
  as <- mapM (\(lbl,v) -> fmap (\t -> (lbl,(Nothing,t))) (value2termM flat xs v)) as
  return (R as)
value2termM flat xs (VP v lbl vs) = do
  t <- value2termM flat xs v
  foldM (\e1 tnk -> fmap (App e1) (value2termM flat xs tnk)) (P t lbl) vs
value2termM flat xs (VExtR v1 v2) = do
  t1 <- value2termM flat xs v1
  t2 <- value2termM flat xs v2
  return (ExtR t1 t2) 
value2termM flat xs (VTable v1 v2) = do
  t1 <- value2termM flat xs v1
  t2 <- value2termM flat xs v2
  return (Table t1 t2)
value2termM flat xs (VT vty env s cs)= do
  ty <- value2termM flat xs vty
  cs <- forM cs $ \(p,t) -> do
          let (_,xs',env') = pattVars (length xs,xs,env) p
          g <- globals
          t <- value2termM flat xs' (eval g env' s t [])
          return (p,t)
  return (T (TTyped ty) cs)
value2termM flat xs (VV vty vs)= do
  ty <- value2termM flat xs vty
  ts <- mapM (value2termM flat xs) vs
  return (V ty ts)
value2termM flat xs (VS v1 v2 vs) = do
  t1 <- value2termM flat xs v1
  t2 <- value2termM flat xs v2
  foldM (\e1 tnk -> fmap (App e1) (value2termM flat xs tnk)) (S t1 t2) vs
value2termM flat xs (VSort s) = return (Sort s)
value2termM flat xs (VStr tok) = return (K tok)
value2termM flat xs (VInt n) = return (EInt n)
value2termM flat xs (VFlt n) = return (EFloat n)
value2termM flat xs VEmpty = return Empty
value2termM flat xs (VC v1 v2) = do
  t1 <- value2termM flat xs v1
  t2 <- value2termM flat xs v2
  return (C t1 t2)
value2termM flat xs (VGlue v1 v2) = do
  t1 <- value2termM flat xs v1
  t2 <- value2termM flat xs v2
  return (Glue t1 t2)
value2termM flat xs (VFV i vs) =
  case flat of
    True  -> do v <- variants i vs
                value2termM True xs v
    False -> variants' i (value2termM False xs) vs
value2termM flat xs (VPatt min max p) = return (EPatt min max p)
value2termM flat xs (VPattType v) = do t <- value2termM flat xs v
                                       return (EPattType t)
value2termM flat xs (VAlts vd vas) = do
  d <- value2termM flat xs vd
  as <- forM vas $ \(vt,vs) -> do
           t <- value2termM flat xs vt
           s <- value2termM flat xs vs
           return (t,s)
  return (Alts d as)
value2termM flat xs (VStrs vs) = do
  ts <- mapM (value2termM flat xs) vs
  return (Strs ts)
value2termM flat xs (VMarkup tag as vs) = do
  as <- mapM (\(id,v) -> value2termM flat xs v >>= \t -> return (id,t)) as
  ts <- mapM (value2termM flat xs) vs
  return (Markup tag as ts)
value2termM flat xs (VError msg) = evalError msg
value2termM flat xs (VCRecType lbls) = do
  lbls <- mapM (\(lbl,_,v) -> fmap ((,) lbl) (value2termM flat xs v)) lbls
  return (RecType lbls)
value2termM flat xs (VCInts Nothing    Nothing) = return (App (QC (cPredef,cInts)) (Meta 0))
value2termM flat xs (VCInts (Just min) Nothing) = return (App (QC (cPredef,cInts)) (EInt min))
value2termM flat xs (VCInts _       (Just max)) = return (App (QC (cPredef,cInts)) (EInt max))
value2termM flat xs v = evalError ("value2termM" <+> ppValue Unqualified 5 v)


pattVars st (PP _ ps)    = foldl pattVars st ps
pattVars st (PV x)       = case st of
                             (i,xs,env) -> (i+1,x:xs,(x,VGen i []):env)
pattVars st (PR as)      = foldl (\st (_,p) -> pattVars st p) st as
pattVars st (PT ty p)    = pattVars st p
pattVars st (PAs x p)    = case st of
                             (i,xs,env) -> pattVars (i+1,x:xs,(x,VGen i []):env) p
pattVars st (PImplArg p) = pattVars st p
pattVars st (PSeq _ _ p1 _ _ p2) = pattVars (pattVars st p1) p2
pattVars st _            = st


ppValue q d (VApp c vs) = prec d 4 (hsep (ppQIdent q c : map (ppValue q 5) vs))
ppValue q d (VMeta i vs) = prec d 4 (hsep ((if i > 0 then pp "?" <> pp i else pp "?") : map (ppValue q 5) vs))
ppValue q d (VSusp i k vs) = prec d 4 (hsep (pp "#susp" : (if i > 0 then pp "?" <> pp i else pp "?") : map (ppValue q 5) vs))
ppValue q d (VGen _ _) = pp "VGen"
ppValue q d (VClosure env c t) = pp "[|" <> ppTerm q 4 t <> pp "|]"
ppValue q d (VProd _ _ _ _) = pp "VProd"
ppValue q d (VRecType _) = pp "VRecType"
ppValue q d (VR _) = pp "VR"
ppValue q d (VP v l vs) = prec d 5 (hsep (ppValue q 5 v <> '.' <> l : map (ppValue q 5) vs))
ppValue q d (VExtR _ _) = pp "VExtR"
ppValue q d (VTable _ _) = pp "VTable"
ppValue q d (VT t _ _ cs) = "table" <+> ppValue q 0 t <+> '{' $$
                               nest 2 (vcat (punctuate ';' (map (ppCase q) cs))) $$
                            '}'
                            where
                              ppCase q (p,e) = ppPatt q 0 p <+> "=>" <+> ppTerm q 0 e
ppValue q d (VV _ _) = pp "VV"
ppValue q d (VS v1 v2 vs) = prec d 3 (hsep (hang (ppValue q 3 v1) 2 ("!" <+> ppValue q 4 v2) : map (ppValue q 5) vs))
ppValue q d (VSort s) = pp s
ppValue q d (VInt n) = pp n
ppValue q d (VFlt f) = pp f
ppValue q d (VStr s) = ppTerm q d (K s)
ppValue q d VEmpty = pp "[]"
ppValue q d (VC v1 v2) = prec d 1 (hang (ppValue q 2 v1) 2 ("++" <+> ppValue q 1 v2))
ppValue q d (VGlue v1 v2) = prec d 2 (ppValue q 3 v1 <+> '+'  <+> ppValue q 2 v2)
ppValue q d (VPatt _ _ _) = pp "VPatt"
ppValue q d (VPattType _) = pp "VPattType"
ppValue q d (VFV i vs) = prec d 4 ("variants" <+> pp i <+> braces (fsep (punctuate ';' (map (ppValue q 0) vs))))
ppValue q d (VAlts e xs) = prec d 4 ("pre" <+> braces (ppValue q 0 e <> ';' <+> fsep (punctuate ';' (map (ppAltern q) xs))))
ppValue q d (VStrs _) = pp "VStrs"
ppValue q d (VSymCat i r rs) = pp '<' <> pp i <> pp ',' <> pp r <> pp '>'
ppValue q d (VError msg) = prec d 4 (pp "error" <+> ppTerm q 5 (K (show msg)))
ppValue q d (VCRecType ass) = pp "VCRecType"
ppValue q d (VCInts Nothing    Nothing)    = prec d 4 (pp "Ints ?")
ppValue q d (VCInts (Just min) Nothing)    = prec d 4 (pp "Ints" <+> brackets (pp min <> ".."))
ppValue q d (VCInts Nothing    (Just max)) = prec d 4 (pp "Ints" <+> brackets (".." <> pp max))
ppValue q d (VCInts (Just min) (Just max))
   | min == max                            = prec d 4 (pp "Ints" <+> min)
   | otherwise                             = prec d 4 (pp "Ints" <+> brackets (pp min <> ".." <> pp max))

ppAltern q (x,y) = ppValue q 0 x <+> '/' <+> ppValue q 0 y

prec d1 d2 doc
  | d1 > d2   = parens doc
  | otherwise = doc

value2string g v = fmap (\(_,ws,_) -> unwords ws) (value2string' g v False [] [])

value2string' g (VMeta i vs)   b ws        qs = CSusp i (\v -> value2string' g (apply g v vs) b ws qs)
value2string' g (VSusp i k vs) b ws        qs = CSusp i (\v -> value2string' g (apply g (k v) vs) b ws qs)
value2string' g (VStr w1)     True (w2:ws) qs = Const (False,(w1++w2):ws,qs)
value2string' g (VStr w)         _ ws      qs = Const (False,w       :ws,qs)
value2string' g VEmpty           b ws      qs = Const (b,ws,qs)
value2string' g (VC v1 v2)       b ws      qs = concat v1 (value2string' g v2 b ws qs)
  where
    concat v1 (Const (b,ws,qs)) = value2string' g v1 b ws qs
    concat v1 (CFV i vs)        = CFV i [concat v1 v2 | v2 <- vs]
    concat v1 res               = res
value2string' g (VApp q []) b    ws      qs
  | q == (cPredef,cNonExist)              = NonExist
value2string' g (VApp q []) b    ws      qs
  | q == (cPredef,cSOFT_SPACE)            = if null ws
                                              then Const (b,ws,q:qs)
                                              else Const (b,ws,qs)
value2string' g (VApp q []) b     ws     qs
  | q == (cPredef,cBIND) || q == (cPredef,cSOFT_BIND) 
                                          = if null ws
                                              then Const (True,ws,q:qs)
                                              else Const (True,ws,qs)
value2string' g (VApp q []) b     ws     qs
  | q == (cPredef,cCAPIT) = capit ws
  where
    capit []            = Const (b,[],q:qs)
    capit ((c:cs) : ws) = Const (b,(toUpper c : cs) : ws,qs)
    capit ws            = Const (b,ws,qs)
value2string' g (VApp q []) b     ws     qs
  | q == (cPredef,cALL_CAPIT) = all_capit ws
  where
    all_capit []       = Const (b,[],q:qs)
    all_capit (w : ws) = Const (b,map toUpper w : ws,qs)
value2string' g (VAlts vd vas) b  ws     qs =
  case ws of
    []    -> value2string' g vd b ws qs
    (w:_) -> pre vd vas w b ws qs
  where
    pre vd []                 w            = value2string' g vd
    pre vd ((v,VStrs ss):vas) w
      | or [startsWith s w | VStr s <- ss] = value2string' g v
      | otherwise                          = pre vd vas w
value2string' g (VFV s vs) b      ws     qs =
  CFV s [value2string' g v b ws qs | v <- vs]
value2string' _ _ _ _ _ = RunTime

startsWith []          _ = True
startsWith (x:xs) (y:ys)
  | x == y               = startsWith xs ys
startsWith      _      _ = False

string2value s = string2value' (words s)

string2value' []     = VEmpty
string2value' [w]    = VStr w
string2value' (w:ws) = VC (VStr w) (string2value' ws)

value2int g (VMeta i vs)     = CSusp i (\v -> value2int g (apply g v vs))
value2int g (VSusp i k vs)   = CSusp i (\v -> value2int g (apply g (k v) vs))
value2int g (VInt n)         = Const n
value2int g (VFV s vs)       = CFV s (map (value2int g) vs)
value2int g _                = RunTime

newtype Choice = Choice Integer deriving (Eq,Ord,Pretty,Show)

unit :: Choice
unit = Choice 1

split :: Choice -> (Choice,Choice)
split (Choice c) = (Choice (2*c), Choice (2*c+1))

split4 :: Choice -> (Choice,Choice,Choice,Choice)
split4 (Choice c) = (Choice (4*c), Choice (4*c+1), Choice (4*c+2), Choice (4*c+3))

mapC :: (Choice -> a -> b) -> Choice -> [a] -> [b]
mapC f c []     = []
mapC f c [x]    = [f c x]
mapC f c (x:xs) =
  let (!c1,!c2) = split c
  in f c1 x : mapC f c2 xs

mapCM :: Monad m => (Choice -> a -> m b) -> Choice -> [a] -> m [b]
mapCM f c []     = return []
mapCM f c [x]    = do y <- f c x
                      return [y]
mapCM f c (x:xs) = do
  let (!c1,!c2) = split c
  y  <- f c1 x
  ys <- mapCM f c2 xs
  return (y:ys)

pdArity :: Int -> PredefCombinator Value
pdArity n def = \g c args ->
  case splitAt' n args of
    Nothing -> RunTime
    Just (usedArgs, remArgs) ->
      fmap (\v -> apply g v remArgs) (def g c usedArgs)
  where
    abstract i n t
      | n <= 0    = t
      | otherwise = let x = identV (rawIdentS "a") i
                    in Abs Explicit x (abstract (i + 1) (n - 1) (App t (Vr x)))
