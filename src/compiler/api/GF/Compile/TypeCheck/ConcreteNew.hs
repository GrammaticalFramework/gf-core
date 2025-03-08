{-# LANGUAGE RankNTypes, CPP, TupleSections, LambdaCase #-}
module GF.Compile.TypeCheck.ConcreteNew ( checkLType, checkLType', inferLType, inferLType' ) where

-- The code here is based on the paper:
-- Simon Peyton Jones, Dimitrios Vytiniotis, Stephanie Weirich.
-- Practical type inference for arbitrary-rank types.
-- 14 September 2011

import GF.Grammar hiding (Env, VGen, VApp, VRecType, ppValue)
import GF.Grammar.Lookup
import GF.Grammar.Predef
import GF.Grammar.Lockfield
import GF.Compile.Compute.Concrete2
import GF.Infra.CheckM
import GF.Data.ErrM ( Err(Ok, Bad) )
import Control.Applicative(Applicative(..))
import Control.Monad(ap,liftM,mplus,foldM,zipWithM,forM,filterM,unless)
import Control.Monad.ST
import GF.Text.Pretty
import Data.STRef
import Data.List (nub, (\\), tails)
import qualified Data.Map as Map
import Data.Maybe(fromMaybe,isNothing,mapMaybe)
import Data.Functor((<&>))
import qualified Control.Monad.Fail as Fail

checkLType :: Globals -> Term -> Type -> Check [(Term, Type)]
checkLType globals t ty = runEvalM globals $
  do let (c1,c2) = split unit
     (t,vty) <- checkLType' c1 t (eval globals [] c2 ty [])
     ty <- value2termM True [] vty
     return (t,ty)

checkLType' :: Choice -> Term -> Constraint -> EvalM (Term, Constraint)
checkLType' c t vty = do
  (t,vty) <- tcRho [] c t (Just vty)
  t <- zonkTerm [] t
  return (t,vty)

inferLType :: Globals -> Term -> Check [(Term, Type)]
inferLType globals t = runEvalM globals $ do
  (t,vty) <- inferLType' t
  ty <- value2termM True [] vty
  return (t,ty)

inferLType' :: Term -> EvalM (Term, Constraint)
inferLType' t = do
  (t,vty) <- inferSigma [] unit t
  t <- zonkTerm [] t
  return (t,vty)

inferSigma :: Scope -> Choice -> Term -> EvalM (Term,Sigma)
inferSigma scope s t = do                                      -- GEN1
  (t,ty) <- tcRho scope s t Nothing
  env_tvs <- getMetaVars (scopeTypes scope)
  res_tvs <- getMetaVars [(scope,ty)]
  let forall_tvs = res_tvs \\ env_tvs
  quantify scope t forall_tvs ty

vtypeInt   = VApp (cPredef,cInt) []
vtypeFloat = VApp (cPredef,cFloat) []
vtypeInts i= VApp (cPredef,cInts) [VInt i]
vtypeStr   = VSort cStr
vtypeStrs  = VSort cStrs
vtypeType  = VSort cType
vtypePType = VSort cPType
vtypeMarkup= VApp (cPredef,cMarkup) []

tcRho :: Scope -> Choice -> Term -> Maybe Rho -> EvalM (Term, Rho)
tcRho scope s t@(EInt i)   mb_ty = instSigma scope s t (vtypeInts i) mb_ty -- INT
tcRho scope s t@(EFloat _) mb_ty = instSigma scope s t vtypeFloat mb_ty    -- FLOAT
tcRho scope s t@(K _)      mb_ty = instSigma scope s t vtypeStr   mb_ty    -- STR
tcRho scope s t@(Empty)    mb_ty = instSigma scope s t vtypeStr   mb_ty
tcRho scope s t@(Vr v)     mb_ty = do                          -- VAR
  case lookup v scope of
    Just v_sigma -> instSigma scope s t v_sigma mb_ty
    Nothing      -> evalError ("Unknown variable" <+> v)
tcRho scope c t@(Q id)     mb_ty = do
  let (c1,c2) = split c
  (t,ty) <- tcApp scope c1 t t []
  instSigma scope c2 t ty mb_ty
tcRho scope c t@(QC id)    mb_ty = do
  let (c1,c2) = split c
  (t,ty) <- tcApp scope c1 t t []
  instSigma scope c2 t ty mb_ty
tcRho scope c t@(App fun arg) mb_ty = do
  let (c1,c2) = split c
  (t,ty) <- tcApp scope c1 t t []
  instSigma scope c2 t ty mb_ty
tcRho scope c (Abs bt var body) Nothing = do                   -- ABS1
  i <- newResiduation scope
  let arg_ty = VMeta i []
  (body,body_ty) <- tcRho ((var,arg_ty):scope) c body Nothing
  let m = length scope
      n = m+1
  (b,used_bndrs) <- check m n (False,[]) body_ty
  if b
    then let v = head (allBinders \\ used_bndrs)
         in return (Abs bt var body, (VProd bt v arg_ty body_ty))
    else return (Abs bt var body, (VProd bt identW arg_ty body_ty))
  where
    check m n st (VApp f vs)       = foldM (check m n) st vs
    check m n st (VMeta i vs)      = do
      state <- getMeta i
      case state of
        Bound _ v -> do g <- globals
                        check m n st (apply g v vs)
        _         -> foldM (check m n) st vs
    check m n st@(b,xs) (VGen i vs)
      | i == m                     = return (True, xs)
      | otherwise                  = return st
    check m n st (VClosure env c (Abs bt x t)) = do
      g <- globals
      check m (n+1) st (eval g ((x,VGen n []):env) c t [])
    check m n st (VProd _ x v1 v2) = do
      st@(b,xs) <- check m n st v1
      case v2 of
        VClosure env c t -> do g  <- globals
                               check m (n+1) (b,x:xs) (eval g ((x,VGen n []):env) c t [])
        v2               -> check m n st v2
    check m n st (VRecType as)     = foldM (\st (l,v) -> check m n st v) st as
    check m n st (VR as)           =
      foldM (\st (lbl,tnk) -> check m n st tnk) st as
    check m n st (VP v l vs)       =
      check m n st v >>= \st -> foldM (check m n) st vs
    check m n st (VExtR v1 v2)     =
      check m n st v1 >>= \st -> check m n st v2
    check m n st (VTable v1 v2)    =
      check m n st v1 >>= \st -> check m n st v2
    check m n st (VT ty env c cs)    =
      check m n st ty    -- Traverse cs as well
    check m n st (VV ty cs)        =
      check m n st ty >>= \st -> foldM (check m n) st cs
    check m n st (VS v1 tnk vs)    = do
      st <- check m n st v1
      st <- check m n st tnk
      foldM (check m n) st vs
    check m n st (VSort _)         = return st
    check m n st (VInt _)          = return st
    check m n st (VFlt _)          = return st
    check m n st (VStr _)          = return st
    check m n st VEmpty            = return st
    check m n st (VC v1 v2)        =
      check m n st v1 >>= \st -> check m n st v2
    check m n st (VGlue v1 v2)     =
      check m n st v1 >>= \st -> check m n st v2
    check m n st (VPatt _ _ _)     = return st
    check m n st (VPattType v)     = check m n st v
    check m n st (VAlts v vs)      = do
      st <- check m n st v
      foldM (\st (v1,v2) -> check m n st v1 >>= \st -> check m n st v2) st vs
    check m n st (VStrs vs)        =
      foldM (check m n) st vs
tcRho scope c t@(Abs Implicit var body) (Just ty) = do         -- ABS2
  (bt, x, var_ty, body_ty) <- unifyFun scope ty
  if bt == Implicit
    then return ()
    else evalError (ppTerm Unqualified 0 t <+> "is an implicit function, but no implicit function is expected")
  body_ty <- evalCodomain scope x body_ty
  (body, body_ty) <- tcRho ((var,var_ty):scope) c body (Just body_ty)
  return (Abs Implicit var body,ty)
tcRho scope c (Abs Explicit var body) (Just ty) = do           -- ABS3
  (scope,f,ty') <- skolemise scope ty
  (_,x,var_ty,body_ty) <- unifyFun scope ty'
  body_ty <- evalCodomain scope x body_ty
  (body, body_ty) <- tcRho ((var,var_ty):scope) c body (Just body_ty)
  return (f (Abs Explicit var body),ty)
tcRho scope c (Meta _) mb_ty = do
  i <- newResiduation scope
  ty <- case mb_ty of
          Just ty -> return ty
          Nothing -> do j <- newResiduation scope
                        return (VMeta j [])
  return (Meta i, ty)
tcRho scope c (Let (var, (Nothing, rhs)) body) mb_ty = do      -- LET
  let (c1,c2) = split c
  (rhs,var_ty) <- tcRho scope c1 rhs Nothing
  (body, body_ty) <- tcRho ((var,var_ty):scope) c2 body mb_ty
  var_ty <- value2termM True (scopeVars scope) var_ty
  return (Let (var, (Just var_ty, rhs)) body, body_ty)
tcRho scope c (Let (var, (Just ann_ty, rhs)) body) mb_ty = do  -- LET
  let (c1,c2,c3,c4) = split4 c
  (ann_ty, _) <- tcRho scope c1 ann_ty (Just vtypeType)
  g <- globals
  let v_ann_ty = eval g (scopeEnv scope) c2 ann_ty []
  (rhs,_) <- tcRho scope c3 rhs (Just v_ann_ty)
  (body, body_ty) <- tcRho ((var,v_ann_ty):scope) c4 body mb_ty
  var_ty <- value2termM True (scopeVars scope) v_ann_ty
  return (Let (var, (Just var_ty, rhs)) body, body_ty)
tcRho scope c (Typed body ann_ty) mb_ty = do                   -- ANNOT
  let (c1,c2,c3,c4) = split4 c
  (ann_ty, _) <- tcRho scope c1 ann_ty (Just vtypeType)
  g <- globals
  let v_ann_ty = eval g (scopeEnv scope) c2 ann_ty []
  (body,_) <- tcRho scope c3 body (Just v_ann_ty)
  instSigma scope c4 (Typed body ann_ty) v_ann_ty mb_ty
tcRho scope c (FV ts) mb_ty = do
  (ty,subsume) <-
    case mb_ty of
      Just ty -> do return (ty, \t ty' -> return t)
      Nothing -> do i <- newResiduation scope
                    let ty = VMeta i []
                    return (ty, \t ty' -> subsCheckRho scope t ty' ty)

  let go c t = do (t, ty) <- tcRho scope c t mb_ty
                  subsume t ty

  ts <- mapCM go c ts
  return (FV ts, ty)
tcRho scope s t@(Sort _) mb_ty = do
  instSigma scope s t vtypeType mb_ty
tcRho scope c t@(RecType rs) Nothing   = do
  (rs,mb_ty) <- tcRecTypeFields scope c rs Nothing
  return (RecType rs,fromMaybe vtypePType mb_ty)
tcRho scope c t@(RecType rs) (Just ty) = do
  (scope,f,ty') <- skolemise scope ty
  case ty' of
    VSort s
      | s == cType  -> return ()
      | s == cPType -> return ()
    VMeta i vs-> case rs of
                   [] -> unifyVar scope i vs vtypePType
                   _  -> return ()
    ty        -> do ty <- value2termM False (scopeVars scope) ty
                    evalError ("The record type" <+> ppTerm Unqualified 0 t $$
                               "cannot be of type" <+> ppTerm Unqualified 0 ty)
  (rs,mb_ty) <- tcRecTypeFields scope c rs (Just ty')
  return (f (RecType rs),ty)
tcRho scope s t@(Table p res) mb_ty = do
  let (s1,s23) = split s
      (s2,s3)  = split s23
  (p,  p_ty)   <- tcRho scope s1 p   (Just vtypePType)
  (res,res_ty) <- tcRho scope s2 res (Just vtypeType)
  instSigma scope s3 (Table p res) vtypeType mb_ty
tcRho scope c (Prod bt x ty1 ty2) mb_ty = do
  let (c1,c2,c3,c4) = split4 c
  (ty1,ty1_ty) <- tcRho scope c1 ty1 (Just vtypeType)
  g <- globals
  (ty2,ty2_ty) <- tcRho ((x,eval g (scopeEnv scope) c2 ty1 []):scope) c3 ty2 (Just vtypeType)
  instSigma scope c4 (Prod bt x ty1 ty2) vtypeType mb_ty
tcRho scope c (S t p) mb_ty = do
  let (c1,c2) = split c
  let mk_val i = VMeta i []
  p_ty   <- fmap mk_val $ newResiduation scope
  res_ty <- case mb_ty of
              Nothing -> fmap mk_val $ newResiduation scope
              Just ty -> return ty
  let t_ty = VTable p_ty res_ty
  (t,t_ty) <- tcRho scope c1 t (Just t_ty)
  (p,_) <- tcRho scope c2 p (Just p_ty)
  return (S t p, res_ty)
tcRho scope c (T tt ps) Nothing = do                           -- ABS1/AABS1 for tables
  let (c1,c2) = split c
  let mk_val i = VMeta i []
  p_ty   <- case tt of
              TRaw      -> fmap mk_val $ newResiduation scope
              TTyped ty -> do let (c3,c4) = split c1
                              (ty, _) <- tcRho scope c3 ty (Just vtypeType)
                              g <- globals
                              return (eval g (scopeEnv scope) c4 ty [])
  res_ty <- fmap mk_val $ newResiduation scope
  ps <- tcCases scope c2 ps p_ty res_ty
  p_ty_t <- value2termM True [] p_ty
  return (T (TTyped p_ty_t) ps, VTable p_ty res_ty)
tcRho scope c (T tt ps) (Just ty) = do                         -- ABS2/AABS2 for tables
  let (c12,c34) = split c
      (c3,c4)   = split c34
  (scope,f,ty') <- skolemise scope ty
  (p_ty, res_ty) <- unifyTbl scope ty'
  case tt of
    TRaw      -> return ()
    TTyped ty -> do let (c1,c2) = split c12
                    (ty, _) <- tcRho scope c1 ty (Just vtypeType)
                    g <- globals
                    unify scope (eval g (scopeEnv scope) c2 ty []) p_ty
  ps <- tcCases scope c3 ps p_ty res_ty
  p_ty_t <- value2termM True (scopeVars scope) p_ty
  return (f (T (TTyped p_ty_t) ps), VTable p_ty res_ty)
tcRho scope c (V p_ty ts) Nothing = do
  let (c1,c2,c3,c4) = split4 c
  (p_ty, _) <- tcRho scope c1 p_ty (Just vtypeType)
  i <- newResiduation scope
  let res_ty = VMeta i []

  let go c t = do (t, ty) <- tcRho scope c t Nothing
                  subsCheckRho scope t ty res_ty

  ts <- mapCM go c2 ts
  g <- globals
  return (V p_ty ts, VTable (eval g (scopeEnv scope) c3 p_ty []) res_ty)
tcRho scope c (V p_ty0 ts) (Just ty) = do
  let (c1,c2,c3,c4) = split4 c
  (scope,f,ty') <- skolemise scope ty
  (p_ty, res_ty) <- unifyTbl scope ty'
  (p_ty0, _) <- tcRho scope c1 p_ty0 (Just vtypeType)
  g <- globals
  let p_vty0 = eval g (scopeEnv scope) c2 p_ty0 []
  unify scope p_ty p_vty0
  ts <- mapCM (\c t -> fmap fst $ tcRho scope c t (Just res_ty)) c3 ts
  return (V p_ty0 ts, VTable p_ty res_ty)
tcRho scope c (R rs) Nothing = do
  lttys <- inferRecFields scope c rs
  rs <- mapM (\(l,t,ty) -> value2termM True (scopeVars scope) ty >>= \ty -> return (l, (Just ty, t))) lttys
  return (R        rs,
          VRecType [(l, ty) | (l,t,ty) <- lttys]
         )
tcRho scope c (R rs) (Just ty) = do
  (scope,f,ty') <- skolemise scope ty
  case ty' of
    (VRecType ltys) -> do lttys <- checkRecFields scope c rs ltys
                          rs <- mapM (\(l,t,ty) -> value2termM True (scopeVars scope) ty >>= \ty -> return (l, (Just ty, t))) lttys
                          return ((f . R)  rs,
                                  VRecType [(l, ty) | (l,t,ty) <- lttys]
                                 )
    ty              -> do lttys <- inferRecFields scope c rs
                          t <- liftM (f . R) (mapM (\(l,t,ty) -> value2termM True (scopeVars scope) ty >>= \ty -> return (l, (Just ty, t))) lttys)
                          let ty' = VRecType [(l, ty) | (l,t,ty) <- lttys]
                          t <- subsCheckRho scope t ty' ty
                          return (t, ty')
tcRho scope c (P t l) mb_ty = do
  l_ty   <- case mb_ty of
              Just ty -> return ty
              Nothing -> do i <- newResiduation scope
                            return (VMeta i [])
  (t,t_ty) <- tcRho scope c t (Just (VRecType [(l,l_ty)]))
  return (P t l,l_ty)
tcRho scope c (C t1 t2) mb_ty = do
  let (c1,c2,c3,c4) = split4 c
  (t1,t1_ty) <- tcRho scope c1 t1 (Just vtypeStr)
  (t2,t2_ty) <- tcRho scope c2 t2 (Just vtypeStr)
  instSigma scope c3 (C t1 t2) vtypeStr mb_ty
tcRho scope c (Glue t1 t2) mb_ty = do
  let (c1,c2,c3,c4) = split4 c
  (t1,t1_ty) <- tcRho scope c1 t1 (Just vtypeStr)
  (t2,t2_ty) <- tcRho scope c2 t2 (Just vtypeStr)
  instSigma scope c3 (Glue t1 t2) vtypeStr mb_ty
tcRho scope c t@(ExtR t1 t2) mb_ty = do
  let (c1,c2,c3,c4) = split4 c
  (t1,t1_ty) <- tcRho scope c1 t1 Nothing
  (t2,t2_ty) <- tcRho scope c2 t2 Nothing
  case (t1_ty,t2_ty) of
    (VSort s1,VSort s2)
       | (s1 == cType || s1 == cPType) &&
         (s2 == cType || s2 == cPType) -> let sort | s1 == cPType && s2 == cPType = cPType
                                                   | otherwise                    = cType
                                          in instSigma scope c3 (ExtR t1 t2) (VSort sort) mb_ty
    (VRecType rs1, VRecType rs2)       -> instSigma scope c3 (ExtR t1 t2) (VRecType (rs2++rs1)) mb_ty
    _                                  -> evalError ("Cannot type check" <+> ppTerm Unqualified 0 t)
tcRho scope c (ELin cat t) mb_ty = do  -- this could be done earlier, i.e. in the parser
  tcRho scope c (ExtR t (R [(lockLabel cat,(Just (RecType []),R []))])) mb_ty
tcRho scope c (ELincat cat t) mb_ty = do  -- this could be done earlier, i.e. in the parser
  tcRho scope c (ExtR t (RecType [(lockLabel cat,RecType [])])) mb_ty
tcRho scope c (Alts t ss) mb_ty = do
  let (c1,c2,c3,c4) = split4 c
  (t,_) <- tcRho scope c1 t (Just vtypeStr)
  ss    <- mapCM (\c (t1,t2) -> do
                          let (c1,c2) = split c
                          (t1,_) <- tcRho scope c1 t1 (Just vtypeStr)
                          (t2,_) <- tcRho scope c2 t2 (Just vtypeStrs)
                          return (t1,t2))
                 c2 ss
  instSigma scope c3 (Alts t ss) vtypeStr mb_ty
tcRho scope c (Strs ss) mb_ty = do
  let (c1,c2) = split c
  ss <- mapCM (\c t -> do (t,_) <- tcRho scope c t (Just vtypeStr)
                          return t)
              c1 ss
  instSigma scope c2 (Strs ss) vtypeStrs mb_ty
tcRho scope c (EPattType ty) mb_ty = do
  let (c1,c2) = split c
  (ty, _) <- tcRho scope c1 ty (Just vtypeType)
  instSigma scope c2 (EPattType ty) vtypeType mb_ty
tcRho scope c t@(EPatt min max p) mb_ty = do
  (scope,f,ty) <- case mb_ty of
                    Nothing -> do i <- newResiduation scope
                                  return (scope,id,VMeta i [])
                    Just ty -> do (scope,f,ty) <- skolemise scope ty
                                  case ty of
                                    VPattType ty -> return (scope,f,ty)
                                    _            -> evalError (ppTerm Unqualified 0 t <+> "must be of pattern type but" <+> ppTerm Unqualified 0 t <+> "is expected")
  tcPatt scope c p ty
  return (f (EPatt min max p), ty)
tcRho scope c (Markup tag attrs children) mb_ty = do
  let (c1,c2,c3,c4) = split4 c
  attrs <- mapCM (\c (id,t) -> do
                       (t,_) <- tcRho scope c t Nothing
                       return (id,t))
                 c1 attrs
  res <- mapCM (\c child -> tcRho scope c child Nothing) c2 children
  instSigma scope c3 (Markup tag attrs (map fst res)) vtypeMarkup mb_ty
tcRho scope c (Reset ctl t) mb_ty = do
  let (c1,c2) = split c
  (t,_) <- tcRho scope c1 t Nothing
  instSigma scope c2 (Reset ctl t) vtypeMarkup mb_ty
tcRho scope s t _ = unimplemented ("tcRho "++show t)

evalCodomain :: Scope -> Ident -> Value -> EvalM Value
evalCodomain scope x (VClosure env c t) = do
  g <- globals
  return (eval g ((x,VGen (length scope) []):env) c t [])
evalCodomain scope x t = return t

tcCases scope c []         p_ty res_ty = return []
tcCases scope c ((p,t):cs) p_ty res_ty = do
  let (c1,c2,c3,c4) = split4 c
  scope' <- tcPatt scope c1 p p_ty
  (t,_)  <- tcRho scope' c2 t (Just res_ty)
  cs <- tcCases scope c3 cs p_ty res_ty
  return ((p,t):cs)

tcApp scope c t0 (App fun arg) args = tcApp scope c t0 fun (arg:args)     -- APP
tcApp scope c t0 t@(Q id)      args = resolveOverloads scope c t0 id args -- VAR (global)
tcApp scope c t0 t@(QC id)     args = resolveOverloads scope c t0 id args -- VAR (global)
tcApp scope c t0 t             args = do
  let (c1,c2) = split c
  (t,ty) <- tcRho scope c1 t Nothing
  reapply1 scope c2 t ty args

reapply1 :: Scope -> Choice -> Term -> Value -> [Term] -> EvalM (Term,Rho)
reapply1 scope c fun fun_ty []                   = return (fun,fun_ty)
reapply1 scope c fun fun_ty ((ImplArg arg):args) = do -- Implicit arg case
  let (c1,c2,c3,c4) = split4 c
  (bt, x, arg_ty, res_ty) <- unifyFun scope fun_ty
  unless (bt == Implicit) $
     evalError (ppTerm Unqualified 0 (App fun (ImplArg arg)) <+>
                "is an implicit argument application, but no implicit argument is expected")
  (arg,_) <- tcRho scope c1 arg (Just arg_ty)
  res_ty <- case res_ty of
              VClosure res_env res_c res_ty -> do g <- globals
                                                  return (eval g ((x,eval g (scopeEnv scope) c2 arg []):res_env) res_c res_ty [])
              res_ty                        -> return res_ty
  reapply1 scope c3 (App fun (ImplArg arg)) res_ty args
reapply1 scope c fun fun_ty (arg:args) = do -- Explicit arg (fallthrough) case
  let (c1,c2,c3,c4) = split4 c
  (fun,fun_ty) <- instantiate scope fun fun_ty
  (_, x, arg_ty, res_ty) <- unifyFun scope fun_ty
  (arg,_) <- tcRho scope c1 arg (Just arg_ty)
  res_ty <- case res_ty of
              VClosure res_env res_c res_ty -> do g <- globals
                                                  return (eval g ((x,eval g (scopeEnv scope) c2 arg []):res_env) res_c res_ty [])
              res_ty                  -> return res_ty
  reapply1 scope c3 (App fun arg) res_ty args

resolveOverloads :: Scope -> Choice -> Term -> QIdent -> [Term] -> EvalM (Term,Rho)
resolveOverloads scope c t0 q args = do
  g@(Gl gr _) <- globals
  case lookupOverloadTypes gr q of
    Bad msg  -> evalError (pp msg)
    Ok [(t,ty)] -> do let (c1,c2) = split c
                      reapply1 scope c1 t (eval g [] c2 ty []) args
    Ok ttys     -> do let (c1,c2) = split c
                      arg_tys <- mapCM (checkArg g) c1 args
                      let v_ttys = mapC (\c (t,ty) -> (t,eval g [] c ty [])) c2 ttys
                      try (\(fun,fun_ty) -> reapply2 scope fun fun_ty arg_tys) v_ttys (pp "Overload resolution failed")
  where
    checkArg g c (ImplArg arg) = do
      let (c1,c2) = split c
      (arg,arg_ty) <- tcRho scope c1 arg Nothing
      let v = eval g (scopeEnv scope) c2 arg []
      return (ImplArg arg,v,arg_ty)
    checkArg g c arg = do
      let (c1,c2) = split c
      (arg,arg_ty) <- tcRho scope c1 arg Nothing
      let v = eval g (scopeEnv scope) c2 arg []
      return (arg,v,arg_ty)

reapply2 :: Scope -> Term -> Value -> [(Term,Value,Value)] -> EvalM (Term,Rho)
reapply2 scope fun fun_ty []                                = return (fun,fun_ty)
reapply2 scope fun fun_ty ((ImplArg arg,arg_v,arg_ty):args) = do -- Implicit arg case
  (bt, x, arg_ty', res_ty) <- unifyFun scope fun_ty
  unless (bt == Implicit) $
     evalError (ppTerm Unqualified 0 (App fun (ImplArg arg)) <+>
                "is an implicit argument application, but no implicit argument is expected")
  arg <- subsCheckRho scope arg arg_ty' arg_ty
  res_ty <- case res_ty of
              VClosure res_env res_c res_ty -> do g <- globals
                                                  return (eval g ((x,arg_v):res_env) res_c res_ty [])
              res_ty                        -> return res_ty
  reapply2 scope (App fun (ImplArg arg)) res_ty args
reapply2 scope fun fun_ty ((arg,arg_v,arg_ty):args) = do -- Explicit arg (fallthrough) case
  (fun,fun_ty) <- instantiate scope fun fun_ty
  (_, x, arg_ty', res_ty) <- unifyFun scope fun_ty
  arg <- subsCheckRho scope arg arg_ty arg_ty'
  res_ty <- case res_ty of
              VClosure res_env res_c res_ty -> do g <- globals
                                                  return (eval g ((x,arg_v):res_env) res_c res_ty [])
              res_ty                  -> return res_ty
  reapply2 scope (App fun arg) res_ty args

tcPatt scope c PW        ty0 =
  return scope
tcPatt scope c (PV x)    ty0 =
  return ((x,ty0):scope)
tcPatt scope c (PP q ps) ty0 = do
  g@(Gl gr _) <- globals
  ty <- case lookupResType gr q of
          Ok ty   -> return ty
          Bad msg -> evalError (pp msg)
  let go scope c ty []     = return (scope,ty)
      go scope c ty (p:ps) = do (_,_,arg_ty,res_ty) <- unifyFun scope ty
                                let (c1,c2) = split c
                                scope <- tcPatt scope c1 p arg_ty
                                go scope c2 res_ty ps
  let (c1,c2) = split c
  (scope,ty) <- go scope c1 (eval g [] c2 ty []) ps
  unify scope ty0 ty
  return scope
tcPatt scope c (PInt i) ty0 = do
  unify scope (vtypeInts i) ty0
  return scope
tcPatt scope c (PString s) ty0 = do
  unify scope ty0 vtypeStr
  return scope
tcPatt scope c PChar ty0 = do
  unify scope ty0 vtypeStr
  return scope
tcPatt scope c (PSeq _ _ p1 _ _ p2) ty0 = do
  unify scope ty0 vtypeStr
  let (c1,c2) = split c
  scope <- tcPatt scope c1 p1 vtypeStr
  scope <- tcPatt scope c2 p2 vtypeStr
  return scope
tcPatt scope c (PAs x p) ty0 = do
  tcPatt ((x,ty0):scope) c p ty0
tcPatt scope c (PR rs) ty0 = do
  let mk_ltys []            = return []
      mk_ltys ((l,p):rs)    = do i <- newResiduation scope
                                 ltys <- mk_ltys rs
                                 return ((l,p,VMeta i []) : ltys)
      go scope c []            = return scope
      go scope c ((l,p,ty):rs) = do let (c1,c2) = split c
                                    scope <- tcPatt scope c1 p ty
                                    go scope c2 rs
  ltys <- mk_ltys rs
  subsCheckRho scope (EPatt 0 Nothing (PR rs)) (VRecType [(l,ty) | (l,p,ty) <- ltys]) ty0
  go scope c ltys
tcPatt scope c (PAlt p1 p2) ty0 = do
  let (c1,c2) = split c
  tcPatt scope c1 p1 ty0
  tcPatt scope c2 p2 ty0
  return scope
tcPatt scope c (PM q) ty0 = do
  g@(Gl gr _) <- globals
  ty <- case lookupResType gr q of
          Ok ty   -> return ty
          Bad msg -> evalError (pp msg)
  case ty of
    EPattType ty
            -> do unify scope ty0 (eval g [] c ty [])
                  return scope
    ty   -> evalError ("Pattern type expected but " <+> pp ty <+> " found.")
tcPatt scope c p ty = unimplemented ("tcPatt "++show p)

inferRecFields scope c rs =
  mapCM (\c (l,r) -> tcRecField scope c l r Nothing) c rs

checkRecFields scope c []          ltys
  | null ltys                            = return []
  | otherwise                            = evalError ("Missing fields:" <+> hsep (map fst ltys))
checkRecFields scope c ((l,t):lts) ltys =
  case takeIt l ltys of
    (Just ty,ltys) -> do let (c1,c2) = split c
                         ltty  <- tcRecField scope c1 l t (Just ty)
                         lttys <- checkRecFields scope c2 lts ltys
                         return (ltty : lttys)
    (Nothing,ltys) -> do evalWarn ("Discarded field:" <+> l)
                         let (c1,c2) = split c
                         ltty  <- tcRecField scope c1 l t Nothing
                         lttys <- checkRecFields scope c2 lts ltys
                         return lttys     -- ignore the field
  where
    takeIt l1 []  = (Nothing, [])
    takeIt l1 (lty@(l2,ty):ltys)
      | l1 == l2  = (Just ty,ltys)
      | otherwise = let (mb_ty,ltys') = takeIt l1 ltys
                    in (mb_ty,lty:ltys')

tcRecField scope c l (mb_ann_ty,t) mb_ty = do
  (t,ty) <- case mb_ann_ty of
              Just ann_ty -> do let (c1,c2,c3,c4) = split4 c
                                (ann_ty, _) <- tcRho scope c1 ann_ty (Just vtypeType)
                                g <- globals
                                let v_ann_ty = eval g (scopeEnv scope) c2 ann_ty []
                                (t,_) <- tcRho scope c3 t (Just v_ann_ty)
                                instSigma scope c4 t v_ann_ty mb_ty
              Nothing     -> tcRho scope c t mb_ty
  return (l,t,ty)

tcRecTypeFields scope c []          mb_ty = return ([],mb_ty)
tcRecTypeFields scope c ((l,ty):rs) mb_ty = do
  let (c1,c2) = split c
  (ty,sort) <- tcRho scope c1 ty mb_ty
  mb_ty <- case sort of
             VSort s
               | s == cType  -> return (Just sort)
               | s == cPType -> return mb_ty
             VMeta _ _       -> return mb_ty
             _               -> do sort <- value2termM False (scopeVars scope) sort
                                   evalError ("The record type field" <+> l <+> ':' <+> ppTerm Unqualified 0 ty $$
                                              "cannot be of type" <+> ppTerm Unqualified 0 sort)
  (rs,mb_ty) <- tcRecTypeFields scope c2 rs mb_ty
  return ((l,ty):rs,mb_ty)

-- | Invariant: if the third argument is (Just rho),
--              then rho is in weak-prenex form
instSigma :: Scope -> Choice -> Term -> Sigma -> Maybe Rho -> EvalM (Term, Rho)
instSigma scope s t ty1 Nothing    = return (t,ty1)           -- INST1
instSigma scope s t ty1 (Just ty2) = do                       -- INST2
  t <- subsCheckRho scope t ty1 ty2
  return (t,ty2)

-- | Invariant: the second argument is in weak-prenex form
subsCheckRho :: Scope -> Term -> Sigma -> Rho -> EvalM Term
subsCheckRho scope t (VMeta i vs1) (VMeta j vs2)
  | i  == j   = do sequence_ (zipWith (unify scope) vs1 vs2)
                   return t
  | otherwise = do
      mv <- getMeta i
      case mv of
        Bound _ v1 -> do
          g <- globals
          subsCheckRho scope t (apply g v1 vs1) (VMeta j vs2)
        Residuation scope1 _ -> do
          mv <- getMeta j
          case mv of
            Bound _ v2 -> do
              g <- globals
              subsCheckRho scope t (VMeta i vs1) (apply g v2 vs2)
            Residuation scope2 _
              | m > n     -> do setMeta i (Bound scope1 (VMeta j vs2))
                                return t
              | otherwise -> do setMeta j (Bound scope2 (VMeta i vs2))
                                return t
              where
                m = length scope1
                n = length scope2
subsCheckRho scope t ty1@(VMeta i vs) ty2 = do
  mv <- getMeta i
  case mv of
    Bound _ ty1 -> do
      g <- globals
      subsCheckRho scope t (apply g ty1 vs) ty2
    Residuation scope' ctr -> do
      occursCheck scope' i scope ty2
      ctr <- subtype scope ctr ty2
      setMeta i (Residuation scope' (Just ctr))
      return t
subsCheckRho scope t ty1 ty2@(VMeta i vs) = do
  mv <- getMeta i
  case mv of
    Bound _ ty2 -> do
      g <- globals
      subsCheckRho scope t ty1 (apply g ty2 vs)
    Residuation scope' ctr -> do
      occursCheck scope' i scope ty1
      ctr <- supertype scope ctr ty1
      setMeta i (Residuation scope' (Just ctr))
      return t
subsCheckRho scope t (VProd Implicit x ty1 ty2) rho2 = do     -- Rule SPEC
  i <- newResiduation scope
  g <- globals
  let ty2' = case ty2 of
               VClosure env c ty2 -> eval g ((x,VMeta i []):env) c ty2 []
               ty2                -> ty2
  subsCheckRho scope (App t (ImplArg (Meta i))) ty2' rho2
subsCheckRho scope t rho1 (VProd Implicit x ty1 ty2) = do     -- Rule SKOL
  let v = newVar scope
  ty2 <- evalCodomain scope x ty2
  t <- subsCheckRho ((v,ty1):scope) t rho1 ty2
  return (Abs Implicit v t)
subsCheckRho scope t rho1 (VProd Explicit _ a2 r2) = do       -- Rule FUN
  (_,_,a1,r1) <- unifyFun scope rho1
  subsCheckFun scope t a1 r1 a2 r2
subsCheckRho scope t (VProd Explicit _ a1 r1) rho2 = do       -- Rule FUN
  (_,_,a2,r2) <- unifyFun scope rho2
  subsCheckFun scope t a1 r1 a2 r2
subsCheckRho scope t rho1 (VTable p2 r2) = do                 -- Rule TABLE
  (p1,r1) <- unifyTbl scope rho1
  subsCheckTbl scope t p1 r1 p2 r2
subsCheckRho scope t (VTable p1 r1) rho2 = do                 -- Rule TABLE
  (p2,r2) <- unifyTbl scope rho2
  subsCheckTbl scope t p1 r1 p2 r2
subsCheckRho scope t (VSort s1) (VSort s2)                    -- Rule PTYPE
  | s1 == cPType && s2 == cType = return t
subsCheckRho scope t (VApp p1 _) (VApp p2 _)                  -- Rule INT1
  | p1 == (cPredef,cInts) && p2 == (cPredef,cInt) = return t
subsCheckRho scope t (VApp p1 [VInt i]) (VApp p2 [VInt j])        -- Rule INT2
  | p1 == (cPredef,cInts) && p2 == (cPredef,cInts) = do
      if i <= j
        then return t
        else evalError ("Ints" <+> i <+> "is not a subtype of" <+> "Ints" <+> j)
subsCheckRho scope t ty1@(VRecType rs1) ty2@(VRecType rs2) = do      -- Rule REC
  let mkAccess scope t =
        case t of
          ExtR t1 t2 -> do (scope,mkProj1,mkWrap1) <- mkAccess scope t1
                           (scope,mkProj2,mkWrap2) <- mkAccess scope t2
                           return (scope
                                  ,\l -> mkProj2 l `mplus` mkProj1 l
                                  ,mkWrap1 . mkWrap2
                                  )
          R rs -> do sequence_ [evalWarn ("Discarded field:" <+> l) | (l,_) <- rs, isNothing (lookup l rs2)]
                     return (scope
                            ,\l -> lookup l rs
                            ,id
                            )
          Vr x -> do return (scope
                            ,\l -> do VRecType rs <- lookup x scope
                                      ty <- lookup l rs
                                      return (Nothing,P t l)
                            ,id
                            )
          t    -> let x = newVar scope
                  in return (((x,ty1):scope)
                            ,\l  -> return (Nothing,P (Vr x) l)
                            ,Let (x, (Nothing, t))
                            )

      mkField scope l (mb_ty,t) ty1 ty2 = do
        t <- subsCheckRho scope t ty1 ty2
        return (l, (mb_ty,t))

  (scope,mkProj,mkWrap) <- mkAccess scope t

  let fields = [(l,ty2,lookup l rs1) | (l,ty2) <- rs2]
  case [l | (l,_,Nothing) <- fields] of
    []      -> return ()
    missing -> evalError ("In the term" <+> pp t $$
                          "there are no values for fields:" <+> hsep missing)
  rs <- sequence [mkField scope l t ty1 ty2 | (l,ty2,Just ty1) <- fields, Just t <- [mkProj l]]
  return (mkWrap (R rs))
subsCheckRho scope t tau1 (VFV c vs) = do
  tau2 <- variants c vs
  subsCheckRho scope t tau1 tau2
subsCheckRho scope t (VFV c vs) tau2 = do
  tau1 <- variants c vs
  subsCheckRho scope t tau1 tau2
subsCheckRho scope t tau1 tau2 = do                           -- Rule EQ
  unify scope tau1 tau2                                 -- Revert to ordinary unification
  return t

subsCheckFun :: Scope -> Term -> Sigma -> Value -> Sigma -> Value -> EvalM Term
subsCheckFun scope t a1 r1 a2 r2 = do
  let v   = newVar scope
  vt <- subsCheckRho ((v,a2):scope) (Vr v) a2 a1
  g  <- globals
  let r1 = case r1 of
             VClosure env c r1 -> eval g ((v,(VGen (length scope) [])):env) c r1 []
             r1                -> r1
  let r2 = case r2 of
             VClosure env c r2 -> eval g ((v,(VGen (length scope) [])):env) c r2 []
             r2                -> r2
  t  <- subsCheckRho ((v,vtypeType):scope) (App t vt) r1 r2
  return (Abs Explicit v t)

subsCheckTbl :: Scope -> Term -> Sigma -> Rho -> Sigma -> Rho  -> EvalM Term
subsCheckTbl scope t p1 r1 p2 r2 = do
  let x = newVar scope
  xt <- subsCheckRho scope (Vr x) p2 p1
  t  <- subsCheckRho ((x,vtypePType):scope) (S t xt) r1 r2
  p2 <- value2termM True (scopeVars scope) p2
  return (T (TTyped p2) [(PV x,t)])

subtype scope Nothing (VApp p [VInt i])
  | p == (cPredef,cInts) = do
      return (VCInts Nothing (Just i))
subtype scope (Just (VCInts i j)) (VApp p [VInt k])
  | p == (cPredef,cInts) = do
      return (VCInts j (Just (maybe k (min k) i)))
subtype scope Nothing (VRecType ltys) = do
  lctrs <- mapM (\(l,ty) -> supertype scope Nothing ty >>= \ctr -> return (l,True,ctr)) ltys
  return (VCRecType lctrs)
subtype scope (Just (VCRecType lctrs)) (VRecType ltys) = do
  lctrs <- foldM (\lctrs (l,ty) -> union l ty lctrs) lctrs ltys
  return (VCRecType lctrs)
  where
    union l ty [] = do ctr <- subtype scope Nothing ty
                       return [(l,True,ctr)]
    union l ty ((l',o,ctr):lctrs)
      | l == l'   = do ctr <- subtype scope (Just ctr) ty
                       return ((l,True,ctr):lctrs)
      | otherwise = do lctrs <- union l ty lctrs
                       return ((l',o,ctr):lctrs)
subtype scope Nothing    ty = return ty
subtype scope (Just ctr) ty = do
  unify scope ctr ty
  return ty

supertype scope Nothing (VApp p [VInt i])
  | p == (cPredef,cInts) = do
      return (VCInts (Just i) Nothing)
supertype scope (Just (VCInts i j)) (VApp p [VInt k])
  | p == (cPredef,cInts) = do
      return (VCInts (Just (maybe k (max k) i)) j)
supertype scope Nothing (VRecType ltys) = do
  lctrs <- mapM (\(l,ty) -> supertype scope Nothing ty >>= \ctr -> return (l,False,ctr)) ltys
  return (VCRecType lctrs)
supertype scope (Just (VCRecType lctrs)) (VRecType ltys) = do
  lctrs <- foldM (\lctrs (l,o,ctr) -> intersect l o ctr lctrs ltys) [] lctrs
  return (VCRecType lctrs)
  where
    intersect l o ctr lctrs [] = return lctrs
    intersect l o ctr lctrs ((l',ty):ltys2)
      | l == l'   = do ctr <- supertype scope (Just ctr) ty
                       return ((l,o,ctr):lctrs)
      | otherwise = do intersect l o ctr lctrs ltys2
supertype scope Nothing    ty = return ty
supertype scope (Just ctr) ty = do
  unify scope ctr ty
  return ty

-----------------------------------------------------------------------
-- Unification
-----------------------------------------------------------------------

unifyFun :: Scope -> Rho -> EvalM (BindType, Ident, Sigma, Rho)
unifyFun scope (VProd bt x arg res) =
  return (bt,x,arg,res)
unifyFun scope (VFV c vs) = do
  res <- mapM (unifyFun scope) vs
  return (Explicit, identW, VFV c [sigma | (_,_,sigma,rho) <- res], VFV c [rho | (_,_,sigma,rho) <- res])
unifyFun scope tau = do
  let mk_val i = VMeta i []
  arg <- fmap mk_val $ newResiduation scope
  res <- fmap mk_val $ newResiduation scope
  let bt = Explicit
  unify scope tau (VProd bt identW arg res)
  return (bt,identW,arg,res)

unifyTbl :: Scope -> Rho -> EvalM (Sigma, Rho)
unifyTbl scope (VTable arg res) =
  return (arg,res)
unifyTbl scope tau = do
  let mk_val i = VMeta i []
  arg <- fmap mk_val $ newResiduation scope
  res <- fmap mk_val $ newResiduation scope
  unify scope tau (VTable arg res)
  return (arg,res)

unify scope (VApp f1 vs1) (VApp f2 vs2)
  | f1 == f2  = sequence_ (zipWith (unify scope) vs1 vs2)
unify scope (VMeta i vs1) (VMeta j vs2)
  | i  == j   = sequence_ (zipWith (unify scope) vs1 vs2)
  | otherwise = do 
      mv <- getMeta i
      case mv of
        Bound _ v1 -> do
          g <- globals
          unify scope (apply g v1 vs1) (VMeta j vs2)
        Residuation scope1 _ -> do
          mv <- getMeta j
          case mv of
            Bound _ v2 -> do
              g <- globals
              unify scope (VMeta i vs1) (apply g v2 vs2)
            Residuation scope2 _
              | m > n     -> setMeta i (Bound scope1 (VMeta j vs2))
              | otherwise -> setMeta j (Bound scope2 (VMeta i vs2))
              where
                m = length scope1
                n = length scope2 
unify scope (VMeta i vs) v = unifyVar scope i vs v
unify scope v (VMeta i vs) = unifyVar scope i vs v
unify scope (VGen i vs1)       (VGen j vs2)
  | i == j                     = sequence_ (zipWith (unify scope) vs1 vs2)
unify scope (VProd b x d cod) (VProd b' x' d' cod')
  | b == b'                    = do
      unify scope d d'
      cod <- evalCodomain scope x cod
      cod' <- evalCodomain scope x' cod'
      unify scope cod cod'
unify scope (VTable p1 res1) (VTable p2 res2) = do
  unify scope p2   p1
  unify scope res1 res2
unify scope (VSort s1) (VSort s2)
  | s1 == s2                   = return ()
unify scope (VInt i)  (VInt j)
  | i == j                     = return ()
unify scope (VFlt x)  (VFlt y)
  | x == y                     = return ()
unify scope (VStr s1) (VStr s2)
  | s1 == s2                   = return ()
unify scope VEmpty VEmpty      = return ()
unify scope v1 v2 = do
  t1 <- value2termM False (scopeVars scope) v1
  t2 <- value2termM False (scopeVars scope) v2
  evalError ("Cannot unify terms:" <+> (ppTerm Unqualified 0 t1 $$
                                        ppTerm Unqualified 0 t2))


-- | Invariant: tv1 is a flexible type variable
unifyVar :: Scope -> MetaId -> [Value] -> Tau -> EvalM ()
unifyVar scope metaid vs ty2 = do            -- Check whether i is bound
  mv <- getMeta metaid
  case mv of
    Bound _ ty1          -> do g <- globals
                               unify scope (apply g ty1 vs) ty2
    Residuation scope' _ -> do occursCheck scope' metaid scope ty2
                               setMeta metaid (Bound scope' ty2)

occursCheck scope' i0 scope v =
  let m = length scope'
      n = length scope
  in check m n v
  where
    check m n (VApp f vs) = mapM_ (check m n) vs
    check m n (VMeta i vs)
      | i0 == i  = do ty1 <- value2termM False (scopeVars scope) (VMeta i vs)
                      ty2 <- value2termM False (scopeVars scope) v
                      evalError ("Occurs check for" <+> ppTerm Unqualified 0 ty1 <+> "in:" $$
                                 nest 2 (ppTerm Unqualified 0 ty2))
      | otherwise = do
          s <- getMeta i
          case s of
            Bound _ v -> do g <- globals
                            check m n (apply g v vs)
            _         -> mapM_ (check m n) vs
    check m n (VGen i vs)
      | i > m     = let (v,_) = reverse scope !! i
                    in evalError ("Variable" <+> pp v <+> "has escaped")
      | otherwise = mapM_ (check m n) vs
    check m n (VClosure env c (Abs bt x t)) = do
      g <- globals
      check (m+1) (n+1) (eval g ((x,VGen n []):env) c t [])
    check m n (VProd bt x ty1 ty2) = do
      check m n ty1
      case ty2 of
        VClosure env c t -> do g <- globals
                               check (m+1) (n+1) (eval g ((x,VGen n []):env) c t [])
        _                -> check m n ty2
    check m n (VRecType as) =
      mapM_ (\(lbl,v) -> check m n v) as
    check m n (VR as) =
      mapM_ (\(lbl,v) -> check m n v) as
    check m n (VP v l vs) =
      check m n v >> mapM_ (check m n) vs
    check m n (VExtR v1 v2) =
      check m n v1 >> check m n v2
    check m n (VTable v1 v2) =
      check m n v1 >> check m n v2
    check m n (VT ty env c cs) =
      check m n ty      -- Traverse cs as well
    check m n (VV ty cs) =
      check m n ty >> mapM_ (check m n) cs
    check m n (VS v1 v2 vs) =
      check m n v1 >> check m n v2 >> mapM_ (check m n) vs
    check m n (VSort _) = return ()
    check m n (VInt _) = return ()
    check m n (VFlt _) = return ()
    check m n (VStr _) = return ()
    check m n VEmpty   = return ()
    check m n (VC v1 v2) =
      check m n v1 >> check m n v2
    check m n (VGlue v1 v2) =
      check m n v1 >> check m n v2
    check m n (VPatt _ _ _) = return ()
    check m n (VPattType v) =
      check m n v
    check m n (VFV c vs) =
      mapM_ (check m n) vs
    check m n (VAlts v vs) =
      check m n v >> mapM_ (\(v1,v2) -> check m n v1 >> check m n v2) vs
    check m n (VStrs vs) =
      mapM_ (check m n) vs

-----------------------------------------------------------------------
-- Instantiation and quantification
-----------------------------------------------------------------------

-- | Instantiate the topmost implicit arguments with metavariables
instantiate :: Scope -> Term -> Sigma -> EvalM (Term,Rho)
instantiate scope t (VProd Implicit x ty1 ty2) = do
  i <- newResiduation scope
  ty2 <- case ty2 of
           VClosure env c ty2 -> do g <- globals
                                    return (eval g ((x,VMeta i []):env) c ty2 [])
           ty2                -> return ty2
  instantiate scope (App t (ImplArg (Meta i))) ty2
instantiate scope t ty@(VMeta i args) = getMeta i >>= \case
  Bound _ v              -> instantiate scope t v
  Residuation _ (Just v) -> instantiate scope t v
  _                      -> return (t,ty) -- We don't have enough information to try any instantiation
instantiate scope t ty = do
  return (t,ty)

-- | Build fresh lambda abstractions for the topmost implicit arguments
skolemise :: Scope -> Sigma -> EvalM (Scope, Term->Term, Rho)
skolemise scope ty@(VMeta i vs) = do
  mv <- getMeta i
  case mv of
    Residuation _ _ -> return (scope,id,ty)                   -- guarded constant?
    Bound _ ty      -> do g <- globals
                          skolemise scope (apply g ty vs)
skolemise scope (VProd Implicit x ty1 ty2) = do
  let v = newVar scope
  ty2 <- evalCodomain scope x ty2
  (scope,f,ty2) <- skolemise ((v,ty1):scope) ty2
  return (scope,Abs Implicit v . f,ty2)
skolemise scope ty = do
  return (scope,id,ty)

-- | Quantify over the specified type variables (all flexible)
quantify :: Scope -> Term -> [MetaId] -> Rho -> EvalM (Term,Sigma)
quantify scope t tvs ty = do
  let m = length tvs
      n = length scope
  (used_bndrs,ty) <- check m n [] ty
  let new_bndrs  = take m (allBinders \\ used_bndrs)
  mapM_ (bind ([(var,VSort cType)|var <- new_bndrs]++scope)) (zip3 [0..] tvs new_bndrs)
  let ty' = foldr (\ty -> VProd Implicit ty vtypeType) ty new_bndrs
  return (foldr (Abs Implicit) t new_bndrs,ty')
  where
    bind scope (i, meta_id, name) = setMeta meta_id (Bound scope (VGen i []))

    check m n xs (VApp f vs)       = do
      (xs,vs) <- mapAccumM (check m n) xs vs
      return (xs,VApp f vs)
    check m n xs (VMeta i vs)      = do
      s <- getMeta i
      case s of
        Bound _ v -> do g <- globals
                        check m n xs (apply g v vs)
        _         -> do (xs,vs) <- mapAccumM (check m n) xs vs
                        return (xs,VMeta i vs)
    check m n st (VGen i vs)= do
      (st,vs) <- mapAccumM (check m n) st vs
      return (st, VGen (m+i) vs)
    check m n st (VClosure env c (Abs bt x t)) = do
      (st,env) <- mapAccumM (\st (x,v) -> check m n st v >>= \(st,v) -> return (st,(x,v))) st env
      return (st,VClosure env c (Abs bt x t))
    check m n xs (VProd bt x v1 v2) = do
      (xs,v1) <- check m n xs v1
      case v2 of
        VClosure env c t -> do (st,env) <- mapAccumM (\xs (x,tnk) -> check m n xs tnk >>= \(xs,tnk) -> return (xs,(x,tnk))) xs env
                               return (x:xs,VProd bt x v1 (VClosure env c t))
        v2               -> do (xs,v2) <- check m (n+1) xs v2
                               return (x:xs,VProd bt x v1 v2)
    check m n xs (VRecType as)     = do
      (xs,as) <- mapAccumM (\xs (l,v) -> check m n xs v >>= \(xs,v) -> return (xs,(l,v))) xs as
      return (xs,VRecType as)
    check m n xs (VR as)           = do
      (xs,as) <- mapAccumM (\xs (lbl,tnk) -> check m n xs tnk >>= \(xs,tnk) -> return (xs,(lbl,tnk))) xs as
      return (xs,VR as)
    check m n xs (VP v l vs)       = do
      (xs,v)  <- check m n xs v
      (xs,vs) <- mapAccumM (check m n) xs vs
      return (xs,VP v l vs)
    check m n xs (VExtR v1 v2)     = do
      (xs,v1) <- check m n xs v1
      (xs,v2) <- check m n xs v2
      return (xs,VExtR v1 v2)
    check m n xs (VTable v1 v2)    = do
      (xs,v1) <- check m n xs v1
      (xs,v2) <- check m n xs v2
      return (xs,VTable v1 v2)
    check m n xs (VT ty env c cs)    = do
      (xs,ty) <- check m n xs ty
      (xs,env) <- mapAccumM (\xs (x,tnk) -> check m n xs tnk >>= \(xs,tnk) -> return (xs,(x,tnk))) xs env
      return (xs,VT ty env c cs)
    check m n xs (VV ty cs)        = do
      (xs,ty) <- check m n xs ty
      (xs,cs) <- mapAccumM (check m n) xs cs
      return (xs,VV ty cs)
    check m n xs (VS v1 tnk vs)    = do
      (xs,v1)  <- check m n xs v1
      (xs,tnk) <- check m n xs tnk
      (xs,vs)  <- mapAccumM (check m n) xs vs
      return (xs,VS v1 tnk vs)
    check m n xs v@(VSort _)       = return (xs,v)
    check m n xs v@(VInt _)        = return (xs,v)
    check m n xs v@(VFlt _)        = return (xs,v)
    check m n xs v@(VStr _)        = return (xs,v)
    check m n xs v@VEmpty          = return (xs,v)
    check m n xs (VC v1 v2)        = do
      (xs,v1) <- check m n xs v1
      (xs,v2) <- check m n xs v2
      return (xs,VC v1 v2)
    check m n xs (VGlue v1 v2)        = do
      (xs,v1) <- check m n xs v1
      (xs,v2) <- check m n xs v2
      return (xs,VGlue v1 v2)
    check m n xs v@(VPatt _ _ _)   = return (xs,v)
    check m n xs (VPattType v)     = do
      (xs,v) <- check m n xs v
      return (xs,VPattType v)
    check m n xs (VFV c vs)       = do
      (xs,vs) <- mapAccumM (check m n) xs vs
      return (xs,VFV c vs)
    check m n xs (VAlts v vs)      = do
      (xs,v)  <- check m n xs v
      (xs,vs) <- mapAccumM (\xs (v1,v2) -> do (xs,v1) <- check m n xs v1
                                              (xs,v2) <- check m n xs v2
                                              return (xs,(v1,v2)))
                           xs vs
      return (xs,VAlts v vs)
    check m n xs (VStrs vs)        = do
      (xs,vs) <- mapAccumM (check m n) xs vs
      return (xs,VStrs vs)
    check m n xs v = unimplemented ("check "++show (ppValue Unqualified 5 v))

    mapAccumM :: Monad m => (a -> b -> m (a,c)) -> a -> [b] -> m (a,[c])
    mapAccumM f s []     = return (s,[])
    mapAccumM f s (x:xs) = do
      (s,y)  <- f s x
      (s,ys) <- mapAccumM f s xs
      return (s,y:ys)

allBinders :: [Ident]    -- a,b,..z, a1, b1,... z1, a2, b2,...
allBinders = [ identS [x]          | x <- ['a'..'z'] ] ++
             [ identS (x : show i) | i <- [1 :: Integer ..], x <- ['a'..'z']]

-----------------------------------------------------------------------
-- Helpers
-----------------------------------------------------------------------

type Sigma = Value
type Rho   = Value -- No top-level ForAll
type Tau   = Value -- No ForAlls anywhere

unimplemented str = fail ("Unimplemented: "++str)

newVar :: Scope -> Ident
newVar scope = head [x | i <- [1..],
                         let x = identS ('v':show i),
                         isFree scope x]
  where
    isFree []            x = True
    isFree ((y,_):scope) x = x /= y && isFree scope x

scopeEnv   scope = zipWith (\(x,ty) i -> (x,VGen i [])) (reverse scope) [0..]
scopeVars  scope = map fst scope
scopeTypes scope = zipWith (\(_,ty) scope -> (scope,ty)) scope (tails scope)

-- | This function takes account of zonking, and returns a set
-- (no duplicates) of unbound meta-type variables
getMetaVars :: [(Scope,Sigma)] -> EvalM [MetaId]
getMetaVars sc_tys = foldM (\acc (scope,ty) -> go acc ty) [] sc_tys
  where
    -- Get the MetaIds from a term; no duplicates in result
    go acc (VGen i args)     = foldM go acc args
    go acc (VSort s)         = return acc
    go acc (VInt _)          = return acc
    go acc (VRecType as)     = foldM (\acc (lbl,v) -> go acc v) acc as
    go acc (VClosure _ _ _)  = return acc
    go acc (VProd b x v1 v2) = go acc v2 >>= \acc -> go acc v1
    go acc (VTable v1 v2)    = go acc v2 >>= \acc -> go acc v1
    go acc (VMeta m args)
      | m `elem` acc         = return acc
      | otherwise            = do res <- getMeta m
                                  case res of
                                    Bound _ v              -> go acc v
                                    Residuation _ Nothing  -> foldM go (m:acc) args
                                    Residuation _ (Just v) -> go acc v
                                    _                      -> return acc
    go acc (VApp f args)     = foldM go acc args
    go acc (VFV c vs)        = foldM go acc vs
    go acc (VCInts _ _)      = return acc
    go acc v                 = unimplemented ("go "++show (ppValue Unqualified 5 v))

-- | Eliminate any substitutions in a term
zonkTerm :: [Ident] -> Term -> EvalM Term
zonkTerm xs (Abs b x t) = do
  t <- zonkTerm (x:xs) t
  return (Abs b x t)
zonkTerm xs (Prod b x t1 t2) = do
  t1 <- zonkTerm xs  t1
  t2 <- zonkTerm xs' t2
  return (Prod b x t1 t2)
  where
    xs' | x == identW = xs
        | otherwise   = x:xs
zonkTerm xs (Meta i) = do
  st <- getMeta i
  case st of
    Bound _ v           -> zonkTerm xs =<< value2termM False xs v
    Residuation scope v -> case v of
                            Just v  -> zonkTerm xs =<< value2termM False (map fst scope) v
                            Nothing -> return (Meta i)
    Narrowing _         -> return (Meta i)
zonkTerm xs t = composOp (zonkTerm xs) t
