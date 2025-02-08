{-# LANGUAGE RankNTypes, CPP, TupleSections #-}
module GF.Compile.TypeCheck.ConcreteNew( checkLType, checkLType', inferLType, inferLType' ) where

-- The code here is based on the paper:
-- Simon Peyton Jones, Dimitrios Vytiniotis, Stephanie Weirich.
-- Practical type inference for arbitrary-rank types.
-- 14 September 2011

import GF.Grammar hiding (Env, VGen, VApp, VRecType)
import GF.Grammar.Lookup
import GF.Grammar.Predef
import GF.Grammar.Lockfield
import GF.Compile.Compute.Concrete
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

checkLType :: Globals -> Term -> Type -> Check (Term, Type)
checkLType globals t ty = runEvalOneM globals (checkLType' t ty)

checkLType' :: Term -> Type -> EvalM s (Term, Type)
checkLType' t ty = do
  vty <- eval [] ty []
  (t,_) <- tcRho [] t (Just vty)
  t <- zonkTerm [] t
  return (t,ty)

inferLType :: Globals -> Term -> Check (Term, Type)
inferLType globals t = runEvalOneM globals (inferLType' t)

inferLType' :: Term -> EvalM s (Term, Type)
inferLType' t = do
  (t,ty) <- inferSigma [] t
  t  <- zonkTerm [] t
  ty <- value2term False [] ty
  return (t,ty)

inferSigma :: Scope s -> Term -> EvalM s (Term,Sigma s)
inferSigma scope t = do                                      -- GEN1
  (t,ty) <- tcRho scope t Nothing
  env_tvs <- getMetaVars (scopeTypes scope)
  res_tvs <- getMetaVars [(scope,ty)]
  let forall_tvs = res_tvs \\ env_tvs
  quantify scope t forall_tvs ty

vtypeInt   = VApp (cPredef,cInt) []
vtypeFloat = VApp (cPredef,cFloat) []
vtypeInts i= newEvaluatedThunk (VInt i) >>= \tnk -> return (VApp (cPredef,cInts) [tnk])
vtypeStr   = VSort cStr
vtypeStrs  = VSort cStrs
vtypeType  = VSort cType
vtypePType = VSort cPType
vtypeMarkup= VApp (cPredef,cMarkup) []

tcRho :: Scope s -> Term -> Maybe (Rho s) -> EvalM s (Term, Rho s)
tcRho scope t@(EInt i)   mb_ty = vtypeInts i >>= \sigma -> instSigma scope t sigma mb_ty -- INT
tcRho scope t@(EFloat _) mb_ty = instSigma scope t vtypeFloat mb_ty    -- FLOAT
tcRho scope t@(K _)      mb_ty = instSigma scope t vtypeStr   mb_ty    -- STR
tcRho scope t@(Empty)    mb_ty = instSigma scope t vtypeStr   mb_ty
tcRho scope t@(Vr v)     mb_ty = do                          -- VAR
  case lookup v scope of
    Just v_sigma -> instSigma scope t v_sigma mb_ty
    Nothing      -> evalError ("Unknown variable" <+> v)
tcRho scope t@(Q id)     mb_ty = do
  (t,ty) <- tcApp scope t t []
  instSigma scope t ty mb_ty
tcRho scope t@(QC id)    mb_ty = do
  (t,ty) <- tcApp scope t t []
  instSigma scope t ty mb_ty
tcRho scope t@(App fun arg) mb_ty = do
  (t,ty) <- tcApp scope t t []
  instSigma scope t ty mb_ty
tcRho scope (Abs bt var body) Nothing = do                   -- ABS1
  (i,tnk) <- newResiduation scope
  let arg_ty = VMeta tnk []
  (body,body_ty) <- tcRho ((var,arg_ty):scope) body Nothing
  let m = length scope
      n = m+1
  (b,used_bndrs) <- check m n (False,[]) body_ty
  if b
    then let v = head (allBinders \\ used_bndrs)
         in return (Abs bt var body, (VProd bt v arg_ty body_ty))
    else return (Abs bt var body, (VProd bt identW arg_ty body_ty))
  where
    check m n st (VApp f vs)       = foldM (follow m n) st vs
    check m n st (VMeta i vs)      = do
      s <- getRef i
      case s of
        Evaluated _ v -> do v <- apply v vs
                            check m n st v
        _             -> foldM (follow m n) st vs
    check m n st@(b,xs) (VGen i vs)
      | i == m                     = return (True, xs)
      | otherwise                  = return st
    check m n st (VClosure env (Abs bt x t)) = do
      tnk <- newEvaluatedThunk (VGen n [])
      v <- eval ((x,tnk):env) t []
      check m (n+1) st v
    check m n st (VProd _ x v1 v2) = do
      st@(b,xs) <- check m n st v1
      case v2 of
        VClosure env t -> do tnk <- newEvaluatedThunk (VGen n [])
                             v2 <- eval ((x,tnk):env) t []
                             check m (n+1) (b,x:xs) v2
        v2             -> check m n st v2
    check m n st (VRecType as)     = foldM (\st (l,v) -> check m n st v) st as
    check m n st (VR as)           =
      foldM (\st (lbl,tnk) -> follow m n st tnk) st as
    check m n st (VP v l vs)       =
      check m n st v >>= \st -> foldM (follow m n) st vs
    check m n st (VExtR v1 v2)     =
      check m n st v1 >>= \st -> check m n st v2
    check m n st (VTable v1 v2)    =
      check m n st v1 >>= \st -> check m n st v2
    check m n st (VT ty env cs)    =
      check m n st ty    -- Traverse cs as well
    check m n st (VV ty cs)        =
      check m n st ty >>= \st -> foldM (follow m n) st cs
    check m n st (VS v1 tnk vs)    = do
      st <- check m n st v1
      st <- follow m n st tnk
      foldM (follow m n) st vs
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
    check m n st v                 = error (showValue v)

    follow m n st tnk = force tnk >>= \v -> check m n st v

tcRho scope t@(Abs Implicit var body) (Just ty) = do         -- ABS2
  (bt, x, var_ty, body_ty) <- unifyFun scope ty
  if bt == Implicit
    then return ()
    else evalError (ppTerm Unqualified 0 t <+> "is an implicit function, but no implicit function is expected")
  body_ty <- case body_ty of
               VClosure env body_ty -> do tnk <- newEvaluatedThunk (VGen (length scope) [])
                                          eval ((x,tnk):env) body_ty []
               body_ty              -> return body_ty
  (body, body_ty) <- tcRho ((var,var_ty):scope) body (Just body_ty)
  return (Abs Implicit var body,ty)
tcRho scope (Abs Explicit var body) (Just ty) = do           -- ABS3
  (scope,f,ty') <- skolemise scope ty
  (_,x,var_ty,body_ty) <- unifyFun scope ty'
  body_ty <- case body_ty of
               VClosure env body_ty -> do tnk <- newEvaluatedThunk (VGen (length scope) [])
                                          eval ((x,tnk):env) body_ty []
               body_ty              -> return body_ty
  (body, body_ty) <- tcRho ((var,var_ty):scope) body (Just body_ty)
  return (f (Abs Explicit var body),ty)
tcRho scope (Meta _) mb_ty = do
  (i,_) <- newResiduation scope
  ty <- case mb_ty of
          Just ty -> return ty
          Nothing -> do (_,tnk) <- newResiduation scope
                        return (VMeta tnk [])
  return (Meta i, ty)
tcRho scope (Let (var, (mb_ann_ty, rhs)) body) mb_ty = do    -- LET
  (rhs,var_ty) <- case mb_ann_ty of
                    Nothing     -> tcRho scope rhs Nothing
                    Just ann_ty -> do (ann_ty, _) <- tcRho scope ann_ty (Just vtypeType)
                                      env <- scopeEnv scope
                                      v_ann_ty <- eval env ann_ty []
                                      (rhs,_) <- tcRho scope rhs (Just v_ann_ty)
                                      return (rhs, v_ann_ty)
  (body, body_ty) <- tcRho ((var,var_ty):scope) body mb_ty
  var_ty <- value2term True (scopeVars scope) var_ty
  return (Let (var, (Just var_ty, rhs)) body, body_ty)
tcRho scope (Typed body ann_ty) mb_ty = do                   -- ANNOT
  (ann_ty, _) <- tcRho scope ann_ty (Just vtypeType)
  env <- scopeEnv scope
  v_ann_ty <- eval env ann_ty []
  (body,_) <- tcRho scope body (Just v_ann_ty)
  (res1,res2) <- instSigma scope (Typed body ann_ty) v_ann_ty mb_ty
  return (res1,res2)
tcRho scope (FV ts) mb_ty = do
  (ty,subsume) <-
    case mb_ty of
      Just ty -> do return (ty, \t ty' -> return t)
      Nothing -> do (i,tnk) <- newResiduation scope
                    let ty = VMeta tnk []
                    return (ty, \t ty' -> subsCheckRho scope t ty' ty)

  let go t = do (t, ty) <- tcRho scope t mb_ty
                subsume t ty

  ts <- mapM go ts
  return (FV ts, ty)
tcRho scope t@(Sort s) mb_ty = do
  instSigma scope t vtypeType mb_ty
tcRho scope t@(RecType rs) Nothing   = do
  (rs,mb_ty) <- tcRecTypeFields scope rs Nothing
  return (RecType rs,fromMaybe vtypePType mb_ty)
tcRho scope t@(RecType rs) (Just ty) = do
  (scope,f,ty') <- skolemise scope ty
  case ty' of
    VSort s
      | s == cType  -> return ()
      | s == cPType -> return ()
    VMeta i vs  -> case rs of
                     [] -> unifyVar scope i vs vtypePType
                     _  -> return ()
    ty          -> do ty <- value2term False (scopeVars scope) ty
                      evalError ("The record type" <+> ppTerm Unqualified 0 t $$
                                 "cannot be of type" <+> ppTerm Unqualified 0 ty)
  (rs,mb_ty) <- tcRecTypeFields scope rs (Just ty')
  return (f (RecType rs),ty)
tcRho scope t@(Table p res) mb_ty = do
  (p,  p_ty)   <- tcRho scope p   (Just vtypePType)
  (res,res_ty) <- tcRho scope res (Just vtypeType)
  instSigma scope (Table p res) vtypeType mb_ty
tcRho scope (Prod bt x ty1 ty2) mb_ty = do
  (ty1,ty1_ty) <- tcRho scope ty1 (Just vtypeType)
  env <- scopeEnv scope
  vty1 <- eval env ty1 []
  (ty2,ty2_ty) <- tcRho ((x,vty1):scope) ty2 (Just vtypeType)
  instSigma scope (Prod bt x ty1 ty2) vtypeType mb_ty
tcRho scope (S t p) mb_ty = do
  let mk_val (_,tnk) = VMeta tnk []
  p_ty   <- fmap mk_val $ newResiduation scope
  res_ty <- case mb_ty of
              Nothing -> fmap mk_val $ newResiduation scope
              Just ty -> return ty
  let t_ty = VTable p_ty res_ty
  (t,t_ty) <- tcRho scope t (Just t_ty)
  (p,_) <- tcRho scope p (Just p_ty)
  return (S t p, res_ty)
tcRho scope (T tt ps) Nothing = do                           -- ABS1/AABS1 for tables
  let mk_val (_,tnk) = VMeta tnk []
  p_ty   <- case tt of
              TRaw      -> fmap mk_val $ newResiduation scope
              TTyped ty -> do (ty, _) <- tcRho scope ty (Just vtypeType)
                              env <- scopeEnv scope
                              eval env ty []
  res_ty <- fmap mk_val $ newResiduation scope
  ps <- tcCases scope ps p_ty res_ty
  p_ty_t <- value2term True [] p_ty
  return (T (TTyped p_ty_t) ps, VTable p_ty res_ty)
tcRho scope (T tt ps) (Just ty) = do                         -- ABS2/AABS2 for tables
  (scope,f,ty') <- skolemise scope ty
  (p_ty, res_ty) <- unifyTbl scope ty'
  case tt of
    TRaw      -> return ()
    TTyped ty -> do (ty, _) <- tcRho scope ty (Just vtypeType)
                    env <- scopeEnv scope
                    ty <- eval env ty []
                    unify scope ty p_ty
  ps <- tcCases scope ps p_ty res_ty
  p_ty_t <- value2term True (scopeVars scope) p_ty
  return (f (T (TTyped p_ty_t) ps), VTable p_ty res_ty)
tcRho scope (V p_ty ts) Nothing = do
  (p_ty, _) <- tcRho scope p_ty (Just vtypeType)
  (i,tnk) <- newResiduation scope
  let res_ty = VMeta tnk []

  let go t = do (t, ty) <- tcRho scope t Nothing
                subsCheckRho scope t ty res_ty

  ts <- mapM go ts
  env <- scopeEnv scope
  p_vty <- eval env p_ty []
  return (V p_ty ts, VTable p_vty res_ty)
tcRho scope (V p_ty0 ts) (Just ty) = do
  (scope,f,ty') <- skolemise scope ty
  (p_ty, res_ty) <- unifyTbl scope ty'
  (p_ty0, _) <- tcRho scope p_ty0 (Just vtypeType)
  env <- scopeEnv scope
  p_vty0 <- eval env p_ty0 []
  unify scope p_ty p_vty0
  ts <- mapM (\t -> fmap fst $ tcRho scope t (Just res_ty)) ts
  return (V p_ty0 ts, VTable p_ty res_ty)
tcRho scope (R rs) Nothing = do
  lttys <- inferRecFields scope rs
  rs <- mapM (\(l,t,ty) -> value2term True (scopeVars scope) ty >>= \ty -> return (l, (Just ty, t))) lttys
  return (R        rs,
          VRecType [(l, ty) | (l,t,ty) <- lttys]
         )
tcRho scope (R rs) (Just ty) = do
  (scope,f,ty') <- skolemise scope ty
  case ty' of
    (VRecType ltys) -> do lttys <- checkRecFields scope rs ltys
                          rs <- mapM (\(l,t,ty) -> value2term True (scopeVars scope) ty >>= \ty -> return (l, (Just ty, t))) lttys
                          return ((f . R)  rs,
                                  VRecType [(l, ty) | (l,t,ty) <- lttys]
                                 )
    ty              -> do lttys <- inferRecFields scope rs
                          t <- liftM (f . R) (mapM (\(l,t,ty) -> value2term True (scopeVars scope) ty >>= \ty -> return (l, (Just ty, t))) lttys)
                          let ty' = VRecType [(l, ty) | (l,t,ty) <- lttys]
                          t <- subsCheckRho scope t ty' ty
                          return (t, ty')
tcRho scope (P t l) mb_ty = do
  l_ty   <- case mb_ty of
              Just ty -> return ty
              Nothing -> do (i,tnk) <- newResiduation scope
                            return (VMeta tnk [])
  (t,t_ty) <- tcRho scope t (Just (VRecType [(l,l_ty)]))
  return (P t l,l_ty)
tcRho scope (C t1 t2) mb_ty = do
  (t1,t1_ty) <- tcRho scope t1 (Just vtypeStr)
  (t2,t2_ty) <- tcRho scope t2 (Just vtypeStr)
  instSigma scope (C t1 t2) vtypeStr mb_ty
tcRho scope (Glue t1 t2) mb_ty = do
  (t1,t1_ty) <- tcRho scope t1 (Just vtypeStr)
  (t2,t2_ty) <- tcRho scope t2 (Just vtypeStr)
  instSigma scope (Glue t1 t2) vtypeStr mb_ty
tcRho scope t@(ExtR t1 t2) mb_ty = do
  (t1,t1_ty) <- tcRho scope t1 Nothing
  (t2,t2_ty) <- tcRho scope t2 Nothing
  case (t1_ty,t2_ty) of
    (VSort s1,VSort s2)
       | (s1 == cType || s1 == cPType) &&
         (s2 == cType || s2 == cPType) -> let sort | s1 == cPType && s2 == cPType = cPType
                                                   | otherwise                    = cType
                                          in instSigma scope (ExtR t1 t2) (VSort sort) mb_ty
    (VRecType rs1, VRecType rs2)       -> instSigma scope (ExtR t1 t2) (VRecType (rs2++rs1)) mb_ty
    _                                  -> evalError ("Cannot type check" <+> ppTerm Unqualified 0 t)
tcRho scope (ELin cat t) mb_ty = do  -- this could be done earlier, i.e. in the parser
  tcRho scope (ExtR t (R [(lockLabel cat,(Just (RecType []),R []))])) mb_ty
tcRho scope (ELincat cat t) mb_ty = do  -- this could be done earlier, i.e. in the parser
  tcRho scope (ExtR t (RecType [(lockLabel cat,RecType [])])) mb_ty
tcRho scope (Alts t ss) mb_ty = do
  (t,_) <- tcRho scope t (Just vtypeStr)
  ss    <- flip mapM ss $ \(t1,t2) -> do
              (t1,_) <- tcRho scope t1 (Just vtypeStr)
              (t2,_) <- tcRho scope t2 (Just vtypeStrs)
              return (t1,t2)
  instSigma scope (Alts t ss) vtypeStr mb_ty
tcRho scope (Strs ss) mb_ty = do
  ss <- flip mapM ss $ \t -> do
           (t,_) <- tcRho scope t (Just vtypeStr)
           return t
  instSigma scope (Strs ss) vtypeStrs mb_ty
tcRho scope (EPattType ty) mb_ty = do
  (ty, _) <- tcRho scope ty (Just vtypeType)
  instSigma scope (EPattType ty) vtypeType mb_ty
tcRho scope t@(EPatt min max p) mb_ty = do
  (scope,f,ty) <- case mb_ty of
                    Nothing -> do (i,tnk) <- newResiduation scope
                                  return (scope,id,VMeta tnk [])
                    Just ty -> do (scope,f,ty) <- skolemise scope ty
                                  case ty of
                                    VPattType ty -> return (scope,f,ty)
                                    _            -> evalError (ppTerm Unqualified 0 t <+> "must be of pattern type but" <+> ppTerm Unqualified 0 t <+> "is expected")
  tcPatt scope p ty
  return (f (EPatt min max p), ty)
tcRho scope (Markup tag attrs children) mb_ty = do
  attrs <- forM attrs $ \(id,t) -> do
              (t,_) <- tcRho scope t Nothing
              return (id,t)
  res <- mapM (\child -> tcRho scope child Nothing) children
  instSigma scope (Markup tag attrs (map fst res)) vtypeMarkup mb_ty
tcRho scope (Reset c t) mb_ty = do
  (t,_) <- tcRho scope t Nothing
  instSigma scope (Reset c t) vtypeMarkup mb_ty
tcRho scope t _ = unimplemented ("tcRho "++show t)

tcCases scope []         p_ty res_ty = return []
tcCases scope ((p,t):cs) p_ty res_ty = do
  scope' <- tcPatt scope p p_ty
  (t,_)  <- tcRho scope' t (Just res_ty)
  cs <- tcCases scope cs p_ty res_ty
  return ((p,t):cs)

tcApp scope t0 (App fun arg) args = tcApp scope t0 fun (arg:args)     -- APP
tcApp scope t0 (Q id)        args = resolveOverloads scope t0 id args -- VAR (global)
tcApp scope t0 (QC id)       args = resolveOverloads scope t0 id args -- VAR (global)
tcApp scope t0 t args = do
  (t,ty) <- tcRho scope t Nothing
  reapply scope t ty args

reapply :: Scope s -> Term -> Constraint s -> [Term] -> EvalM s (Term,Rho s)
reapply scope fun fun_ty [] = return (fun,fun_ty)
reapply scope fun fun_ty ((ImplArg arg):args) = do -- Implicit arg case
  (bt, x, arg_ty, res_ty) <- unifyFun scope fun_ty
  unless (bt == Implicit) $ evalError (ppTerm Unqualified 0 (App fun (ImplArg arg)) <+>
                              "is an implicit argument application, but no implicit argument is expected")
  (arg,_) <- tcRho scope arg (Just arg_ty)
  res_ty <- case res_ty of
              VClosure res_env res_ty -> do env <- scopeEnv scope
                                            tnk <- newThunk env arg
                                            eval ((x,tnk):res_env) res_ty []
              res_ty                  -> return res_ty
  reapply scope (App fun (ImplArg arg)) res_ty args
reapply scope fun fun_ty (arg:args) = do -- Explicit arg (fallthrough) case
  (fun,fun_ty) <- instantiate scope fun fun_ty
  (_, x, arg_ty, res_ty) <- unifyFun scope fun_ty
  (arg,_) <- tcRho scope arg (Just arg_ty)
  res_ty <- case res_ty of
              VClosure res_env res_ty -> do env <- scopeEnv scope
                                            tnk <- newThunk env arg
                                            eval ((x,tnk):res_env) res_ty []
              res_ty                  -> return res_ty
  reapply scope (App fun arg) res_ty args

resolveOverloads :: Scope s -> Term -> QIdent -> [Term] -> EvalM s (Term,Rho s)
resolveOverloads scope t q args = EvalM $ \gl@(Gl gr _) k mt d r msgs ->
  case lookupOverloadTypes gr q of
    Bad msg  -> return $ Fail (pp msg) msgs
    Ok [tty] -> try tty gl k mt d r msgs -- skip overload resolution if there's only one overload
    Ok ttys  -> do rs <- mapM (\tty -> (tty,) <$> try tty gl k mt d r msgs) ttys
                   let successes = mapMaybe isSuccess rs
                   r <- case successes of
                     []           -> return $ Fail mempty msgs
                     [(_,r,msgs)] -> return $ Success r msgs
                     _            -> case unifyOverloads (successes <&> \(tty,_,_) -> tty) of
                                       EvalM f -> f gl k mt d r msgs
                   return $ case r of
                     s@(Success _ _) -> s
                     Fail err msgs   -> let h = "Overload resolution failed" $$
                                                "of term   " <+> pp t $$
                                                "with types" <+> vcat [ppTerm Terse 0 ty | (_,ty) <- ttys]
                                        in Fail (h $+$ err) msgs
  where
    try (t,ty) = case eval [] ty [] >>= \vty -> reapply scope t vty args of EvalM f -> f

    isSuccess (tty, Success r msg) = Just (tty,r,msg)
    isSuccess (_, Fail _ _)        = Nothing

    unifyOverloads ttys = do
      ttys <- forM ttys $ \(t,ty) -> do
        vty <- eval [] ty []
        (t,vty) <- papply scope t vty args
        return (t,vty)
      (_,tnk) <- newResiduation scope
      let mv = VMeta tnk []
      mapM_ (\(_,vty) -> unify scope vty mv) ttys
      fvty <- force tnk
      return (FV (fst <$> ttys), fvty)

    papply scope fun fun_ty [] = return (fun,fun_ty)
    papply scope fun (VProd Implicit x arg_ty res_ty) ((ImplArg arg):args) = do -- Implicit arg case
      (arg,_) <- tcRho scope arg (Just arg_ty)
      res_ty <- case res_ty of
                  VClosure res_env res_ty -> do env <- scopeEnv scope
                                                tnk <- newThunk env arg
                                                eval ((x,tnk):res_env) res_ty []
                  res_ty                  -> return res_ty
      papply scope (App fun (ImplArg arg)) res_ty args
    papply scope fun fun_ty (arg:args) = do -- Explicit arg (fallthrough) case
      (fun,VProd Explicit x arg_ty res_ty) <- instantiate scope fun fun_ty
      (arg,_) <- tcRho scope arg (Just arg_ty)
      res_ty <- case res_ty of
                  VClosure res_env res_ty -> do env <- scopeEnv scope
                                                tnk <- newThunk env arg
                                                eval ((x,tnk):res_env) res_ty []
                  res_ty                  -> return res_ty
      papply scope (App fun arg) res_ty args

tcPatt scope PW        ty0 =
  return scope
tcPatt scope (PV x)    ty0 =
  return ((x,ty0):scope)
tcPatt scope (PP c ps) ty0 = do
  ty <- getResType c
  let go scope ty []     = return (scope,ty)
      go scope ty (p:ps) = do (_,_,arg_ty,res_ty) <- unifyFun scope ty
                              scope <- tcPatt scope p arg_ty
                              go scope res_ty ps
  vty <- eval [] ty []
  (scope,ty) <- go scope vty ps
  unify scope ty0 ty
  return scope
tcPatt scope (PInt i) ty0 = do
  ty <- vtypeInts i
  unify scope ty ty0
  return scope
tcPatt scope (PString s) ty0 = do
  unify scope ty0 vtypeStr
  return scope
tcPatt scope PChar ty0 = do
  unify scope ty0 vtypeStr
  return scope
tcPatt scope (PSeq _ _ p1 _ _ p2) ty0 = do
  unify scope ty0 vtypeStr
  scope <- tcPatt scope p1 vtypeStr
  scope <- tcPatt scope p2 vtypeStr
  return scope
tcPatt scope (PAs x p) ty0 = do
  tcPatt ((x,ty0):scope) p ty0
tcPatt scope (PR rs) ty0 = do
  let mk_ltys  []            = return []
      mk_ltys  ((l,p):rs)    = do (_,tnk) <- newResiduation scope
                                  ltys <- mk_ltys rs
                                  return ((l,p,VMeta tnk []) : ltys)
      go scope []            = return scope
      go scope ((l,p,ty):rs) = do scope <- tcPatt scope p ty
                                  go scope rs
  ltys <- mk_ltys rs
  subsCheckRho scope (EPatt 0 Nothing (PR rs)) (VRecType [(l,ty) | (l,p,ty) <- ltys]) ty0
  go scope ltys
tcPatt scope (PAlt p1 p2) ty0 = do
  tcPatt scope p1 ty0
  tcPatt scope p2 ty0
  return scope
tcPatt scope (PM q) ty0 = do
  ty <- getResType q
  case ty of
    EPattType ty
            -> do vty <- eval [] ty []
                  unify scope ty0 vty
                  return scope
    ty   -> evalError ("Pattern type expected but " <+> pp ty <+> " found.")
tcPatt scope p ty = unimplemented ("tcPatt "++show p)

inferRecFields scope rs =
  mapM (\(l,r) -> tcRecField scope l r Nothing) rs

checkRecFields scope []          ltys
  | null ltys                            = return []
  | otherwise                            = evalError ("Missing fields:" <+> hsep (map fst ltys))
checkRecFields scope ((l,t):lts) ltys =
  case takeIt l ltys of
    (Just ty,ltys) -> do ltty  <- tcRecField scope l t (Just ty)
                         lttys <- checkRecFields scope lts ltys
                         return (ltty : lttys)
    (Nothing,ltys) -> do evalWarn ("Discarded field:" <+> l)
                         ltty  <- tcRecField scope l t Nothing
                         lttys <- checkRecFields scope lts ltys
                         return lttys     -- ignore the field
  where
    takeIt l1 []  = (Nothing, [])
    takeIt l1 (lty@(l2,ty):ltys)
      | l1 == l2  = (Just ty,ltys)
      | otherwise = let (mb_ty,ltys') = takeIt l1 ltys
                    in (mb_ty,lty:ltys')

tcRecField scope l (mb_ann_ty,t) mb_ty = do
  (t,ty) <- case mb_ann_ty of
              Just ann_ty -> do (ann_ty, _) <- tcRho scope ann_ty (Just vtypeType)
                                env <- scopeEnv scope
                                v_ann_ty <- eval env ann_ty []
                                (t,_) <- tcRho scope t (Just v_ann_ty)
                                instSigma scope t v_ann_ty mb_ty
              Nothing     -> tcRho scope t mb_ty
  return (l,t,ty)

tcRecTypeFields scope []          mb_ty = return ([],mb_ty)
tcRecTypeFields scope ((l,ty):rs) mb_ty = do
  (ty,sort) <- tcRho scope ty mb_ty
  mb_ty <- case sort of
             VSort s
               | s == cType  -> return (Just sort)
               | s == cPType -> return mb_ty
             VMeta _ _       -> return mb_ty
             _               -> do sort <- value2term False (scopeVars scope) sort
                                   evalError ("The record type field" <+> l <+> ':' <+> ppTerm Unqualified 0 ty $$
                                              "cannot be of type" <+> ppTerm Unqualified 0 sort)
  (rs,mb_ty) <- tcRecTypeFields scope rs mb_ty
  return ((l,ty):rs,mb_ty)

-- | Invariant: if the third argument is (Just rho),
--              then rho is in weak-prenex form
instSigma :: Scope s -> Term -> Sigma s -> Maybe (Rho s) -> EvalM s (Term, Rho s)
instSigma scope t ty1 Nothing    = return (t,ty1)           -- INST1
instSigma scope t ty1 (Just ty2) = do                       -- INST2
  t <- subsCheckRho scope t ty1 ty2
  return (t,ty2)

-- | Invariant: the second argument is in weak-prenex form
subsCheckRho :: Scope s -> Term -> Sigma s -> Rho s -> EvalM s Term
subsCheckRho scope t (VMeta i vs1) (VMeta j vs2)
  | i  == j   = do sequence_ (zipWith (unifyThunk scope) vs1 vs2)
                   return t
  | otherwise = do
      mv <- getRef i
      case mv of
        Evaluated _ v1 -> do
          v1 <- apply v1 vs1
          subsCheckRho scope t v1 (VMeta j vs2)
        Residuation _ scope1 _ -> do
          mv <- getRef j
          case mv of
            Evaluated _ v2 -> do
              v2 <- apply v2 vs2
              subsCheckRho scope t (VMeta i vs1) v2
            Residuation _ scope2 _
              | m > n     -> do setRef i (Evaluated m (VMeta j vs2))
                                return t
              | otherwise -> do setRef j (Evaluated n (VMeta i vs2))
                                return t
              where
                m = length scope1
                n = length scope2
subsCheckRho scope t ty1@(VMeta tnk vs) ty2 = do
  mv <- getRef tnk
  case mv of
    Evaluated _ ty1 -> do
      ty1 <- apply ty1 vs
      subsCheckRho scope t ty1 ty2
    Residuation i scope' ctr -> do
      occursCheck scope' tnk scope ty2
      ctr <- subtype scope ctr ty2
      setRef tnk (Residuation i scope' (Just ctr))
      return t
subsCheckRho scope t ty1 ty2@(VMeta tnk vs) = do
  mv <- getRef tnk
  case mv of
    Evaluated _ ty2 -> do
      ty2 <- apply ty2 vs
      subsCheckRho scope t ty1 ty2
    Residuation i scope' ctr -> do
      occursCheck scope' tnk scope ty1
      ctr <- supertype scope ctr ty1
      setRef tnk (Residuation i scope' (Just ctr))
      return t
subsCheckRho scope t (VProd Implicit x ty1 ty2) rho2 = do     -- Rule SPEC
  (i,tnk) <- newResiduation scope
  ty2 <- case ty2 of
           VClosure env ty2 -> eval ((x,tnk):env) ty2 []
           ty2              -> return ty2
  subsCheckRho scope (App t (ImplArg (Meta i))) ty2 rho2
subsCheckRho scope t rho1 (VProd Implicit x ty1 ty2) = do     -- Rule SKOL
  let v = newVar scope
  ty2 <- case ty2 of
           VClosure env ty2 -> do tnk <- newEvaluatedThunk (VGen (length scope) [])
                                  eval ((x,tnk):env) ty2 []
           ty2              -> return ty2
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
subsCheckRho scope t (VApp p1 [tnk1]) (VApp p2 [tnk2])        -- Rule INT2
  | p1 == (cPredef,cInts) && p2 == (cPredef,cInts) = do
      VInt i <- force tnk1
      VInt j <- force tnk2
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
subsCheckRho scope t tau1 tau2 = do                           -- Rule EQ
  unify scope tau1 tau2                                 -- Revert to ordinary unification
  return t

subsCheckFun :: Scope s -> Term -> Sigma s -> Value s -> Sigma s -> Value s -> EvalM s Term
subsCheckFun scope t a1 r1 a2 r2 = do
  let v   = newVar scope
  vt <- subsCheckRho ((v,a2):scope) (Vr v) a2 a1
  tnk <- newEvaluatedThunk (VGen (length scope) [])
  r1 <- case r1 of
          VClosure env r1 -> eval ((v,tnk):env) r1 []
          r1              -> return r1
  r2 <- case r2 of
          VClosure env r2 -> eval ((v,tnk):env) r2 []
          r2              -> return r2
  t  <- subsCheckRho ((v,vtypeType):scope) (App t vt) r1 r2
  return (Abs Explicit v t)

subsCheckTbl :: Scope s -> Term -> Sigma s -> Rho s -> Sigma s -> Rho s -> EvalM s Term
subsCheckTbl scope t p1 r1 p2 r2 = do
  let x = newVar scope
  xt <- subsCheckRho scope (Vr x) p2 p1
  t  <- subsCheckRho ((x,vtypePType):scope) (S t xt) r1 r2
  p2 <- value2term True (scopeVars scope) p2
  return (T (TTyped p2) [(PV x,t)])

subtype scope Nothing (VApp p [tnk])
  | p == (cPredef,cInts) = do
      VInt i <- force tnk
      return (VCInts Nothing (Just i))
subtype scope (Just (VCInts i j)) (VApp p [tnk])
  | p == (cPredef,cInts) = do
      VInt k <- force tnk
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

supertype scope Nothing (VApp p [tnk])
  | p == (cPredef,cInts) = do
      VInt i <- force tnk
      return (VCInts (Just i) Nothing)
supertype scope (Just (VCInts i j)) (VApp p [tnk])
  | p == (cPredef,cInts) = do
      VInt k <- force tnk
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

unifyFun :: Scope s -> Rho s -> EvalM s (BindType, Ident, Sigma s, Rho s)
unifyFun scope (VProd bt x arg res) =
  return (bt,x,arg,res)
unifyFun scope tau = do
  let mk_val (_,tnk) = VMeta tnk []
  arg <- fmap mk_val $ newResiduation scope
  res <- fmap mk_val $ newResiduation scope
  let bt = Explicit
  unify scope tau (VProd bt identW arg res)
  return (bt,identW,arg,res)

unifyTbl :: Scope s -> Rho s -> EvalM s (Sigma s, Rho s)
unifyTbl scope (VTable arg res) =
  return (arg,res)
unifyTbl scope tau = do
  let mk_val (i,tnk) = VMeta tnk []
  arg <- fmap mk_val $ newResiduation scope
  res <- fmap mk_val $ newResiduation scope
  unify scope tau (VTable arg res)
  return (arg,res)

unify scope (VApp f1 vs1) (VApp f2 vs2)
  | f1 == f2  = sequence_ (zipWith (unifyThunk scope) vs1 vs2)
unify scope (VMeta i vs1) (VMeta j vs2)
  | i  == j   = sequence_ (zipWith (unifyThunk scope) vs1 vs2)
  | otherwise = do 
      mv <- getRef i
      case mv of
        Evaluated _ v1 -> do
          v1 <- apply v1 vs1
          unify scope v1 (VMeta j vs2)
        Residuation _ scope1 _ -> do
          mv <- getRef j
          case mv of
            Evaluated _ v2 -> do
              v2 <- apply v2 vs2
              unify scope (VMeta i vs1) v2
            Residuation _ scope2 _
              | m > n     -> setRef i (Evaluated m (VMeta j vs2))
              | otherwise -> setRef j (Evaluated n (VMeta i vs2))
              where
                m = length scope1
                n = length scope2 
unify scope (VMeta i vs) v = unifyVar scope i vs v
unify scope v (VMeta i vs) = unifyVar scope i vs v
unify scope (VGen i vs1)       (VGen j vs2)
  | i == j                     = sequence_ (zipWith (unifyThunk scope) vs1 vs2)
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
  t1 <- value2term False (scopeVars scope) v1
  t2 <- value2term False (scopeVars scope) v2
  evalError ("Cannot unify terms:" <+> (ppTerm Unqualified 0 t1 $$
                                        ppTerm Unqualified 0 t2))

unifyThunk scope tnk1 tnk2 = do
  res1 <- getRef tnk1
  res2 <- getRef tnk2
  case (res1,res2) of
    (Evaluated _ v1,Evaluated _ v2) -> unify scope v1 v2

-- | Invariant: tv1 is a flexible type variable
unifyVar :: Scope s -> Thunk s -> [Thunk s] -> Tau s -> EvalM s ()
unifyVar scope tnk vs ty2 = do            -- Check whether i is bound
  mv <- getRef tnk
  case mv of
    Evaluated _ ty1        -> do ty1 <- apply ty1 vs
                                 unify scope ty1 ty2
    Residuation i scope' _ -> do occursCheck scope' tnk scope ty2
                                 setRef tnk (Evaluated (length scope') ty2)

occursCheck scope' tnk scope v =
  let m = length scope'
      n = length scope
  in check m n v
  where
    check m n (VApp f vs) = mapM_ (follow m n) vs
    check m n (VMeta i vs)
      | tnk == i  = do ty1 <- value2term False (scopeVars scope) (VMeta i vs)
                       ty2 <- value2term False (scopeVars scope) v
                       evalError ("Occurs check for" <+> ppTerm Unqualified 0 ty1 <+> "in:" $$
                                  nest 2 (ppTerm Unqualified 0 ty2))
      | otherwise = do
          s <- getRef i
          case s of
            Evaluated _ v -> do v <- apply v vs
                                check m n v
            _             -> mapM_ (follow m n) vs
    check m n (VGen i vs)
      | i > m     = let (v,_) = reverse scope !! i
                    in evalError ("Variable" <+> pp v <+> "has escaped")
      | otherwise = mapM_ (follow m n) vs
    check m n (VClosure env (Abs bt x t)) = do
      tnk <- newEvaluatedThunk (VGen n [])
      v <- eval ((x,tnk):env) t []
      check (m+1) (n+1) v
    check m n (VProd bt x ty1 ty2) = do
      check m n ty1
      case ty2 of
        VClosure env t -> do tnk <- newEvaluatedThunk (VGen n [])
                             v <- eval ((x,tnk):env) t []
                             check (m+1) (n+1) v
        _              -> check m n ty2
    check m n (VRecType as) =
      mapM_ (\(lbl,v) -> check m n v) as
    check m n (VR as) =
      mapM_ (\(lbl,tnk) -> follow m n tnk) as
    check m n (VP v l vs) =
      check m n v >> mapM_ (follow m n) vs
    check m n (VExtR v1 v2) =
      check m n v1 >> check m n v2
    check m n (VTable v1 v2) =
      check m n v1 >> check m n v2
    check m n (VT ty env cs) =
      check m n ty      -- Traverse cs as well
    check m n (VV ty cs) =
      check m n ty >> mapM_ (follow m n) cs
    check m n (VS v1 tnk vs) =
      check m n v1 >> follow m n tnk >> mapM_ (follow m n) vs
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
    check m n (VAlts v vs) =
      check m n v >> mapM_ (\(v1,v2) -> check m n v1 >> check m n v2) vs
    check m n (VStrs vs) =
      mapM_ (check m n) vs

    follow m n tnk = check m n =<< force tnk

-----------------------------------------------------------------------
-- Instantiation and quantification
-----------------------------------------------------------------------

-- | Instantiate the topmost implicit arguments with metavariables
instantiate :: Scope s -> Term -> Sigma s -> EvalM s (Term,Rho s)
instantiate scope t (VProd Implicit x ty1 ty2) = do
  (i,tnk) <- newResiduation scope
  ty2 <- case ty2 of
           VClosure env ty2 -> eval ((x,tnk):env) ty2 []
           ty2              -> return ty2
  instantiate scope (App t (ImplArg (Meta i))) ty2
instantiate scope t ty = do
  return (t,ty)

-- | Build fresh lambda abstractions for the topmost implicit arguments
skolemise :: Scope s -> Sigma s -> EvalM s (Scope s, Term->Term, Rho s)
skolemise scope ty@(VMeta i vs) = do
  mv <- getRef i
  case mv of
    Residuation _ _ _ -> return (scope,id,ty)                   -- guarded constant?
    Evaluated _ ty    -> do ty <- apply ty vs
                            skolemise scope ty
skolemise scope (VProd Implicit x ty1 ty2) = do
  let v = newVar scope
  ty2 <- case ty2 of
           VClosure env ty2 -> do tnk <- newEvaluatedThunk (VGen (length scope) [])
                                  eval ((x,tnk) : env) ty2 []
           ty2              -> return ty2
  (scope,f,ty2) <- skolemise ((v,ty1):scope) ty2
  return (scope,Abs Implicit v . f,ty2)
skolemise scope ty = do
  return (scope,id,ty)

-- | Quantify over the specified type variables (all flexible)
quantify :: Scope s -> Term -> [Thunk s] -> Rho s -> EvalM s (Term,Sigma s)
quantify scope t tvs ty = do
  let m = length tvs
      n = length scope
  (used_bndrs,ty) <- check m n [] ty
  let new_bndrs  = take m (allBinders \\ used_bndrs)
  mapM_ bind (zip3 [0..] tvs new_bndrs)
  let ty' = foldr (\ty -> VProd Implicit ty vtypeType) ty new_bndrs
  return (foldr (Abs Implicit) t new_bndrs,ty')
  where
    bind (i, tnk, name) = setRef tnk (Evaluated (length scope) (VGen i []))

    check m n xs (VApp f vs)       = do
      (xs,vs) <- mapAccumM (follow m n) xs vs
      return (xs,VApp f vs)
    check m n xs (VMeta i vs)      = do
      s <- getRef i
      case s of
        Evaluated _ v -> do v <- apply v vs
                            check m n xs v
        _             -> do (xs,vs) <- mapAccumM (follow m n) xs vs
                            return (xs,VMeta i vs)
    check m n st (VGen i vs)= do
      (st,vs) <- mapAccumM (follow m n) st vs
      return (st, VGen (m+i) vs)
    check m n st (VClosure env (Abs bt x t)) = do
      (st,env) <- mapAccumM (\st (x,tnk) -> follow m n st tnk >>= \(st,tnk) -> return (st,(x,tnk))) st env
      return (st,VClosure env (Abs bt x t))
    check m n xs (VProd bt x v1 v2) = do
      (xs,v1) <- check m n xs v1
      case v2 of
        VClosure env t -> do tnk <- newEvaluatedThunk (VGen n [])
                             (st,env) <- mapAccumM (\xs (x,tnk) -> follow m n xs tnk >>= \(xs,tnk) -> return (xs,(x,tnk))) xs env
                             return (x:xs,VProd bt x v1 (VClosure env t))
        v2             -> do (xs,v2) <- check m (n+1) xs v2
                             return (x:xs,VProd bt x v1 v2)
    check m n xs (VRecType as)     = do
      (xs,as) <- mapAccumM (\xs (l,v) -> check m n xs v >>= \(xs,v) -> return (xs,(l,v))) xs as
      return (xs,VRecType as)
    check m n xs (VR as)           = do
      (xs,as) <- mapAccumM (\xs (lbl,tnk) -> follow m n xs tnk >>= \(xs,tnk) -> return (xs,(lbl,tnk))) xs as
      return (xs,VR as)
    check m n xs (VP v l vs)       = do
      (xs,v)  <- check m n xs v
      (xs,vs) <- mapAccumM (follow m n) xs vs
      return (xs,VP v l vs)
    check m n xs (VExtR v1 v2)     = do
      (xs,v1) <- check m n xs v1
      (xs,v2) <- check m n xs v2
      return (xs,VExtR v1 v2)
    check m n xs (VTable v1 v2)    = do
      (xs,v1) <- check m n xs v1
      (xs,v2) <- check m n xs v2
      return (xs,VTable v1 v2)
    check m n xs (VT ty env cs)    = do
      (xs,ty) <- check m n xs ty
      (xs,env) <- mapAccumM (\xs (x,tnk) -> follow m n xs tnk >>= \(xs,tnk) -> return (xs,(x,tnk))) xs env
      return (xs,VT ty env cs)
    check m n xs (VV ty cs)        = do
      (xs,ty) <- check m n xs ty
      (xs,cs) <- mapAccumM (follow m n) xs cs
      return (xs,VV ty cs)
    check m n xs (VS v1 tnk vs)    = do
      (xs,v1)  <- check m n xs v1
      (xs,tnk) <- follow m n xs tnk
      (xs,vs)  <- mapAccumM (follow m n) xs vs
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
    check m n st v                 = error (showValue v)

    follow m n st tnk = do
      v      <- force tnk
      (st,v) <- check m n st v
      tnk    <- newEvaluatedThunk v
      return (st,tnk)

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

type Sigma s = Value s
type Rho s   = Value s -- No top-level ForAll
type Tau s   = Value s -- No ForAlls anywhere

unimplemented str = fail ("Unimplemented: "++str)

newVar :: Scope s -> Ident
newVar scope = head [x | i <- [1..],
                         let x = identS ('v':show i),
                         isFree scope x]
  where
    isFree []            x = True
    isFree ((y,_):scope) x = x /= y && isFree scope x

scopeEnv   scope = zipWithM (\(x,ty) i -> newEvaluatedThunk (VGen i []) >>= \tnk -> return (x,tnk)) (reverse scope) [0..]
scopeVars  scope = map fst scope
scopeTypes scope = zipWith (\(_,ty) scope -> (scope,ty)) scope (tails scope)

-- | This function takes account of zonking, and returns a set
-- (no duplicates) of unbound meta-type variables
getMetaVars :: [(Scope s,Sigma s)] -> EvalM s [Thunk s]
getMetaVars sc_tys = foldM (\acc (scope,ty) -> go ty acc) [] sc_tys
  where
    follow acc tnk = force tnk >>= \v -> go v acc

    -- Get the MetaIds from a term; no duplicates in result
    go (VGen i args)     acc = foldM follow acc args
    go (VSort s)         acc = return acc
    go (VInt _)          acc = return acc
    go (VRecType as)     acc = foldM (\acc (lbl,v) -> go v acc) acc as
    go (VClosure _ _)    acc = return acc
    go (VProd b x v1 v2) acc = go v2 acc >>= go v1
    go (VTable v1 v2)    acc = go v2 acc >>= go v1
    go (VMeta m args)    acc
      | m `elem` acc         = return acc
      | otherwise            = do res <- getRef m
                                  case res of
                                    Evaluated _ v           -> go v acc
                                    Residuation _ _ Nothing -> foldM follow (m:acc) args
                                    _                       -> return acc
    go (VApp f args)     acc = foldM follow acc args
    go v                 acc = unimplemented ("go "++showValue v)

-- | Eliminate any substitutions in a term
zonkTerm :: [Ident] -> Term -> EvalM s Term
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
  tnk <- EvalM $ \gr k mt d r msgs -> 
           case Map.lookup i mt of
             Just v  -> k v mt d r msgs
             Nothing -> return (Fail (pp "Undefined meta variable") msgs)
  st <- getRef tnk
  case st of
    Hole _            -> return (Meta i)
    Residuation _ scope v -> case v of
                              Just v  -> zonkTerm xs =<< value2term False (map fst scope) v
                              Nothing -> return (Meta i)
    Narrowing _ _     -> return (Meta i)
    Evaluated _ v     -> zonkTerm xs =<< value2term False xs v
zonkTerm xs t = composOp (zonkTerm xs) t
