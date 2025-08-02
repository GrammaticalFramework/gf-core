{-# LANGUAGE CPP #-}
module GF.Compile.TypeCheck.ConcreteNew( checkLType, inferLType ) where

-- The code here is based on the paper:
-- Simon Peyton Jones, Dimitrios Vytiniotis, Stephanie Weirich.
-- Practical type inference for arbitrary-rank types.
-- 14 September 2011

import GF.Grammar hiding (Env, VGen, VApp, VRecType)
import GF.Grammar.Lookup
import GF.Grammar.Predef
import GF.Grammar.Lockfield
import GF.Compile.Compute.Concrete
import GF.Compile.Compute.Predef(predef,predefName)
import GF.Infra.CheckM
import GF.Data.Operations
import Control.Applicative(Applicative(..))
import Control.Monad(ap,liftM,mplus)
import GF.Text.Pretty
import Data.List (nub, (\\), tails)
import qualified Data.IntMap as IntMap
import Data.Maybe(fromMaybe,isNothing)
import qualified Control.Monad.Fail as Fail

checkLType :: GlobalEnv -> Term -> Type -> Check (Term, Type)
checkLType ge t ty = runTcM $ do
  vty <- liftErr (eval ge [] ty)
  (t,_) <- tcRho ge [] t (Just vty)
  t <- zonkTerm t
  return (t,ty)

inferLType :: GlobalEnv -> Term -> Check (Term, Type)
inferLType ge t = runTcM $ do
  (t,ty) <- inferSigma ge [] t
  t  <- zonkTerm t
  ty <- zonkTerm =<< tc_value2term (geLoc ge) [] ty
  return (t,ty)

inferSigma :: GlobalEnv -> Scope -> Term -> TcM (Term,Sigma)
inferSigma ge scope t = do                                      -- GEN1
  (t,ty) <- tcRho ge scope t Nothing
  env_tvs <- getMetaVars (geLoc ge) (scopeTypes scope)
  res_tvs <- getMetaVars (geLoc ge) [(scope,ty)]
  let forall_tvs = res_tvs \\ env_tvs
  quantify ge scope t forall_tvs ty

Just vtypeInt   = fmap (flip VApp []) (predef cInt)
Just vtypeFloat = fmap (flip VApp []) (predef cFloat)
Just vtypeInts  = fmap (\p i -> VApp p [VInt i]) (predef cInts)
vtypeStr   = VSort cStr
vtypeStrs  = VSort cStrs
vtypeType  = VSort cType
vtypePType = VSort cPType

tcRho :: GlobalEnv -> Scope -> Term -> Maybe Rho -> TcM (Term, Rho)
tcRho ge scope t@(EInt i)   mb_ty = instSigma ge scope t (vtypeInts i) mb_ty -- INT
tcRho ge scope t@(EFloat _) mb_ty = instSigma ge scope t vtypeFloat mb_ty    -- FLOAT
tcRho ge scope t@(K _)      mb_ty = instSigma ge scope t vtypeStr   mb_ty    -- STR
tcRho ge scope t@(Empty)    mb_ty = instSigma ge scope t vtypeStr   mb_ty
tcRho ge scope t@(Vr v)     mb_ty = do                          -- VAR
  case lookup v scope of
    Just v_sigma -> instSigma ge scope t v_sigma mb_ty
    Nothing      -> tcError ("Unknown variable" <+> v)
tcRho ge scope t@(Q id)     mb_ty =
  runTcA (tcOverloadFailed t) $
    tcApp ge scope t `bindTcA` \(t,ty) ->
      instSigma ge scope t ty mb_ty
tcRho ge scope t@(QC id)    mb_ty =
  runTcA (tcOverloadFailed t) $
    tcApp ge scope t `bindTcA` \(t,ty) ->
      instSigma ge scope t ty mb_ty
tcRho ge scope t@(App fun arg) mb_ty = do
  runTcA (tcOverloadFailed t) $
    tcApp ge scope t `bindTcA` \(t,ty) ->
      instSigma ge scope t ty mb_ty
tcRho ge scope (Abs bt var body) Nothing = do                   -- ABS1
  i <- newMeta scope vtypeType
  let arg_ty = VMeta i (scopeEnv scope) []
  (body,body_ty) <- tcRho ge ((var,arg_ty):scope) body Nothing
  return (Abs bt var body, (VProd bt arg_ty identW (Bind (const body_ty))))
tcRho ge scope t@(Abs Implicit var body) (Just ty) = do         -- ABS2
  (bt, var_ty, body_ty) <- unifyFun ge scope ty
  if bt == Implicit
    then return ()
    else tcError (ppTerm Unqualified 0 t <+> "is an implicit function, but no implicit function is expected")
  (body, body_ty) <- tcRho ge ((var,var_ty):scope) body (Just (body_ty (VGen (length scope) [])))
  return (Abs Implicit var body,ty)
tcRho ge scope (Abs Explicit var body) (Just ty) = do           -- ABS3
  (scope,f,ty') <- skolemise ge scope ty
  (_,var_ty,body_ty) <- unifyFun ge scope ty'
  (body, body_ty) <- tcRho ge ((var,var_ty):scope) body (Just (body_ty (VGen (length scope) [])))
  return (f (Abs Explicit var body),ty)
tcRho ge scope (Let (var, (mb_ann_ty, rhs)) body) mb_ty = do    -- LET
  (rhs,var_ty) <- case mb_ann_ty of
                    Nothing     -> inferSigma ge scope rhs
                    Just ann_ty -> do (ann_ty, _) <- tcRho ge scope ann_ty (Just vtypeType)
                                      v_ann_ty <- liftErr (eval ge (scopeEnv scope) ann_ty)
                                      (rhs,_) <- tcRho ge scope rhs (Just v_ann_ty)
                                      return (rhs, v_ann_ty)
  (body, body_ty) <- tcRho ge ((var,var_ty):scope) body mb_ty
  var_ty <- tc_value2term (geLoc ge) (scopeVars scope) var_ty
  return (Let (var, (Just var_ty, rhs)) body, body_ty)
tcRho ge scope (Typed body ann_ty) mb_ty = do                   -- ANNOT
  (ann_ty, _) <- tcRho ge scope ann_ty (Just vtypeType)
  v_ann_ty <- liftErr (eval ge (scopeEnv scope) ann_ty)
  (body,_) <- tcRho ge scope body (Just v_ann_ty)
  instSigma ge scope (Typed body ann_ty) v_ann_ty mb_ty
tcRho ge scope (FV ts) mb_ty = do
  case ts of
    []     -> do i <- newMeta scope vtypeType
                 instSigma ge scope (FV []) (VMeta i (scopeEnv scope) []) mb_ty
    (t:ts) -> do (t,ty) <- tcRho ge scope t mb_ty

                 let go []     ty = return ([],ty)
                     go (t:ts) ty = do (t, ty) <- tcRho ge scope t (Just ty)
                                       (ts,ty) <- go ts ty
                                       return (t:ts,ty)

                 (ts,ty) <- go ts ty
                 return (FV (t:ts), ty)
tcRho ge scope t@(Sort s) mb_ty = do
  instSigma ge scope t vtypeType mb_ty
tcRho ge scope t@(RecType rs) Nothing   = do
  (rs,mb_ty) <- tcRecTypeFields ge scope rs Nothing
  return (RecType rs,fromMaybe vtypePType mb_ty)
tcRho ge scope t@(RecType rs) (Just ty) = do
  (scope,f,ty') <- skolemise ge scope ty
  case ty' of
    VSort s
      | s == cType  -> return ()
      | s == cPType -> return ()
    VMeta i env vs  -> case rs of
                         [] -> unifyVar ge scope i env vs vtypePType
                         _  -> return ()
    ty              -> do ty <- zonkTerm =<< tc_value2term (geLoc ge) (scopeVars scope) ty
                          tcError ("The record type" <+> ppTerm Unqualified 0 t $$
                                   "cannot be of type" <+> ppTerm Unqualified 0 ty)
  (rs,mb_ty) <- tcRecTypeFields ge scope rs (Just ty')
  return (f (RecType rs),ty)
tcRho ge scope t@(Table p res) mb_ty = do
  (p,  p_ty)   <- tcRho ge scope p   (Just vtypePType)
  (res,res_ty) <- tcRho ge scope res (Just vtypeType)
  instSigma ge scope (Table p res) vtypeType mb_ty
tcRho ge scope (Prod bt x ty1 ty2) mb_ty = do
  (ty1,ty1_ty) <- tcRho ge scope ty1 (Just vtypeType)
  vty1 <- liftErr (eval ge (scopeEnv scope) ty1)
  (ty2,ty2_ty) <- tcRho ge ((x,vty1):scope) ty2 (Just vtypeType)
  instSigma ge scope (Prod bt x ty1 ty2) vtypeType mb_ty
tcRho ge scope (S t p) mb_ty = do
  p_ty   <- fmap (\i -> VMeta i (scopeEnv scope) []) $ newMeta scope vtypePType
  res_ty <- case mb_ty of
              Nothing -> fmap (\i -> VMeta i (scopeEnv scope) []) $ newMeta scope vtypeType
              Just ty -> return ty
  let t_ty = VTblType p_ty res_ty
  (t,t_ty) <- tcRho ge scope t (Just t_ty)
  (p,_) <- tcRho ge scope p (Just p_ty)
  return (S t p, res_ty)
tcRho ge scope (T tt ps) Nothing = do                           -- ABS1/AABS1 for tables
  p_ty   <- case tt of
              TRaw      -> fmap (\i -> VMeta i (scopeEnv scope) []) $ newMeta scope vtypePType
              TTyped ty -> do (ty, _) <- tcRho ge scope ty (Just vtypeType)
                              liftErr (eval ge (scopeEnv scope) ty)
  (ps,mb_res_ty) <- tcCases ge scope ps p_ty Nothing
  res_ty <- case mb_res_ty of
              Just res_ty -> return res_ty
              Nothing     -> fmap (\i -> VMeta i (scopeEnv scope) []) $ newMeta scope vtypeType
  p_ty_t <- tc_value2term (geLoc ge) [] p_ty
  return (T (TTyped p_ty_t) ps, VTblType p_ty res_ty)
tcRho ge scope (T tt ps) (Just ty) = do                         -- ABS2/AABS2 for tables
  (scope,f,ty') <- skolemise ge scope ty
  (p_ty, res_ty) <- unifyTbl ge scope ty'
  case tt of
    TRaw      -> return ()
    TTyped ty -> do (ty, _) <- tcRho ge scope ty (Just vtypeType)
                    return ()--subsCheckRho ge scope -> Term ty res_ty
  (ps,Just res_ty) <- tcCases ge scope ps p_ty (Just res_ty)
  p_ty_t <- tc_value2term (geLoc ge) [] p_ty
  return (f (T (TTyped p_ty_t) ps), VTblType p_ty res_ty)
tcRho ge scope (R rs) Nothing = do
  lttys <- inferRecFields ge scope rs
  rs <- mapM (\(l,t,ty) -> tc_value2term (geLoc ge) (scopeVars scope) ty >>= \ty -> return (l, (Just ty, t))) lttys
  return (R        rs,
          VRecType [(l, ty) | (l,t,ty) <- lttys]
         )
tcRho ge scope (R rs) (Just ty) = do
  (scope,f,ty') <- skolemise ge scope ty
  case ty' of
    (VRecType ltys) -> do lttys <- checkRecFields ge scope rs ltys
                          rs <- mapM (\(l,t,ty) -> tc_value2term (geLoc ge) (scopeVars scope) ty >>= \ty -> return (l, (Just ty, t))) lttys
                          return ((f . R)  rs,
                                  VRecType [(l, ty) | (l,t,ty) <- lttys]
                                 )
    ty              -> do lttys <- inferRecFields ge scope rs
                          t <- liftM (f . R) (mapM (\(l,t,ty) -> tc_value2term (geLoc ge) (scopeVars scope) ty >>= \ty -> return (l, (Just ty, t))) lttys)
                          let ty' = VRecType [(l, ty) | (l,t,ty) <- lttys]
                          t <- subsCheckRho ge scope t ty' ty
                          return (t, ty')
tcRho ge scope (P t l) mb_ty = do
  l_ty   <- case mb_ty of
              Just ty -> return ty
              Nothing -> do i <- newMeta scope vtypeType
                            return (VMeta i (scopeEnv scope) [])
  (t,t_ty) <- tcRho ge scope t (Just (VRecType [(l,l_ty)]))
  return (P t l,l_ty)
tcRho ge scope (C t1 t2) mb_ty = do
  (t1,t1_ty) <- tcRho ge scope t1 (Just vtypeStr)
  (t2,t2_ty) <- tcRho ge scope t2 (Just vtypeStr)
  instSigma ge scope (C t1 t2) vtypeStr mb_ty
tcRho ge scope (Glue t1 t2) mb_ty = do
  (t1,t1_ty) <- tcRho ge scope t1 (Just vtypeStr)
  (t2,t2_ty) <- tcRho ge scope t2 (Just vtypeStr)
  instSigma ge scope (Glue t1 t2) vtypeStr mb_ty
tcRho ge scope t@(ExtR t1 t2) mb_ty = do
  (t1,t1_ty) <- tcRho ge scope t1 Nothing
  (t2,t2_ty) <- tcRho ge scope t2 Nothing
  case (t1_ty,t2_ty) of
    (VSort s1,VSort s2)
       | (s1 == cType || s1 == cPType) &&
         (s2 == cType || s2 == cPType) -> let sort | s1 == cPType && s2 == cPType = cPType
                                                   | otherwise                    = cType
                                          in instSigma ge scope (ExtR t1 t2) (VSort sort) mb_ty
    (VRecType rs1, VRecType rs2)       -> instSigma ge scope (ExtR t1 t2) (VRecType (rs2++rs1)) mb_ty
    _                                  -> tcError ("Cannot type check" <+> ppTerm Unqualified 0 t)
tcRho ge scope (ELin cat t) mb_ty = do  -- this could be done earlier, i.e. in the parser
  tcRho ge scope (ExtR t (R [(lockLabel cat,(Just (RecType []),R []))])) mb_ty
tcRho ge scope (ELincat cat t) mb_ty = do  -- this could be done earlier, i.e. in the parser
  tcRho ge scope (ExtR t (RecType [(lockLabel cat,RecType [])])) mb_ty
tcRho ge scope (Alts t ss) mb_ty = do
  (t,_) <- tcRho ge scope t (Just vtypeStr)
  ss    <- flip mapM ss $ \(t1,t2) -> do
              (t1,_) <- tcRho ge scope t1 (Just vtypeStr)
              (t2,_) <- tcRho ge scope t2 (Just vtypeStrs)
              return (t1,t2)
  instSigma ge scope (Alts t ss) vtypeStr mb_ty
tcRho ge scope (Strs ss) mb_ty = do
  ss <- flip mapM ss $ \t -> do
           (t,_) <- tcRho ge scope t (Just vtypeStr)
           return t
  instSigma ge scope (Strs ss) vtypeStrs mb_ty
tcRho ge scope (EPattType ty) mb_ty = do
  (ty, _) <- tcRho ge scope ty (Just vtypeType)
  instSigma ge scope (EPattType ty) vtypeType mb_ty
tcRho ge scope t@(EPatt p) mb_ty = do
  (scope,f,ty) <- case mb_ty of
                    Nothing -> do i <- newMeta scope vtypeType
                                  return (scope,id,VMeta i (scopeEnv scope) [])
                    Just ty -> do (scope,f,ty) <- skolemise ge scope ty
                                  case ty of
                                    VPattType ty -> return (scope,f,ty)
                                    _            -> tcError (ppTerm Unqualified 0 t <+> "must be of pattern type but" <+> ppTerm Unqualified 0 t <+> "is expected")
  tcPatt ge scope p ty
  return (f (EPatt p), ty)
tcRho gr scope t _ = unimplemented ("tcRho "++show t)

tcCases ge scope []         p_ty mb_res_ty = return ([],mb_res_ty)
tcCases ge scope ((p,t):cs) p_ty mb_res_ty = do
  scope' <- tcPatt ge scope p p_ty
  (t,res_ty)     <- tcRho ge scope' t mb_res_ty
  (cs,mb_res_ty) <- tcCases ge scope cs p_ty (Just res_ty)
  return ((p,t):cs,mb_res_ty)


tcApp ge scope t@(App fun (ImplArg arg)) = do                   -- APP1
  tcApp ge scope fun `bindTcA` \(fun,fun_ty) ->
     do (bt, arg_ty, res_ty) <- unifyFun ge scope fun_ty
        if (bt == Implicit)
          then return ()
          else tcError (ppTerm Unqualified 0 t <+> "is an implicit argument application, but no implicit argument is expected")
        (arg,_) <- tcRho ge scope arg (Just arg_ty)
        varg <- liftErr (eval ge (scopeEnv scope) arg)
        return (App fun (ImplArg arg), res_ty varg)
tcApp ge scope (App fun arg) =                                  -- APP2
  tcApp ge scope fun `bindTcA` \(fun,fun_ty) ->
     do (fun,fun_ty) <- instantiate scope fun fun_ty
        (_, arg_ty, res_ty) <- unifyFun ge scope fun_ty
        (arg,_) <- tcRho ge scope arg (Just arg_ty)
        varg <- liftErr (eval ge (scopeEnv scope) arg)
        return (App fun arg, res_ty varg)
tcApp ge scope (Q id) =                                          -- VAR (global)
  mkTcA (lookupOverloadTypes (geGrammar ge) id) `bindTcA` \(t,ty) ->
     do ty <- liftErr (eval ge [] ty)
        return (t,ty)
tcApp ge scope (QC id) =                                         -- VAR (global)
  mkTcA (lookupOverloadTypes (geGrammar ge) id) `bindTcA` \(t,ty) ->
     do ty <- liftErr (eval ge [] ty)
        return (t,ty)
tcApp ge scope t =
  singleTcA (tcRho ge scope t Nothing)


tcOverloadFailed t ttys =
  tcError ("Overload resolution failed" $$
           "of term   " <+> pp t $$
           "with types" <+> vcat [ppTerm Terse 0 ty | (_,ty) <- ttys])


tcPatt ge scope PW        ty0 =
  return scope
tcPatt ge scope (PV x)    ty0 =
  return ((x,ty0):scope)
tcPatt ge scope (PP c ps) ty0 =
  case lookupResType (geGrammar ge) c of
    Ok ty  -> do let go scope ty []     = return (scope,ty)
                     go scope ty (p:ps) = do (_,arg_ty,res_ty) <- unifyFun ge scope ty
                                             scope <- tcPatt ge scope p arg_ty
                                             go scope (res_ty (VGen (length scope) [])) ps
                 vty <- liftErr (eval ge [] ty)
                 (scope,ty) <- go scope vty ps
                 unify ge scope ty0 ty
                 return scope
    Bad err -> tcError (pp err)
tcPatt ge scope (PInt i) ty0 = do
  subsCheckRho ge scope (EInt i) (vtypeInts i) ty0
  return scope
tcPatt ge scope (PString s) ty0 = do
  unify ge scope ty0 vtypeStr
  return scope
tcPatt ge scope PChar ty0 = do
  unify ge scope ty0 vtypeStr
  return scope
tcPatt ge scope (PSeq p1 p2) ty0 = do
  unify ge scope ty0 vtypeStr
  scope <- tcPatt ge scope p1 vtypeStr
  scope <- tcPatt ge scope p2 vtypeStr
  return scope
tcPatt ge scope (PAs x p) ty0 = do
  tcPatt ge ((x,ty0):scope) p ty0
tcPatt ge scope (PR rs) ty0 = do
  let mk_ltys  []            = return []
      mk_ltys  ((l,p):rs)    = do i <- newMeta scope vtypePType
                                  ltys <- mk_ltys rs
                                  return ((l,p,VMeta i (scopeEnv scope) []) : ltys)
      go scope []            = return scope
      go scope ((l,p,ty):rs) = do scope <- tcPatt ge scope p ty
                                  go scope rs
  ltys <- mk_ltys rs
  subsCheckRho ge scope (EPatt (PR rs)) (VRecType [(l,ty) | (l,p,ty) <- ltys]) ty0
  go scope ltys
tcPatt ge scope (PAlt p1 p2) ty0 = do
  tcPatt ge scope p1 ty0
  tcPatt ge scope p2 ty0
  return scope
tcPatt ge scope (PM q) ty0 = do
  case lookupResType (geGrammar ge) q of
    Ok (EPattType ty)
            -> do vty <- liftErr (eval ge [] ty)
                  unify ge scope ty0 vty
                  return scope
    Ok ty   -> tcError ("Pattern type expected but " <+> pp ty <+> " found.")
    Bad err -> tcError (pp err)
tcPatt ge scope p ty = unimplemented ("tcPatt "++show p)

inferRecFields ge scope rs =
  mapM (\(l,r) -> tcRecField ge scope l r Nothing) rs

checkRecFields ge scope []          ltys
  | null ltys                            = return []
  | otherwise                            = tcError ("Missing fields:" <+> hsep (map fst ltys))
checkRecFields ge scope ((l,t):lts) ltys =
  case takeIt l ltys of
    (Just ty,ltys) -> do ltty  <- tcRecField ge scope l t (Just ty)
                         lttys <- checkRecFields ge scope lts ltys
                         return (ltty : lttys)
    (Nothing,ltys) -> do tcWarn ("Discarded field:" <+> l)
                         ltty  <- tcRecField ge scope l t Nothing
                         lttys <- checkRecFields ge scope lts ltys
                         return lttys     -- ignore the field
  where
    takeIt l1 []  = (Nothing, [])
    takeIt l1 (lty@(l2,ty):ltys)
      | l1 == l2  = (Just ty,ltys)
      | otherwise = let (mb_ty,ltys') = takeIt l1 ltys
                    in (mb_ty,lty:ltys')

tcRecField ge scope l (mb_ann_ty,t) mb_ty = do
  (t,ty) <- case mb_ann_ty of
              Just ann_ty -> do (ann_ty, _) <- tcRho ge scope ann_ty (Just vtypeType)
                                v_ann_ty <- liftErr (eval ge (scopeEnv scope) ann_ty)
                                (t,_) <- tcRho ge scope t (Just v_ann_ty)
                                instSigma ge scope t v_ann_ty mb_ty
              Nothing     -> tcRho ge scope t mb_ty
  return (l,t,ty)

tcRecTypeFields ge scope []          mb_ty = return ([],mb_ty)
tcRecTypeFields ge scope ((l,ty):rs) mb_ty = do
  (ty,sort) <- tcRho ge scope ty mb_ty
  mb_ty <- case sort of
             VSort s
               | s == cType  -> return (Just sort)
               | s == cPType -> return mb_ty
             VMeta _ _ _     -> return mb_ty
             _               -> do sort <- zonkTerm =<< tc_value2term (geLoc ge) (scopeVars scope) sort
                                   tcError ("The record type field" <+> l <+> ':' <+> ppTerm Unqualified 0 ty $$
                                            "cannot be of type" <+> ppTerm Unqualified 0 sort)
  (rs,mb_ty) <- tcRecTypeFields ge scope rs mb_ty
  return ((l,ty):rs,mb_ty)

-- | Invariant: if the third argument is (Just rho),
--              then rho is in weak-prenex form
instSigma :: GlobalEnv -> Scope -> Term -> Sigma -> Maybe Rho -> TcM (Term, Rho)
instSigma ge scope t ty1 Nothing    = return (t,ty1)           -- INST1
instSigma ge scope t ty1 (Just ty2) = do                       -- INST2
  t <- subsCheckRho ge scope t ty1 ty2
  return (t,ty2)

-- | Invariant: the second argument is in weak-prenex form
subsCheckRho :: GlobalEnv -> Scope -> Term -> Sigma -> Rho -> TcM Term
subsCheckRho ge scope t ty1@(VMeta i env vs) ty2 = do
  mv <- getMeta i
  case mv of
    Unbound _ _ -> do unify ge scope ty1 ty2
                      return t
    Bound ty1   -> do vty1 <- liftErr (eval ge env ty1)
                      subsCheckRho ge scope t (vapply (geLoc ge) vty1 vs) ty2
subsCheckRho ge scope t ty1 ty2@(VMeta i env vs) = do
  mv <- getMeta i
  case mv of
    Unbound _ _ -> do unify ge scope ty1 ty2
                      return t
    Bound ty2   -> do vty2 <- liftErr (eval ge env ty2)
                      subsCheckRho ge scope t ty1 (vapply (geLoc ge) vty2 vs)
subsCheckRho ge scope t (VProd Implicit ty1 x (Bind ty2)) rho2 = do     -- Rule SPEC
  i <- newMeta scope ty1
  subsCheckRho ge scope (App t (ImplArg (Meta i))) (ty2 (VMeta i [] [])) rho2
subsCheckRho ge scope t rho1 (VProd Implicit ty1 x (Bind ty2)) = do     -- Rule SKOL
  let v = newVar scope
  t <- subsCheckRho ge ((v,ty1):scope) t rho1 (ty2 (VGen (length scope) []))
  return (Abs Implicit v t)
subsCheckRho ge scope t rho1 (VProd Explicit a2 _ (Bind r2)) = do       -- Rule FUN
  (_,a1,r1) <- unifyFun ge scope rho1
  subsCheckFun ge scope t a1 r1 a2 r2
subsCheckRho ge scope t (VProd Explicit a1 _ (Bind r1)) rho2 = do       -- Rule FUN
  (bt,a2,r2) <- unifyFun ge scope rho2
  subsCheckFun ge scope t a1 r1 a2 r2
subsCheckRho ge scope t rho1 (VTblType p2 r2) = do                      -- Rule TABLE
  (p1,r1) <- unifyTbl ge scope rho1
  subsCheckTbl ge scope t p1 r1 p2 r2
subsCheckRho ge scope t (VTblType p1 r1) rho2 = do                      -- Rule TABLE
  (p2,r2) <- unifyTbl ge scope rho2
  subsCheckTbl ge scope t p1 r1 p2 r2
subsCheckRho ge scope t (VSort s1) (VSort s2)                           -- Rule PTYPE
  | s1 == cPType && s2 == cType = return t
subsCheckRho ge scope t (VApp p1 _) (VApp p2 _)                         -- Rule INT1
  | predefName p1 == cInts && predefName p2 == cInt = return t
subsCheckRho ge scope t (VApp p1 [VInt i]) (VApp p2 [VInt j])           -- Rule INT2
  | predefName p1 == cInts && predefName p2 == cInts =
      if i <= j
        then return t
        else tcError ("Ints" <+> i <+> "is not a subtype of" <+> "Ints" <+> j)
subsCheckRho ge scope t ty1@(VRecType rs1) ty2@(VRecType rs2) = do      -- Rule REC
  let mkAccess scope t =
        case t of
          ExtR t1 t2 -> do (scope,mkProj1,mkWrap1) <- mkAccess scope t1
                           (scope,mkProj2,mkWrap2) <- mkAccess scope t2
                           return (scope
                                  ,\l -> mkProj2 l `mplus` mkProj1 l
                                  ,mkWrap1 . mkWrap2
                                  )
          R rs -> do sequence_ [tcWarn ("Discarded field:" <+> l) | (l,_) <- rs, isNothing (lookup l rs2)]
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
        t <- subsCheckRho ge scope t ty1 ty2
        return (l, (mb_ty,t))

  (scope,mkProj,mkWrap) <- mkAccess scope t

  let fields = [(l,ty2,lookup l rs1) | (l,ty2) <- rs2]
  case [l | (l,_,Nothing) <- fields] of
    []      -> return ()
    missing -> tcError ("In the term" <+> pp t $$
                        "there are no values for fields:" <+> hsep missing)
  rs <- sequence [mkField scope l t ty1 ty2 | (l,ty2,Just ty1) <- fields, Just t <- [mkProj l]]
  return (mkWrap (R rs))
subsCheckRho ge scope t tau1 tau2 = do                                  -- Rule EQ
  unify ge scope tau1 tau2                                 -- Revert to ordinary unification
  return t

subsCheckFun :: GlobalEnv -> Scope -> Term -> Sigma -> (Value -> Rho) -> Sigma -> (Value -> Rho) -> TcM Term
subsCheckFun ge scope t a1 r1 a2 r2 = do
  let v   = newVar scope
  vt <- subsCheckRho ge ((v,a2):scope) (Vr v) a2 a1
  val1 <- liftErr (eval ge (scopeEnv ((v,vtypeType):scope)) vt)
  val2 <- return  (VGen (length scope) [])
  t  <- subsCheckRho ge ((v,vtypeType):scope) (App t vt) (r1 val1) (r2 val2)
  return (Abs Explicit v t)

subsCheckTbl :: GlobalEnv -> Scope -> Term -> Sigma -> Rho -> Sigma -> Rho -> TcM Term
subsCheckTbl ge scope t p1 r1 p2 r2 = do
  let x = newVar scope
  xt <- subsCheckRho ge scope (Vr x) p2 p1
  t  <- subsCheckRho ge ((x,vtypePType):scope) (S t xt) r1 r2 ;
  p2 <- tc_value2term (geLoc ge) (scopeVars scope) p2
  return (T (TTyped p2) [(PV x,t)])

-----------------------------------------------------------------------
-- Unification
-----------------------------------------------------------------------

unifyFun :: GlobalEnv -> Scope -> Rho -> TcM (BindType, Sigma, Value -> Rho)
unifyFun ge scope (VProd bt arg x (Bind res)) =
  return (bt,arg,res)
unifyFun ge scope tau = do
  let mk_val ty = VMeta ty [] []
  arg <- fmap mk_val $ newMeta scope vtypeType
  res <- fmap mk_val $ newMeta scope vtypeType
  let bt = Explicit
  unify ge scope tau (VProd bt arg identW (Bind (const res)))
  return (bt,arg,const res)

unifyTbl :: GlobalEnv -> Scope -> Rho -> TcM (Sigma, Rho)
unifyTbl ge scope (VTblType arg res) =
  return (arg,res)
unifyTbl ge scope tau = do
  let mk_val ty = VMeta ty (scopeEnv scope) []
  arg <- fmap mk_val $ newMeta scope vtypePType
  res <- fmap mk_val $ newMeta scope vtypeType
  unify ge scope tau (VTblType arg res)
  return (arg,res)

unify ge scope (VApp f1 vs1)      (VApp f2 vs2)
  | f1 == f2                   = sequence_ (zipWith (unify ge scope) vs1 vs2)
unify ge scope (VCApp f1 vs1)     (VCApp f2 vs2)
  | f1 == f2                   = sequence_ (zipWith (unify ge scope) vs1 vs2)
unify ge scope (VSort s1)         (VSort s2)
  | s1 == s2                   = return ()
unify ge scope (VGen i vs1)       (VGen j vs2)
  | i == j                     = sequence_ (zipWith (unify ge scope) vs1 vs2)
unify ge scope (VTblType p1 res1) (VTblType p2 res2) = do
  unify ge scope p1   p2
  unify ge scope res1 res2
unify ge scope (VMeta i env1 vs1) (VMeta j env2 vs2)
  | i  == j                    = sequence_ (zipWith (unify ge scope) vs1 vs2)
  | otherwise                  = do mv <- getMeta j
                                    case mv of
                                      Bound t2    -> do v2 <- liftErr (eval ge env2 t2)
                                                        unify ge scope (VMeta i env1 vs1) (vapply (geLoc ge) v2 vs2)
                                      Unbound _ _ -> setMeta i (Bound (Meta j))
unify ge scope (VInt i)       (VInt j)
  | i == j                     = return ()
unify ge scope (VMeta i env vs) v = unifyVar ge scope i env vs v
unify ge scope v (VMeta i env vs) = unifyVar ge scope i env vs v
unify ge scope v1 v2 = do
  t1 <- zonkTerm =<< tc_value2term (geLoc ge) (scopeVars scope) v1
  t2 <- zonkTerm =<< tc_value2term (geLoc ge) (scopeVars scope) v2
  tcError ("Cannot unify terms:" <+> (ppTerm Unqualified 0 t1 $$
                                      ppTerm Unqualified 0 t2))

-- | Invariant: tv1 is a flexible type variable
unifyVar :: GlobalEnv -> Scope -> MetaId -> Env -> [Value] -> Tau -> TcM ()
unifyVar ge scope i env vs ty2 = do            -- Check whether i is bound
  mv <- getMeta i
  case mv of
    Bound ty1       -> do v <- liftErr (eval ge env ty1)
                          unify ge scope (vapply (geLoc ge) v vs) ty2
    Unbound scope' _ -> case value2term (geLoc ge) (scopeVars scope') ty2 of
                          -- Left i     -> let (v,_) = reverse scope !! i
                          --               in tcError ("Variable" <+> pp v <+> "has escaped")
                                ty2' -> do ms2 <- getMetaVars (geLoc ge) [(scope,ty2)]
                                           if i `elem` ms2
                                             then tcError ("Occurs check for" <+> ppMeta i <+> "in:" $$
                                                           nest 2 (ppTerm Unqualified 0 ty2'))
                                             else setMeta i (Bound ty2')

-----------------------------------------------------------------------
-- Instantiation and quantification
-----------------------------------------------------------------------

-- | Instantiate the topmost implicit arguments with metavariables
instantiate :: Scope -> Term -> Sigma -> TcM (Term,Rho)
instantiate scope t (VProd Implicit ty1 x (Bind ty2)) = do
  i <- newMeta scope ty1
  instantiate scope (App t (ImplArg (Meta i))) (ty2 (VMeta i [] []))
instantiate scope t ty = do
  return (t,ty)

-- | Build fresh lambda abstractions for the topmost implicit arguments
skolemise :: GlobalEnv -> Scope -> Sigma -> TcM (Scope, Term->Term, Rho)
skolemise ge scope ty@(VMeta i env vs) = do
  mv <- getMeta i
  case mv of
    Unbound _ _ -> return (scope,id,ty)                   -- guarded constant?
    Bound ty    -> do vty <- liftErr (eval ge env ty)
                      skolemise ge scope (vapply (geLoc ge) vty vs)
skolemise ge scope (VProd Implicit ty1 x (Bind ty2)) = do
  let v = newVar scope
  (scope,f,ty2) <- skolemise ge ((v,ty1):scope) (ty2 (VGen (length scope) []))
  return (scope,Abs Implicit v . f,ty2)
skolemise ge scope ty = do
  return (scope,id,ty)

-- | Quantify over the specified type variables (all flexible)
quantify :: GlobalEnv -> Scope -> Term -> [MetaId] -> Rho -> TcM (Term,Sigma)
quantify ge scope t tvs ty0 = do
  ty <- tc_value2term (geLoc ge) (scopeVars scope) ty0
  let used_bndrs = nub (bndrs ty)  -- Avoid quantified type variables in use
      new_bndrs  = take (length tvs) (allBinders \\ used_bndrs)
  mapM_ bind (tvs `zip` new_bndrs)   -- 'bind' is just a cunning way
  ty <- zonkTerm ty                  -- of doing the substitution
  vty <- liftErr (eval ge [] (foldr (\v ty -> Prod Implicit v typeType ty) ty new_bndrs))
  return (foldr (Abs Implicit) t new_bndrs,vty)
  where
    bind (i, name) = setMeta i (Bound (Vr name))

    bndrs (Prod _ x t1 t2) = [x] ++ bndrs t1  ++ bndrs t2
    bndrs _                = []

allBinders :: [Ident]    -- a,b,..z, a1, b1,... z1, a2, b2,...
allBinders = [ identS [x]          | x <- ['a'..'z'] ] ++
             [ identS (x : show i) | i <- [1 :: Integer ..], x <- ['a'..'z']]


-----------------------------------------------------------------------
-- The Monad
-----------------------------------------------------------------------

type Scope = [(Ident,Value)]

type Sigma = Value
type Rho   = Value -- No top-level ForAll
type Tau   = Value -- No ForAlls anywhere

data MetaValue
  = Unbound Scope Sigma
  | Bound   Term
type MetaStore = IntMap.IntMap MetaValue
data TcResult a
  = TcOk   a MetaStore [Message]
  | TcFail             [Message] -- First msg is error, the rest are warnings?
newtype TcM a = TcM {unTcM :: MetaStore -> [Message] -> TcResult a}

instance Monad TcM where
  return = pure
  f >>= g  = TcM (\ms msgs -> case unTcM f ms msgs of
                                TcOk x ms msgs -> unTcM (g x) ms msgs
                                TcFail    msgs -> TcFail msgs)

#if !(MIN_VERSION_base(4,13,0))
  -- Monad(fail) will be removed in GHC 8.8+
  fail = Fail.fail
#endif

instance Fail.MonadFail TcM where
  fail     = tcError . pp


instance Applicative TcM where
  pure x = TcM (\ms msgs -> TcOk x ms msgs)
  (<*>) = ap

instance Functor TcM where
  fmap f g = TcM (\ms msgs -> case unTcM g ms msgs of
                           TcOk x ms msgs -> TcOk (f x) ms msgs
                           TcFail msgs    -> TcFail msgs)

instance ErrorMonad TcM where
  raise  = tcError . pp
  handle f g = TcM (\ms msgs -> case unTcM f ms msgs of
                                  TcFail (msg:msgs) -> unTcM (g (render msg)) ms msgs
                                  r                 -> r)

tcError :: Message -> TcM a
tcError msg = TcM (\ms msgs -> TcFail (msg : msgs))

tcWarn :: Message -> TcM ()
tcWarn msg = TcM (\ms msgs -> TcOk () ms (msg : msgs))

unimplemented str = fail ("Unimplemented: "++str)


runTcM :: TcM a -> Check a
runTcM f = case unTcM f IntMap.empty [] of
             TcOk x _ msgs -> do checkWarnings msgs; return x
             TcFail (msg:msgs) -> do checkWarnings msgs; checkError msg

newMeta :: Scope -> Sigma -> TcM MetaId
newMeta scope ty = TcM (\ms msgs ->
  let i = IntMap.size ms
  in TcOk i (IntMap.insert i (Unbound scope ty) ms) msgs)

getMeta :: MetaId -> TcM MetaValue
getMeta i = TcM (\ms msgs ->
  case IntMap.lookup i ms of
    Just mv -> TcOk mv ms msgs
    Nothing -> TcFail (("Unknown metavariable" <+> ppMeta i) : msgs))

setMeta :: MetaId -> MetaValue -> TcM ()
setMeta i mv = TcM (\ms msgs -> TcOk () (IntMap.insert i mv ms) msgs)

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
getMetaVars :: GLocation -> [(Scope,Sigma)] -> TcM [MetaId]
getMetaVars loc sc_tys = do
  tys <- mapM (\(scope,ty) -> zonkTerm =<< tc_value2term loc (scopeVars scope) ty) sc_tys
  return (foldr go [] tys)
  where
    -- Get the MetaIds from a term; no duplicates in result
    go (Vr tv)    acc = acc
    go (App x y)  acc = go x (go y acc)
    go (Meta i)   acc
      | i `elem` acc  = acc
      | otherwise     = i : acc
    go (Q _)      acc = acc
    go (QC _)     acc = acc
    go (Sort _)   acc = acc
    go (Prod _ _ arg res) acc = go arg (go res acc)
    go (Table p t) acc = go p (go t acc)
    go (RecType rs) acc = foldl (\acc (l,ty) -> go ty acc) acc rs
    go t acc = unimplemented ("go "++show t)

-- | This function takes account of zonking, and returns a set
-- (no duplicates) of free type variables
getFreeVars :: GLocation -> [(Scope,Sigma)] -> TcM [Ident]
getFreeVars loc sc_tys = do
  tys <- mapM (\(scope,ty) -> zonkTerm =<< tc_value2term loc (scopeVars scope) ty) sc_tys
  return (foldr (go []) [] tys)
  where
    go bound (Vr tv)            acc
      | tv `elem` bound             = acc
      | tv `elem` acc               = acc
      | otherwise                   = tv : acc
    go bound (App x y)          acc = go bound x (go bound y acc)
    go bound (Meta _)           acc = acc
    go bound (Q _)              acc = acc
    go bound (QC _)             acc = acc
    go bound (Prod _ x arg res) acc = go bound arg (go (x : bound) res acc)
    go bound (RecType rs) acc = foldl (\acc (l,ty) -> go bound ty acc) acc rs
    go bound (Table p t)        acc = go bound p (go bound t acc)

-- | Eliminate any substitutions in a term
zonkTerm :: Term -> TcM Term
zonkTerm (Meta i) = do
  mv <- getMeta i
  case mv of
    Unbound _ _ -> return (Meta i)
    Bound t     -> do t <- zonkTerm t
                      setMeta i (Bound t)       -- "Short out" multiple hops
                      return t
zonkTerm t = composOp zonkTerm t

tc_value2term loc xs v =
  return $ value2term loc xs v
    -- Old value2term error message:
    -- Left i  -> tcError ("Variable #" <+> pp i <+> "has escaped")



data TcA x a
  = TcSingle   (MetaStore -> [Message] -> TcResult a)
  | TcMany [x] (MetaStore -> [Message] -> [(a,MetaStore,[Message])])

mkTcA :: Err [a] -> TcA a a
mkTcA f = case f of
            Bad msg -> TcSingle  (\ms msgs -> TcFail (pp msg : msgs))
            Ok  [x] -> TcSingle  (\ms msgs -> TcOk x ms msgs)
            Ok  xs  -> TcMany xs (\ms msgs -> [(x,ms,msgs) | x <- xs])

singleTcA :: TcM a -> TcA x a
singleTcA = TcSingle . unTcM

bindTcA :: TcA x a -> (a -> TcM b) -> TcA x b
bindTcA f g = case f of
                TcSingle  f -> TcSingle (unTcM (TcM f >>= g))
                TcMany xs f -> TcMany xs (\ms msgs -> foldr add [] (f ms msgs))
  where
    add (y,ms,msgs) rs =
      case unTcM (g y) ms msgs of
        TcFail _       -> rs
        TcOk y ms msgs -> (y,ms,msgs):rs

runTcA :: ([x] -> TcM a) -> TcA x a -> TcM a
runTcA g f = TcM (\ms msgs -> case f of
                                TcMany xs f -> case f ms msgs of
                                                 [(x,ms,msgs)] -> TcOk x ms msgs
                                                 rs            -> unTcM (g xs) ms msgs
                                TcSingle f                     -> f ms msgs)
