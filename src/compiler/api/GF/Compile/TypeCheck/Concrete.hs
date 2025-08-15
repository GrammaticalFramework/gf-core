{-# LANGUAGE RankNTypes, CPP, TupleSections, LambdaCase #-}
module GF.Compile.TypeCheck.Concrete ( checkLType, checkLType', inferLType, inferLType' ) where

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
import Control.Applicative(Applicative(..),(<|>))
import Control.Monad(ap,liftM,liftM2,mplus,foldM,zipWithM,forM,filterM,unless)
import Control.Monad.ST
import GF.Text.Pretty
import Data.STRef
import Data.List (nub, (\\), tails)
import qualified Data.Map as Map
import Data.Maybe(fromMaybe,isNothing,mapMaybe)
import Data.Bifunctor(second)
import Data.Functor((<&>))
import qualified Control.Monad.Fail as Fail

checkLType :: Globals -> Term -> Type -> Check (Term, Type)
checkLType globals t ty = do
  res <- runEvalM globals $ do
            let (c1,c2) = split unit
            (t,vty) <- checkLType' c1 t (eval globals [] c2 ty [])
            ty <- value2termM True [] vty
            return (t,ty)
  case res of
    [tty] -> return tty
    _     -> checkError (pp "Encountered variants while type checking")

checkLType' :: Choice -> Term -> Value -> EvalM (Term, Value)
checkLType' c t vty = do
  (t,vty) <- tcRho [] c t (Just vty)
  t <- zonkTerm [] t
  return (t,vty)

inferLType :: Globals -> Term -> Check (Term, Type)
inferLType globals t = do
  res <- runEvalM globals $ do
            (t,vty) <- inferLType' t
            ty <- value2termM True [] vty
            return (t,ty)
  case res of
    [tty] -> return tty
    _     -> checkError (pp "Encountered variants while type checking")

inferLType' :: Term -> EvalM (Term, Value)
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

vtypeInt   = VApp poison (cPredef,cInt) []
vtypeFloat = VApp poison (cPredef,cFloat) []
vtypeStr   = VSort cStr
vtypeStrs  = VSort cStrs
vtypeType  = VSort cType
vtypePType = VSort cPType
vtypeMarkup= VApp poison (cPredef,cMarkup) []

tcRho :: Scope -> Choice -> Term -> Maybe Rho -> EvalM (Term, Rho)
tcRho scope s t@(EInt i)   mb_ty = instSigma scope s t (VInts i True) mb_ty -- INT
tcRho scope s t@(EFloat _) mb_ty = instSigma scope s t vtypeFloat mb_ty    -- FLOAT
tcRho scope s t@(K _)      mb_ty = instSigma scope s t vtypeStr   mb_ty    -- STR
tcRho scope s t@(Empty)    mb_ty = instSigma scope s t vtypeStr   mb_ty
tcRho scope s t@(Vr v)     mb_ty = do                          -- VAR
  case lookup v scope of
    Just v_sigma -> instSigma scope s t v_sigma mb_ty
    Nothing      -> evalError ("Unknown variable" <+> v)
tcRho scope c t@(Q id)     mb_ty = tcApp scope c t t [] mb_ty
tcRho scope c t@(QC id)    mb_ty = tcApp scope c t t [] mb_ty
tcRho scope c t@(App fun arg) mb_ty = tcApp scope c t t [] mb_ty
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
    check m n st (VApp c f vs)     = foldM (check m n) st vs
    check m n st (VMeta i vs)      = do
      state <- getMeta i
      case state of
        Bound _ v -> do g <- globals
                        check m n st (apply g v vs)
        _         -> foldM (check m n) st vs
    check m n st@(b,xs) (VGen i vs)
      | i == m                       = return (True, xs)
      | otherwise                    = return st
    check m n st (VClosure env c (Abs bt x t)) = do
      g <- globals
      check m (n+1) st (eval g ((x,VGen n []):env) c t [])
    check m n st (VProd _ x v1 v2) = do
      st@(b,xs) <- check m n st v1
      case v2 of
        VClosure env c t -> do g  <- globals
                               check m (n+1) (b,x:xs) (eval g ((x,VGen n []):env) c t [])
        v2               -> check m n st v2
    check m n st (VRecType as _)   = foldM (\st (l,_,v) -> check m n st v) st as
    check m n st (VR as)           =
      foldM (\st (lbl,tnk) -> check m n st tnk) st as
    check m n st (VP v l vs)       =
      check m n st v >>= \st -> foldM (check m n) st vs
    check m n st (VExtR v1 v2)     =
      check m n st v1 >>= \st -> check m n st v2
    check m n st (VTable v1 v2)    =
      check m n st v1 >>= \st -> check m n st v2
    check m n st (VT ty env c cs) =
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
    check m n st (VInts _ _) = return st
tcRho scope c t@(Abs Implicit var body) (Just ty) = do         -- ABS2
  (bt, x, var_ty, body_ty) <- unifyFun scope ty
  if bt == Implicit
    then return ()
    else evalError (ppTerm Unqualified 0 t <+> "is an implicit function, but no implicit function is expected")
  body_ty <- evalCodomain x (VGen (length scope) []) body_ty
  (body, body_ty) <- tcRho ((var,var_ty):scope) c body (Just body_ty)
  return (Abs Implicit var body,ty)
tcRho scope c (Abs Explicit var body) (Just ty) = do           -- ABS3
  (scope,f,ty') <- skolemise scope ty
  (_,x,var_ty,body_ty) <- unifyFun scope ty'
  body_ty <- evalCodomain x (VGen (length scope) []) body_ty
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
  (ts,ty) <- tcUnifying scope c ts mb_ty
  return (FV ts, ty)
tcRho scope s t@(Sort _) mb_ty = do
  instSigma scope s t vtypeType mb_ty
tcRho scope c t@(RecType rs) Nothing   = do
  (rs,mb_ty) <- tcRecTypeFields scope c [] rs Nothing
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
  (rs,mb_ty) <- tcRecTypeFields scope c [] rs (Just ty')
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
  mb_p_ty <- case tt of
               TRaw      -> return Nothing
               TTyped ty -> do let (c3,c4) = split c1
                               (ty, _) <- tcRho scope c3 ty (Just vtypeType)
                               g <- globals
                               return (Just (eval g (scopeEnv scope) c4 ty []))
  (ps,p_ty,res_ty) <- tcCases scope c2 ps mb_p_ty Nothing
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
                    subsCheckRho scope (Meta 0) (eval g (scopeEnv scope) c2 ty []) p_ty
                    return ()
  (ps,p_ty,res_ty) <- tcCases scope c3 ps (Just p_ty) (Just res_ty)
  p_ty_t <- value2termM True (scopeVars scope) p_ty
  return (f (T (TTyped p_ty_t) ps), VTable p_ty res_ty)
tcRho scope c (V p_ty ts) Nothing = do
  let (c1,c2,c3,c4) = split4 c
  (p_ty, _) <- tcRho scope c1 p_ty (Just vtypeType)
  i <- newResiduation scope
  let res_ty = VMeta i []

  let go c t = do (t, ty) <- tcRho scope c t Nothing
                  (t,_,_) <- subsCheckRho scope t ty res_ty
                  return t

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
  lttys <- inferRecFields scope c [] rs
  rs <- mapM (\(l,t,ty) -> value2termM True (scopeVars scope) ty >>= \ty -> return (l, (Just ty, t))) lttys
  return (R        rs,
          VRecType [(l,True,ty) | (l,t,ty) <- lttys] False
         )
tcRho scope c (R rs) (Just ty) = do
  (scope,f,ty') <- skolemise scope ty
  case ty' of
    (VRecType ltys _)->do lttys <- checkRecFields scope c [] rs ltys
                          rs <- mapM (\(l,t,ty) -> value2termM True (scopeVars scope) ty >>= \ty -> return (l, (Just ty, t))) lttys
                          return ((f . R)  rs,
                                  VRecType [(l,True,ty) | (l,t,ty) <- lttys] False
                                 )
    ty              -> do lttys <- inferRecFields scope c [] rs
                          t <- liftM (f . R) (mapM (\(l,t,ty) -> value2termM True (scopeVars scope) ty >>= \ty -> return (l, (Just ty, t))) lttys)
                          let ty' = VRecType [(l,True,ty) | (l,t,ty) <- lttys] False
                          (t,_,_) <- subsCheckRho scope t ty' ty
                          return (t, ty')
tcRho scope c (P t l) mb_ty = do
  l_ty   <- case mb_ty of
              Just ty -> return ty
              Nothing -> do i <- newResiduation scope
                            return (VMeta i [])
  (t,t_ty) <- tcRho scope c t (Just (VRecType [(l,True,l_ty)] True))
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
tcRho scope c t@(ExtR t1 t2) mb_ty =
  case (t2,mb_ty) of
    (R rs,Just (VRecType ltys ext)) -> do
       let ll2   = map fst rs
           (c1,c2) = split c

       (t1,ty1@(VRecType ltys1 ext)) <- tcRho scope c1 t1 (Just (VRecType [field | field@(l,_,_) <- ltys, not (elem l ll2)] ext))
       let (scope',proj1,wrap) = access scope t1 ty1

       lttys2 <- checkRecFields scope' c2 [] rs [field | field@(l,_,_) <- ltys, elem l ll2]
       let proj2 l =
             case [(Nothing,t) | (l',t,_) <- lttys2, l'==l] of
               []    -> Nothing
               (x:_) -> Just x

       return (wrap (R [(l,t) | (l,_,_) <- ltys, Just t <- [if elem l ll2 then proj2 l else proj1 l]]),
               VRecType ltys False
              )
    _ -> do
       let (c1,c2,c3,c4) = split4 c
       (t1,t1_ty) <- tcRho scope c1 t1 Nothing
       (t2,t2_ty) <- tcRho scope c2 t2 Nothing
       ty <- join t1_ty t2_ty
       let (scope1,proj1,wrap1) = access scope  t1 t1_ty
           (scope2,proj2,wrap2) = access scope1 t2 t2_ty
       let t = case (mb_ty,ty,t2_ty) of
                 (Just (VRecType ltys False), _, VRecType ltys2 False) ->
                    let ll2 = [l | (l,_,_) <- ltys2]
                    in (wrap1 . wrap2) (R [(l,t) | (l,_,_) <- ltys, Just t <- [if elem l ll2 then proj2 l else proj1 l]])
                 (_, VRecType ltys False, VRecType ltys2 False) ->
                    let ll2 = [l | (l,_,_) <- ltys2]
                    in (wrap1 . wrap2) (R [(l,t) | (l,_,_) <- ltys, Just t <- [if elem l ll2 then proj2 l else proj1 l]])
                 _ -> ExtR t1 t2
       return (t,ty)
  where
    access scope (R rs) ty = (scope
                             ,\l -> lookup l rs
                             ,id
                             )
    access scope (RecType rs) ty
                           = (scope
                             ,\l -> fmap ((,) Nothing) (lookup l rs)
                             ,id
                             )
    access scope t@(Vr x) ty
                           = (scope
                             ,\l  -> return (Nothing,P t l)
                             ,id
                             )
    access scope t      ty = let x = newVar scope
                             in (((x,ty):scope)
                                ,\l  -> return (Nothing,P (Vr x) l)
                                ,Let (x, (Nothing, t))
                                )

    join (VMeta i vs) ty2 = do
      mv <- getMeta i
      case mv of
        Bound _ v -> do
          g <- globals
          join (apply g v vs) ty2
        _ -> evalError (pp "Cannot type check record extensions when one of the types is a meta variable")
    join ty1 (VMeta j vs) = do
      mv <- getMeta j
      case mv of
        Bound _ v -> do
          g <- globals
          join ty1 (apply g v vs)
        _ -> evalError (pp "Cannot type check record extensions when one of the types is a meta variable")
    join (VSort s1) (VSort s2)
       | (s1 == cType || s1 == cPType) &&
         (s2 == cType || s2 == cPType) = let sort | s1 == cPType && s2 == cPType = cPType
                                                  | otherwise                    = cType
                                         in return (VSort sort)
    join (VRecType rs1 ext1) (VRecType rs2 ext2) = do
      rs <- foldM (\rs (l,o,ctr) -> extend l o ctr rs) rs1 rs2
      return (VRecType rs (ext1 || ext2))
      where
        extend l o1 ty1 [] = do return [(l,o1,ty1)]
        extend l o1 ty1 ((l',o2,ty2):rs)
          | l == l'   = do return ((l,o1,ty1):rs)
          | otherwise = do rs <- extend l o1 ty1 rs
                           return ((l',o2,ty2):rs)
    join ty1            ty2            = do ty1 <- value2termM False (scopeVars scope) ty1
                                            ty2 <- value2termM False (scopeVars scope) ty2
                                            evalError ("Cannot type check" <+> ppTerm Unqualified 0 t $$
                                                       "       with types" <+> (ppTerm Unqualified 0 ty1 $$
                                                                                ppTerm Unqualified 0 ty2))
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
tcRho scope c t@(EPatt _ _ p) mb_ty = do
  (scope,f,mb_ty) <- case mb_ty of
                       Nothing -> return (scope,id,Nothing)
                       Just ty -> do (scope,f,ty) <- skolemise scope ty
                                     case ty of
                                       VPattType ty -> return (scope,f,Just ty)
                                       _            -> evalError (ppTerm Unqualified 0 t <+> "must be of pattern type but" <+> ppTerm Unqualified 0 t <+> "is expected")
  (_,ty) <- tcPatt scope c p mb_ty
  (min,max,p) <- measurePatt p
  return (f (EPatt min max p), VPattType ty)
tcRho scope c (Markup tag attrs children) mb_ty = do
  let (c1,c2,c3,c4) = split4 c
  attrs <- mapCM (\c (id,t) -> do
                       (t,_) <- tcRho scope c t Nothing
                       return (id,t))
                 c1 attrs
  res <- mapCM (\c child -> tcRho scope c child Nothing) c2 children
  instSigma scope c3 (Markup tag attrs (map fst res)) vtypeMarkup mb_ty
tcRho scope c (Reset ctl mb_ct t qid) mb_ty
  | ctl == cConcat || ctl == cConcat' = do
      let (c1,c23) = split c
          (c2,c3 ) = split c23
      (t,_) <- tcRho scope c1 t Nothing
      mb_ct <- case mb_ct of
                 Just ct -> do (ct,_) <- tcRho scope c2 ct (Just vtypeInt)
                               return (Just ct)
                 Nothing -> return Nothing
      instSigma scope c2 (Reset ctl mb_ct t qid) vtypeMarkup mb_ty
  | ctl == cOne = do
      let (c1,c2) = split c
      (t,ty)     <- tcRho scope c1 t mb_ty
      (mb_ct,ty) <- case mb_ct of
                      Just ct -> do (ct,ty) <- tcRho scope c2 ct (Just ty)
                                    return (Just ct,ty)
                      Nothing -> return (Nothing,ty)
      return (Reset ctl mb_ct t qid,ty)
  | ctl == cSelect = do
      let (c1,c2) = split c
      ty <- case mb_ty of
              Just ty -> return ty
              Nothing -> do i <- newResiduation scope
                            return (VMeta i [])
      let rec_ty = VRecType [ (ident2label cp1, True, ty)
                            , (ident2label cp2, True, VSort cStr)
                            ] False
      mb_ct <- case mb_ct of
                 Just ct -> do (ct,_) <- tcRho scope c2 ct (Just vtypeInt)
                               return (Just ct)
                 Nothing -> evalError (pp "[select: .. | ..] requires an integer argument")
      (t,_) <- tcRho scope c1 t (Just rec_ty)
      return (Reset ctl mb_ct t qid,ty)
  | ctl == cDefault = do
      let (c1,c2) = split c
      (t,ty)     <- tcRho scope c1 t mb_ty
      (mb_ct,ty) <- case mb_ct of
                      Just ct -> do (ct,ty) <- tcRho scope c2 ct (Just ty)
                                    return (Just ct,ty)
                      Nothing -> evalError (pp "[list: .. | ..] requires an argument")
      return (Reset ctl mb_ct t qid,ty)
  | ctl == cList = do
      do let (c1,c2) = split c
         mb_ct  <- case mb_ct of
                     Just ct -> do (ct,ty) <- tcRho scope c1 ct Nothing
                                   return (Just ct)
                     Nothing -> evalError (pp "[list: .. | ..] requires an argument")
         (t,ty) <- tcRho scope c2 t mb_ty
         case ty of
           VApp c qid [] -> return (Reset ctl mb_ct t (Just qid), ty)
           _             -> evalError (pp "Needs atomic type"<+>ppValue Unqualified 0 ty)
  | ctl == cLen = do
      do let (c1,c2) = split c
         (t,_) <- tcRho scope c1 t Nothing
         case mb_ct of
           Just ct -> do res_ty <- case mb_ty of
                                     Just ty -> return ty
                                     Nothing -> do i <- newResiduation scope
                                                   return (VMeta i [])
                         (ct,_) <- tcRho scope c2 ct (Just (VProd Explicit identW vtypeInt res_ty))
                         return (Reset ctl (Just ct) t Nothing, res_ty)
           Nothing -> instSigma scope c2 (Reset ctl Nothing t Nothing) vtypeInt mb_ty
  | otherwise = evalError (pp "Operator" <+> pp ctl <+> pp "is not defined")
tcRho scope s (Opts n cs) mb_ty = do
  let (s1,s2,s3) = split3 s
  (n,_) <- tcRho scope s1 n Nothing
  (ls,_) <- tcUnifying scope s2 (fst <$> cs) Nothing
  (ts,ty) <- tcUnifying scope s3 (snd <$> cs) mb_ty
  return (Opts n (zip ls ts), ty)
tcRho scope s t _ = unimplemented ("tcRho "++show t)

evalCodomain :: Ident -> Value -> Value -> EvalM Value
evalCodomain x v (VClosure env c ty) = do
  g <- globals
  return (eval g ((x,v):env) c ty [])
evalCodomain x _ ty = return ty

tcUnifying :: Scope -> Choice -> [Term] -> Maybe Rho -> EvalM ([Term], Value)
tcUnifying scope c ts mb_ty = do
  (ty,subsume) <-
    case mb_ty of
      Just ty -> do return (ty, \t ty' -> return t)
      Nothing -> do i <- newResiduation scope
                    let ty = VMeta i []
                    return (ty, \t ty' -> subsCheckRho scope t ty' ty >>= \(t,_,_) -> return t)

  let go c t = do (t, ty) <- tcRho scope c t mb_ty
                  subsume t ty

  ts <- mapCM go c ts
  return (ts,ty)

tcCases scope c []         (Just p_ty) (Just res_ty) = return ([],p_ty,res_ty)
tcCases scope c ((p,t):cs) mb_p_ty     mb_res_ty     = do
  let (c1,c2,c3,c4) = split4 c
  (scope',p_ty) <- tcPatt scope c1 p mb_p_ty
  (t,res_ty)  <- tcRho scope' c2 t mb_res_ty
  (cs,p_ty,res_ty) <- tcCases scope c3 cs (Just p_ty) (Just res_ty)
  (_,_,p) <- measurePatt p
  return ((p,t):cs,p_ty,res_ty)

tcApp scope c t0 (App fun arg) args mb_ty = tcApp scope c t0 fun (arg:args) mb_ty     -- APP
tcApp scope c t0 t@(Q id)      args mb_ty = resolveOverloads scope c t0 id args mb_ty -- VAR (global)
tcApp scope c t0 t@(QC id)     args mb_ty = resolveOverloads scope c t0 id args mb_ty -- VAR (global)
tcApp scope c t0 t             args mb_ty = do
  let (c1,c23) = split c
  let (c2,c3)  = split c23
  (t,ty) <- tcRho scope c1 t Nothing
  (t,ty) <- reapply1 scope c2 t ty args
  instSigma scope c3 t ty mb_ty

reapply1 :: Scope -> Choice -> Term -> Value -> [Term] -> EvalM (Term,Rho)
reapply1 scope c fun fun_ty []                   = return (fun,fun_ty)
reapply1 scope c fun fun_ty ((ImplArg arg):args) = do -- Implicit arg case
  let (c1,c2,c3,c4) = split4 c
  (bt, x, arg_ty, res_ty) <- unifyFun scope fun_ty
  unless (bt == Implicit) $
     evalError (ppTerm Unqualified 0 (App fun (ImplArg arg)) <+>
                "is an implicit argument application, but no implicit argument is expected")
  (arg,_) <- tcRho scope c1 arg (Just arg_ty)
  g <- globals
  res_ty <- evalCodomain x (eval g (scopeEnv scope) c2 arg []) res_ty
  reapply1 scope c3 (App fun (ImplArg arg)) res_ty args
reapply1 scope c fun fun_ty (arg:args) = do -- Explicit arg (fallthrough) case
  let (c1,c2,c3,c4) = split4 c
  (fun,fun_ty) <- instantiate scope fun fun_ty
  (_, x, arg_ty, res_ty) <- unifyFun scope fun_ty
  (arg,_) <- tcRho scope c1 arg (Just arg_ty)
  g <- globals
  res_ty <- evalCodomain x (eval g (scopeEnv scope) c2 arg []) res_ty
  reapply1 scope c3 (App fun arg) res_ty args

resolveOverloads :: Scope -> Choice -> Term -> QIdent -> [Term] -> Maybe Rho -> EvalM (Term,Rho)
resolveOverloads scope c t0 q args mb_ty = do
  g@(Gl gr _) <- globals
  case lookupOverloadTypes gr q of
    Bad msg  -> evalError (pp msg)
    Ok [(t,ty)] -> do let (c1,c23) = split c
                          (c2,c3)  = split c23
                      (t,ty) <- reapply1 scope c1 t (eval g [] c2 ty []) args
                      instSigma scope c3 t ty mb_ty
    Ok ttys     -> do let (c1,c23) = split c
                          (c2,c3)  = split c23
                      sz <- checkpoint
                      arg_tys <- mapCM (checkArg g) c1 args
                      let v_ttys = mapC (\c (t,ty) -> (t,eval g [] c ty [])) c2 ttys
                      try sz
                          (\(fun,fun_ty) -> reapply2 scope c3 fun fun_ty arg_tys mb_ty)
                          (\ttys -> fmap (\(ts,ty) -> (mkFV ts,ty)) (snd (minimum g ttys)))
                          v_ttys
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

    mkFV [t] = t
    mkFV ts  = FV ts

    minimum g []                    = (maxBound,err)
      where
        err = evalError (pp "Overload resolution failed")
    minimum g (tty@(t,ty):ttys) =
      let a        = arity ty
          (a',res) = minimum g ttys
      in case compare a a' of
           GT -> (a',res)
           EQ -> (a',join t ty res)
           LT -> (a ,one  t ty)
      where
        arity :: Value -> Int
        arity (VProd _ _ _ ty) = 1 + arity ty
        arity _                = 0

        one t ty = do
          return ([t],ty)

        join t ty res = do
          (ts,ty') <- res
          ty <- supertype scope (Just ty) ty'
          return (t:ts,ty)

reapply2 :: Scope -> Choice -> Term -> Value -> [(Term,Value,Value)] -> Maybe Rho -> EvalM (Term,Rho)
reapply2 scope c fun fun_ty []                                mb_ty = do
  (fun,fun_ty) <- instSigma scope c fun fun_ty mb_ty
  fun <- zonkTerm (scopeVars scope) fun
  return (fun,fun_ty)
reapply2 scope c fun fun_ty ((ImplArg arg,arg_v,arg_ty):args) mb_ty = do -- Implicit arg case
  (bt, x, arg_ty', res_ty) <- unifyFun scope fun_ty
  unless (bt == Implicit) $
     evalError (ppTerm Unqualified 0 (App fun (ImplArg arg)) <+>
                "is an implicit argument application, but no implicit argument is expected")
  (arg,_,_) <- subsCheckRho scope arg arg_ty' arg_ty
  res_ty <- evalCodomain x arg_v res_ty
  reapply2 scope c (App fun (ImplArg arg)) res_ty args mb_ty
reapply2 scope c fun fun_ty ((arg,arg_v,arg_ty):args) mb_ty = do -- Explicit arg (fallthrough) case
  (fun,fun_ty) <- instantiate scope fun fun_ty
  (_, x, arg_ty', res_ty) <- unifyFun scope fun_ty
  (arg,_,_) <- subsCheckRho scope arg arg_ty arg_ty'
  res_ty <- evalCodomain x arg_v res_ty
  reapply2 scope c (App fun arg) res_ty args mb_ty

tcPatt scope c PW        Nothing    = do
  i <- newResiduation scope
  return (scope,VMeta i [])
tcPatt scope c PW        (Just ty0) =
  return (scope,ty0)
tcPatt scope c (PV x)    Nothing    = do
  i <- newResiduation scope
  let ty = VMeta i []
  return ((x,ty):scope,ty)
tcPatt scope c (PV x)    (Just ty) =
  return ((x,ty):scope,ty)
tcPatt scope c (PP q ps) mb_ty = do
  g@(Gl gr _) <- globals
  ty <- case lookupResType gr q of
          Ok ty   -> return ty
          Bad msg -> evalError (pp msg)
  let go scope c ty []     = return (scope,ty)
      go scope c ty (p:ps) = do (_,_,arg_ty,res_ty) <- unifyFun scope ty
                                let (c1,c2) = split c
                                (scope,arg_ty) <- tcPatt scope c1 p (Just arg_ty)
                                go scope c2 res_ty ps
  let (c1,c2) = split c
  (scope,res_ty) <- go scope c1 (eval g [] c2 ty []) ps
  case mb_ty of
    Just ty -> unify scope ty res_ty
    Nothing -> return ()
  return (scope,res_ty)
tcPatt scope c p@(PInt i) mb_ty =
  case mb_ty of
    Just ty0@(VInts n ext)
       | i <= n    -> return (scope,ty0)
       | ext       -> return (scope,VInts i ext)
       | otherwise -> evalError ("Ints" <+> i <+> "is not a subtype of" <+> ppValue Unqualified 0 ty0)
    Just ty0@(VMeta k vs) -> do
       mv <- getMeta k
       case mv of
         Bound scope1 v -> do
           g  <- globals
           (scope,ty) <- tcPatt scope c p (Just (apply g v vs))
           setMeta k (Bound scope1 ty)
           return (scope,ty0)
         Residuation scope1 -> do
           setMeta k (Bound scope1 (VInts i True))
           return (scope,ty0)
    Nothing -> return (scope,VInts i True)
    _ -> evalError (pp "An integer must have an Int or Ints n type")
tcPatt scope c (PString s) mb_ty = do
  case mb_ty of
    Just ty -> unify scope ty vtypeStr
    Nothing -> return ()
  return (scope,vtypeStr)
tcPatt scope c PChar mb_ty = do
  case mb_ty of
    Just ty -> unify scope ty vtypeStr
    Nothing -> return ()
  return (scope,vtypeStr)
tcPatt scope c (PChars cs) mb_ty = do
  case mb_ty of
    Just ty -> unify scope ty vtypeStr
    Nothing -> return ()
  return (scope,vtypeStr)
tcPatt scope c (PSeq _ _ p1 _ _ p2) mb_ty = do
  case mb_ty of
    Just ty -> unify scope ty vtypeStr
    Nothing -> return ()
  let (c1,c2) = split c
  (scope,_) <- tcPatt scope c1 p1 (Just vtypeStr)
  (scope,_) <- tcPatt scope c2 p2 (Just vtypeStr)
  return (scope,vtypeStr)
tcPatt scope c (PRep _ _ p) mb_ty = do
  case mb_ty of
    Just ty -> unify scope ty vtypeStr
    Nothing -> return ()
  tcPatt scope c p (Just vtypeStr)
tcPatt scope c (PAs x p) mb_ty = do
  ty <- case mb_ty of
          Just ty -> return ty
          Nothing -> do i <- newResiduation scope
                        return (VMeta i [])
  tcPatt ((x,ty):scope) c p (Just ty)
tcPatt scope c p@(PR rs) mb_ty =
  case mb_ty of
    Just (VRecType ltys ext) -> check scope c rs ltys ext
    Just ty0@(VMeta i vs) -> do
       mv <- getMeta i
       case mv of
         Bound scope1 v ->
              do g <- globals
                 (scope,ty) <- tcPatt scope c p (Just (apply g v vs))
                 setMeta i (Bound scope1 ty)
                 return (scope,ty0)
         Residuation scope1 ->
              do (scope,ltys) <- infer scope c rs
                 setMeta i (Bound scope1 (VRecType ltys True))
                 return (scope,ty0)
    Nothing ->do (scope,ltys) <- infer scope c rs
                 return (scope,VRecType ltys True)
    _ -> evalError (pp "An record must have an record type")
  where
    check scope c []         ltys ext = return (scope,VRecType ltys ext)
    check scope c ((l,p):rs) ltys ext =
      case lookup3 l ltys of
        Just ty -> do let (c1,c2) = split c
                      (scope,ty) <- tcPatt scope c1 p (Just ty)
                      check scope c2 rs (update3 l True ty ltys) ext
        Nothing
          | ext -> do let (c1,c2) = split c
                      (scope,ty) <- tcPatt scope c1 p Nothing
                      check scope c2 rs (ltys++[(l,True,ty)]) ext
          | otherwise
                -> do ty <- value2termM False (scopeVars scope) (VRecType ltys ext)
                      evalError (pp "Label" <+> pp l <+> " is not defined in the type of the pattern:" $$
                                 nest 4 (ppTerm Unqualified 0 ty))

    infer scope c []         = return (scope,[])
    infer scope c ((l,p):rs) = do
      let (c1,c2) = split c
      (scope,ty) <- tcPatt scope c1 p Nothing
      (scope,ltys) <- infer scope c2 rs
      return (scope,(l,True,ty):ltys)
tcPatt scope c (PNeg p) mb_ty = do
  (_,ty) <- tcPatt scope c p mb_ty
  return (scope, ty)
tcPatt scope c (PAlt p1 p2) mb_ty = do
  let (c1,c2) = split c
  (_,ty) <- tcPatt scope c1 p1 mb_ty
  (_,ty) <- tcPatt scope c2 p2 (Just ty)
  return (scope,ty)
tcPatt scope c (PM q) mb_ty = do
  g@(Gl gr _) <- globals
  ty <- case lookupResType gr q of
          Ok ty   -> return ty
          Bad msg -> evalError (pp msg)
  case ty of
    EPattType ty
         -> do let vty = eval g [] c ty []
               case mb_ty of
                 Just ty0 -> unify scope ty0 vty
                 Nothing  -> return ()
               return (scope,vty)
    ty   -> evalError ("Pattern type expected but " <+> pp ty <+> " found.")
tcPatt scope c p ty = unimplemented ("tcPatt "++show p)

measurePatt p =
  case p of
    PM q       -> do g <- globals
                     case eval g [] unit (Q q) [] of
                       VPatt minp maxp _ -> return (minp,maxp,p)
                       v                 -> evalError ("Expected pattern macro, but found:" $$ nest 2 (ppValue Unqualified 0 v))
    PR ass     -> do ass <- mapM (\(lbl,p) -> measurePatt p >>= \(_,_,p') -> return (lbl,p')) ass
                     return (0,Nothing,PR ass)
    PString s  -> do let len=length s
                     return (len,Just len,p)
    PT t p     -> do (min,max,p') <- measurePatt p
                     return (min,max,PT t p')
    PAs x p    -> do (min,max,p) <- measurePatt p
                     case p of
                       PW -> return (0,Nothing,PV x)
                       _  -> return (min,max,PAs x p)
    PImplArg p -> do (min,max,p') <- measurePatt p
                     return (min,max,PImplArg p')
    PNeg p     -> do (_,_,p') <- measurePatt p
                     return (0,Nothing,PNeg p')
    PAlt p1 p2 -> do (min1,max1,p1) <- measurePatt p1
                     (min2,max2,p2) <- measurePatt p2
                     case (p1,p2) of
                       (PString [c1],PString [c2]) -> return (1,Just 1,PChars [c1,c2])
                       (PString [c], PChars cs)    -> return (1,Just 1,PChars ([c]++cs))
                       (PChars cs,   PString [c])  -> return (1,Just 1,PChars (cs++[c]))
                       (PChars cs1,  PChars cs2)   -> return (1,Just 1,PChars (cs1++cs2))
                       _                           -> return (min min1 min2,liftM2 max max1 max2,PAlt p1 p2)
    PSeq _ _ p1 _ _ p2
               -> do (min1,max1,p1) <- measurePatt p1
                     (min2,max2,p2) <- measurePatt p2
                     case (p1,p2) of
                       (PW,        PW        ) -> return (0,Nothing,PW)
                       (PString s1,PString s2) -> return (min1+min2,liftM2 (+) max1 max2,PString (s1++s2))
                       _                       -> return (min1+min2,liftM2 (+) max1 max2,PSeq min1 max1 p1 min2 max2 p2)
    PRep _ _ p -> do (minp,maxp,p) <- measurePatt p
                     case p of
                       PW    -> return (0,Nothing,PW)
                       PChar -> return (0,Nothing,PW)
                       _     -> return (0,Nothing,PRep minp maxp p)
    PChar      -> return (1,Just 1,p)
    PChars _   -> return (1,Just 1,p)
    _          -> return (0,Nothing,p)

inferRecFields scope c ls []          = return []
inferRecFields scope c ls ((l,t):lts)
  | elem l ls = evalError ("Repeated definition for field" <+> l)
  | otherwise = do
      let (c1,c2) = split c
      lt  <- tcRecField scope c1 l t Nothing
      lts <- inferRecFields scope c2 (l:ls) lts
      return (lt:lts)

checkRecFields scope c ls []          ltys
  | null ltys                            = return []
  | otherwise                            = evalError ("Missing fields:" <+> hsep [l | (l,_,_) <- ltys])
checkRecFields scope c ls ((l,t):lts) ltys
  | elem l ls = evalError ("Repeated definition for field" <+> l)
  | otherwise =
      case takeIt l ltys of
        (Just ty,ltys) -> do let (c1,c2) = split c
                             ltty  <- tcRecField scope c1 l t (Just ty)
                             lttys <- checkRecFields scope c2 ls lts ltys
                             return (ltty : lttys)
        (Nothing,ltys) -> do evalWarn ("Discarded field:" <+> l)
                             lttys <- checkRecFields scope c ls lts ltys
                             return lttys     -- ignore the field
  where
    takeIt l1 []  = (Nothing, [])
    takeIt l1 (lty@(l2,_,ty):ltys)
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

tcRecTypeFields scope c ls []          mb_ty = return ([],mb_ty)
tcRecTypeFields scope c ls ((l,ty):rs) mb_ty
  | elem l ls = evalError ("Repeated definition for field" <+> l)
  | otherwise = do
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
      (rs,mb_ty) <- tcRecTypeFields scope c2 (l:ls) rs mb_ty
      return ((l,ty):rs,mb_ty)

-- | Invariant: if the third argument is (Just rho),
--              then rho is in weak-prenex form
instSigma :: Scope -> Choice -> Term -> Sigma -> Maybe Rho -> EvalM (Term, Rho)
instSigma scope s t ty1 Nothing    = return (t,ty1)           -- INST1
instSigma scope s t ty1 (Just ty2) = do                       -- INST2
  (t,ty1,ty2) <- subsCheckRho scope t ty1 ty2
  return (t,ty2)

-- | Invariant: the second argument is in weak-prenex form
subsCheckRho :: Scope -> Term -> Sigma -> Rho -> EvalM (Term,Sigma,Rho)
subsCheckRho scope t ty1@(VApp _ p1 []) ty2                   -- for backwards compatibility
  | p1 == (cPredef,cErrorType) = return (t,ty1,ty2)
subsCheckRho scope t ty1 ty2@(VApp _ p2 [])                   -- for backwards compatibility
  | p2 == (cPredef,cErrorType) = return (t,ty1,ty2)
subsCheckRho scope t ty1@(VMeta i vs1) ty2@(VMeta j vs2)
  | i  == j   = do sequence_ (zipWith (unify scope) vs1 vs2)
                   return (t,ty1,ty2)
  | otherwise = do
      mv <- getMeta i
      case mv of
        Bound _ v1 -> do
          g <- globals
          subsCheckRho scope t (apply g v1 vs1) (VMeta j vs2)
        Residuation scope1 -> do
          mv <- getMeta j
          case mv of
            Bound _ v2 -> do
              g <- globals
              subsCheckRho scope t (VMeta i vs1) (apply g v2 vs2)
            Residuation scope2
              | m > n     -> do setMeta i (Bound scope1 (VMeta j vs2))
                                return (t,VMeta j vs2,VMeta j vs2)
              | otherwise -> do setMeta j (Bound scope2 (VMeta i vs1))
                                return (t,VMeta i vs1,VMeta j vs1)
              where
                m = length scope1
                n = length scope2
subsCheckRho scope t ty1@(VMeta i vs) ty2 = do
  mv <- getMeta i
  case mv of
    Bound scope' ty1 -> do
      g <- globals
      (t,ty1,ty2) <- subsCheckRho scope t (apply g ty1 vs) ty2
      setMeta i (Bound scope' ty1)
      return (t,ty1,ty2)
    Residuation scope' -> do
      occursCheck scope' i scope ty2
      ty1 <- subtype scope Nothing ty2
      setMeta i (Bound scope' ty1)
      return (t,ty1,ty2)
subsCheckRho scope t ty1 ty2@(VMeta i vs) = do
  mv <- getMeta i
  case mv of
    Bound scope' ty2 -> do
      g <- globals
      (t,ty1,ty2) <- subsCheckRho scope t ty1 (apply g ty2 vs)
      setMeta i (Bound scope' ty2)
      return (t,ty1,ty2)
    Residuation scope' -> do
      occursCheck scope' i scope ty1
      ty2 <- supertype scope Nothing ty1
      setMeta i (Bound scope' ty2)
      return (t,ty1,ty2)
subsCheckRho scope t (VProd Implicit x ty1 ty2) rho2 = do     -- Rule SPEC
  i <- newResiduation scope
  g <- globals
  let ty2' = case ty2 of
               VClosure env c ty2 -> eval g ((x,VMeta i []):env) c ty2 []
               ty2                -> ty2
  subsCheckRho scope (App t (ImplArg (Meta i))) ty2' rho2
subsCheckRho scope t rho1 (VProd Implicit x ty1 ty2) = do     -- Rule SKOL
  let v = newVar scope
  ty2 <- evalCodomain x (VGen (length scope) []) ty2
  (t,ty1,ty2) <- subsCheckRho ((v,ty1):scope) t rho1 ty2
  return (Abs Implicit v t,ty1,ty2)
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
subsCheckRho scope t ty1@(VSort s1) ty2@(VSort s2)            -- Rule PTYPE
  | s1 == cPType && s2 == cType = return (t,ty1,ty2)
subsCheckRho scope t ty1@(VApp _ p _) ty2@(VInts _ _)         -- This is not correct but nextPrec in the RGL relies on it.
  | p == (cPredef,cInt) = return (t,ty1,ty2)                  -- Should be only a temporary hack.
subsCheckRho scope t ty1@(VInts _ _) ty2@(VApp _ p _)         -- Rule INT1
  | p == (cPredef,cInt) = return (t,ty1,ty2)
subsCheckRho scope t ty1@(VInts n1 ext1) ty2@(VInts n2 ext2)  -- Rule INT2
  | n1 <= n2  = return (t,ty1,ty2)
  | ext2      = return (t,ty1,VInts n1 ext2)
  | otherwise = evalError ("In the term" <+> ppTerm Unqualified 0 t $$
                           ppValue Terse 0 ty1 <+> "is not a subtype of" <+> ppValue Terse 0 ty2)
subsCheckRho scope t ty1@(VRecType rs1 ext1) ty2@(VRecType rs2 ext2) = do      -- Rule REC
  let mkAccess scope t =
        case t of
          ExtR t1 (R rs) ->
                  do (scope,mkProj1,mkWrap1) <- mkAccess scope t1
                     sequence_ [evalWarn ("Discarded field:" <+> l) | (l,_) <- rs, isNothing (lookup3 l rs2)]
                     return (scope
                            ,\l -> lookup l rs `mplus` mkProj1 l
                            ,mkWrap1
                            )
          R rs -> do sequence_ [evalWarn ("Discarded field:" <+> l) | (l,_) <- rs, isNothing (lookup3 l rs2)]
                     return (scope
                            ,\l -> lookup l rs
                            ,id
                            )
          Vr x  -> return (scope
                          ,\l  -> return (Nothing,P t l)
                          ,\t' -> if is_trivial x t' then t else t'
                          )
          t    -> let x = newVar scope
                  in return (((x,ty1):scope)
                            ,\l  -> return (Nothing,P (Vr x) l)
                            ,\t' -> if is_trivial x t' then t else Let (x, (Nothing, t)) t'
                            )

      is_trivial x (R rs) = all is_selection rs
        where
          is_selection (l, (_, P (Vr u) l'))
            | l == l' && u == x = True
          is_selection _        = False
      is_trivial x _      = False

      mkField scope l (mb_ty,t) (Just ty1) ty2 = do
        (t,ty1,ty2) <- subsCheckRho scope t ty1 ty2
        return ((l, (mb_ty,t)), (l, True, ty1))
      mkField scope l (mb_ty,t) Nothing    ty2
        | isLockLabel l = return ((l, (Just (RecType []),R [])), (l, True, ty2))
        | otherwise     = return ((l, (mb_ty,t)), (l, True, ty2))

  (scope,mkProj,wrap) <- mkAccess scope t

  let fields = [(l,o2,ty2,lookup3 l rs1) | (l,o2,ty2) <- rs2]
  case [l | (l,_,_,Nothing) <- fields, not ext1 && not (isLockLabel l)] of
    []      -> return ()
    missing -> evalError ("In the term" <+> pp t $$
                          "there are no values for fields:" <+> hsep missing)
  rs <- sequence [mkField scope l t mb_ty1 ty2 | (l,_,ty2,mb_ty1) <- fields, Just t <- [mkProj l]]
  return (wrap (R (map fst rs)),VRecType (foldl (\rs (_,(l,o,ty)) -> update3 l o ty rs) rs1 rs) ext2,ty2)
subsCheckRho scope t ty1 (VFV c (VarFree vs)) = do
  ty2 <- variants c vs
  subsCheckRho scope t ty1 ty2
subsCheckRho scope t (VFV c (VarFree vs)) ty2 = do
  ty1 <- variants c vs
  subsCheckRho scope t ty1 ty2
subsCheckRho scope t ty1@(VPattType (VSort s1)) ty2@(VSort s2)      -- for backwards compatibility
  | s1 == cStr && s2 == cStrs = return (t,ty1,ty2)
subsCheckRho scope t ty1 ty2 = do                           -- Rule EQ
  unify scope ty1 ty2                                  -- Revert to ordinary unification
  return (t,ty1,ty2)

subsCheckFun :: Scope -> Term -> Sigma -> Value -> Sigma -> Value -> EvalM (Term,Value,Value)
subsCheckFun scope t a1 r1 a2 r2 = do
  let x = newVar scope
  (xt,a2,a1) <- subsCheckRho ((x,a2):scope) (Vr x) a2 a1
  g  <- globals
  let (x1',r1') = case r1 of
                    VClosure env c r1 -> (x,eval g ((x,(VGen (length scope) [])):env) c r1 [])
                    r1                -> (identW,r1)
      (x2',r2') = case r2 of
                    VClosure env c r2 -> (x,eval g ((x,(VGen (length scope) [])):env) c r2 [])
                    r2                -> (identW,r2)
  (t,r1,r2)  <- subsCheckRho ((x,a2):scope) (App t xt) r1' r2'
  case t of
    App t (Vr u) | u == x -> return (t, VProd Explicit x1' a1 r1, VProd Explicit x2' a2 r2)
    _                     -> return (Abs Explicit x t, VProd Explicit x1' a1 r1, VProd Explicit x2' a2 r2)

subsCheckTbl :: Scope -> Term -> Sigma -> Rho -> Sigma -> Rho  -> EvalM (Term,Value,Value)
subsCheckTbl scope t p1 r1 p2 r2 = do
  (scope,y,sel,wrap) <-
        case t of
          Vr x -> let y = newVar scope
                  in return ((y,p2):scope
                            ,y
                            ,\t -> S (Vr x) t
                            ,\p2 t' -> case t' of
                                         S (Vr u) (Vr v) | u == x && v == y -> t
                                         _                                  -> T (TTyped p2) [(PV y,t')]
                            )
          T _ [(PV x,t')] ->
                  let scope' = (x,p1):scope
                      y      = newVar scope'
                  in return (((y,p2):scope')
                            ,y
                            ,\t  -> Let (x, (Nothing, t)) t'
                            ,\p2 t -> case t of
                                        Let (u, (Nothing, Vr v)) t | u == x && v == y -> T (TTyped p2) [(PV x,t)]
                                        _                                             -> T (TTyped p2) [(PV y,t)]
                            )
          t    -> let x = newVar scope
                      scope' = (x,VTable p1 r1):scope
                      y = newVar scope'
                  in return (((y,VTable p1 r1):scope')
                            ,y
                            ,\t  -> S (Vr x) t
                            ,\p2 t' -> case t' of
                                         S (Vr u) (Vr v) | u == x && v == y -> t
                                         _                                  -> Let (x, (Nothing, t)) (T (TTyped p2) [(PV y,t')])
                            )
  (yt,p2,p1) <- subsCheckRho scope (Vr y) p2 p1
  (t,r1,r2)  <- subsCheckRho scope (sel yt) r1 r2
  p2_t <- value2termM True (scopeVars scope) p2
  return (wrap p2_t t,VTable p1 r1,VTable p2 r2)


subtype scope Nothing              (VInts i2 _) =
  return (VInts i2 True)
subtype scope (Just (VInts n1 _))  (VInts n2 _) =
  return (VInts (min n1 n2) False)
subtype scope Nothing (VRecType ltys ext) = do
  lctrs <- mapM (\(l,o,ty) -> subtype scope Nothing ty >>= \ctr -> return (l,o,ctr)) ltys
  return (VRecType lctrs ext)
subtype scope (Just (VRecType lctrs1 ext1)) (VRecType lctrs2 ext2) = do
  lctrs <- foldM (\lctrs (l,o,ctr) -> union l o ctr lctrs) lctrs1 lctrs2
  return (VRecType lctrs (ext1 || ext2))
  where
    union l o1 ctr1 [] = do ctr <- subtype scope Nothing ctr1
                            return [(l,True,ctr)]
    union l o1 ctr1 ((l',o2,ctr2):lctrs)
      | l == l'   = do ctr <- subtype scope (Just ctr1) ctr2
                       return ((l,o1||o2,ctr):lctrs)
      | otherwise = do lctrs <- union l o1 ctr1 lctrs
                       return ((l',o2,ctr2):lctrs)
subtype scope (Just (VTable a1 r1)) (VTable a2 r2) = do
  a <- supertype scope (Just a1) a2
  r <- subtype scope (Just r1) r2
  return (VTable a r)
subtype scope (Just (VProd Explicit x a1 r1)) (VProd Explicit y a2 r2)
  | x == identW && y == identW = do
       a <- supertype scope (Just a1) a2
       r <- subtype scope (Just r1) r2
       return (VProd Explicit identW a r)
subtype scope (Just (VApp _ p1 [])) ty2                      -- for backwards compatibility
  | p1 == (cPredef,cErrorType) = return ty2
subtype scope (Just ty1) (VApp _ p2 [])                      -- for backwards compatibility
  | p2 == (cPredef,cErrorType) = return ty1
subtype scope Nothing    ty = return ty
subtype scope (Just ctr) ty = do
  unify scope ctr ty
  return ty

supertype scope Nothing (VInts n2 _) =
  return (VInts n2 True)
supertype scope (Just (VInts n1 _)) (VInts n2 _) =
  return (VInts (max n1 n2) True)
supertype scope Nothing (VRecType ltys ext) = do
  lctrs <- mapM (\(l,o,ty) -> supertype scope Nothing ty >>= \ctr -> return (l,False,ctr)) ltys
  return (VRecType lctrs ext)
supertype scope (Just (VRecType lctrs1 ext1)) (VRecType lctrs2 ext2) = do
  lctrs <- foldM (\lctrs (l,o,ctr) -> intersect l o ctr lctrs lctrs2) [] lctrs1
  return (VRecType lctrs (ext1 || ext2))
  where
    intersect l o1 ctr1 lctrs [] = return lctrs
    intersect l o1 ctr1 lctrs ((l',o2,ctr2):lctrs2)
      | l == l'   = do ctr <- supertype scope (Just ctr1) ctr2
                       return ((l,o1 && o2,ctr):lctrs)
      | otherwise = do intersect l o1 ctr1 lctrs lctrs2
supertype scope (Just (VTable a1 r1)) (VTable a2 r2) = do
  a <- subtype scope (Just a1) a2
  r <- supertype scope (Just r1) r2
  return (VTable a r)
supertype scope (Just (VProd Explicit x a1 r1)) (VProd Explicit y a2 r2)
  | x == identW && y == identW = do
       a <- subtype scope (Just a1) a2
       r <- supertype scope (Just r1) r2
       return (VProd Explicit identW a r)
supertype scope (Just (VApp _ p1 [])) ty2                      -- for backwards compatibility
  | p1 == (cPredef,cErrorType) = return ty2
supertype scope (Just ty1) (VApp _ p2 [])                      -- for backwards compatibility
  | p2 == (cPredef,cErrorType) = return ty1
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
unifyFun scope (VFV c (VarFree vs)) = do
  res <- mapM (unifyFun scope) vs
  return
    ( Explicit
    , identW
    , VFV c (VarFree [sigma | (_,_,sigma,rho) <- res])
    , VFV c (VarFree [rho | (_,_,sigma,rho) <- res])
    )
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

unify scope (VApp c1 f1 vs1) (VApp c2 f2 vs2)
  | f1 == f2  = sequence_ (zipWith (unify scope) vs1 vs2)
unify scope (VMeta i vs1) (VMeta j vs2)
  | i  == j   = sequence_ (zipWith (unify scope) vs1 vs2)
  | otherwise = do 
      mv <- getMeta i
      case mv of
        Bound _ v1 -> do
          g <- globals
          unify scope (apply g v1 vs1) (VMeta j vs2)
        Residuation scope1 -> do
          mv <- getMeta j
          case mv of
            Bound _ v2 -> do
              g <- globals
              unify scope (VMeta i vs1) (apply g v2 vs2)
            Residuation scope2
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
      cod  <- evalCodomain x  (VGen (length scope) []) cod
      cod' <- evalCodomain x' (VGen (length scope) []) cod'
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
unify scope v1 v2 =
  evalError ("Cannot unify:" <+> ppValue Qualified 0 v1 $$
             "        with:" <+> ppValue Qualified 0 v2)


-- | Invariant: tv1 is a flexible type variable
unifyVar :: Scope -> MetaId -> [Value] -> Tau -> EvalM ()
unifyVar scope i vs ty2 = do            -- Check whether i is bound
  mv <- getMeta i
  case mv of
    Bound _ ty1        -> do g <- globals
                             unify scope (apply g ty1 vs) ty2
    Residuation scope' -> do occursCheck scope' i scope ty2
                             setMeta i (Bound scope' ty2)

occursCheck scope' i0 scope v =
  let m = length scope'
      n = length scope
  in check m n v
  where
    check m n (VApp c f vs) = mapM_ (check m n) vs
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
    check m n (VRecType as _) =
      mapM_ (\(_,_,v) -> check m n v) as
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
      mapM_ (check m n) (unvariants vs)
    check m n (VAlts v vs) =
      check m n v >> mapM_ (\(v1,v2) -> check m n v1 >> check m n v2) vs
    check m n (VStrs vs) =
      mapM_ (check m n) vs
    check m n (VInts _ _) = return ()

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
  Bound _ v -> instantiate scope t v
  _         -> return (t,ty) -- We don't have enough information to try any instantiation
instantiate scope t ty = do
  return (t,ty)

-- | Build fresh lambda abstractions for the topmost implicit arguments
skolemise :: Scope -> Sigma -> EvalM (Scope, Term->Term, Rho)
skolemise scope ty@(VMeta i vs) = do
  mv <- getMeta i
  case mv of
    Residuation _ -> return (scope,id,ty)                   -- guarded constant?
    Bound _ ty    -> do g <- globals
                        skolemise scope (apply g ty vs)
skolemise scope (VProd Implicit x ty1 ty2) = do
  let v = newVar scope
  ty2 <- evalCodomain x (VGen (length scope) []) ty2
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

    check m n xs (VApp c f vs)     = do
      (xs,vs) <- mapAccumM (check m n) xs vs
      return (xs,VApp c f vs)
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
    check m n xs (VRecType as ext)     = do
      (xs,as) <- mapAccumM (\xs (l,o,v) -> check m n xs v >>= \(xs,v) -> return (xs,(l,o,v))) xs as
      return (xs,VRecType as ext)
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
    check m n xs (VFV c (VarFree vs)) = do
      (xs,vs) <- mapAccumM (check m n) xs vs
      return (xs,VFV c (VarFree vs))
    check m n xs (VFV c (VarOpts name os)) = do
      (xs,os) <- mapAccumM (\acc (l,v) -> second (l,) <$> check m n acc v) xs os
      return (xs,VFV c (VarOpts name os))
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
    check m n xs v@(VInts _ _) = return (xs,v)
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

lookup3 l []  = Nothing
lookup3 l ((l',_,v):rs)
  | l == l'   = Just v
  | otherwise = lookup3 l rs

update3 l o v []  = [(l,o,v)]
update3 l o v (r@(l',_,_):rs)
  | l == l'   = (l,o,v) : rs
  | otherwise = r : update3 l o v rs

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
    go acc (VRecType vs _)   = foldM (\acc (lbl,_,v) -> go acc v) acc vs
    go acc (VClosure _ _ _)  = return acc
    go acc (VProd b x v1 v2) = go acc v2 >>= \acc -> go acc v1
    go acc (VTable v1 v2)    = go acc v2 >>= \acc -> go acc v1
    go acc (VMeta m args)
      | m `elem` acc         = return acc
      | otherwise            = do res <- getMeta m
                                  case res of
                                    Bound _ v -> go acc v
                                    _         -> foldM go (m:acc) args
    go acc (VApp c f args)   = foldM go acc args
    go acc (VFV c vs)        = foldM go acc (unvariants vs)
    go acc (VInts _ _)       = return acc
    go acc (VPattType v)     = go acc v
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
    Bound _ v -> zonkTerm xs =<< value2termM False xs v
    _         -> return (Meta i)
zonkTerm xs t = composOp (zonkTerm xs) t

zonkValue :: Value -> EvalM Value
zonkValue (VProd bt x ty1 ty2) = do
  ty1 <- zonkValue ty1
  ty2 <- zonkValue ty2
  return (VProd bt x ty1 ty2)
zonkValue (VMeta i vs)         = do
  g  <- globals
  st <- getMeta i
  case st of
    Bound _ v              -> zonkValue (apply g v vs)
    _                      -> do vs <- mapM zonkValue vs
                                 return (VMeta i vs)
zonkValue (VSusp i k vs)       = do
  g  <- globals
  st <- getMeta i
  case st of
    Bound _ v              -> zonkValue (apply g (k v) vs)
    _                      -> do vs <- mapM zonkValue vs
                                 return (VSusp i k vs)
zonkValue v                    = return v
