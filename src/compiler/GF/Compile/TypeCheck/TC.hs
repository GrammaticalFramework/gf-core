----------------------------------------------------------------------
-- |
-- Module      : TC
-- Maintainer  : AR
-- Stability   : (stable)
-- Portability : (portable)
--
-- > CVS $Date: 2005/10/02 20:50:19 $
-- > CVS $Author: aarne $
-- > CVS $Revision: 1.11 $
--
-- Thierry Coquand's type checking algorithm that creates a trace
-----------------------------------------------------------------------------

module GF.Compile.TypeCheck.TC (
    AExp(..),
    Theory,
    checkExp,
    checkNExp,
    inferExp,
    checkBranch,
    eqVal,
    whnf
  ) where

import GF.Data.Operations
import GF.Grammar
import GF.Grammar.Predef

import Control.Monad
--import Data.List (sortBy)
import Data.Maybe
import GF.Text.Pretty


data AExp =
     AVr   Ident Val
   | ACn   QIdent Val
   | AType
   | AInt  Int
   | AFloat Double
   | AStr  String
   | AMeta MetaId Val
   | ALet  (Ident,(Val,AExp)) AExp
   | AApp  AExp AExp Val
   | AAbs  Ident Val AExp
   | AProd Ident AExp AExp
-- -- | AEqs  [([Exp],AExp)] --- not used
   | ARecType [ALabelling]
   | AR    [AAssign]
   | AP    AExp Label Val
   | AGlue AExp AExp
   | AData Val
  deriving (Eq,Show)

type ALabelling = (Label, AExp)
type AAssign = (Label, (Val, AExp))

type Theory = QIdent -> Err (NotVal, Maybe (Int, [Equation]))

lookupConst ::  Theory -> QIdent -> Err (Val, Maybe (Int, [Equation]))
lookupConst th f = do
  (nv,info) <- th f
  -- checkWhnf (pp (snd f)) th v
  v <- whnf th nv
  return (v, info)

checkWhnf ::  Doc -> Theory -> Val -> Err ()
checkWhnf ctx th v = do
  v' <- whnf th (unVal v)
  when (v /= v') $ error . render $ ("Not whnf:" <+>) $
     ctx <+> ":" <+> (ppVal v <+> "<>" <+> ppVal v')
     $$ show v $$ show v'

ppVal = ppValue Unqualified 0

unVal :: Val -> NotVal
unVal (VClos e t) = NVClos e t
unVal v = NVVal v

lookupVar :: Env -> Ident -> Err Val
lookupVar g x = maybe (Bad (render ("unknown variable" <+> x))) return $ lookup x ((identW,VClos [] (Meta 0)):g)
-- wild card IW: no error produced, ?0 instead.

type TCEnv = (Int,Env,Env)

--emptyTCEnv :: TCEnv
--emptyTCEnv = (0,[],[])

whnf :: Theory -> NotVal -> Err Val
whnf th v = ---- errIn ("whnf" +++ prt v) $ ---- debug
         case v of
  NVVal v -> return v
  NVClos env e -> eval th env e

app :: Theory -> Val -> Val -> Err Val
app th u v = case u of
  VClos env (Abs _ x e) -> do
    val <- eval th ((x,v):env) e
    evalDef th val
  VApp u' v' -> do
    let val = VApp u' (v' ++ [v])
    evalDef th val
  _ -> do
    return $ VApp u [v]

evalDef :: Theory -> Val -> Err Val
evalDef th e = case e of
  VApp (VCn c (Just (n, eqs))) xs | length xs == n -> do
    e' <- tryMatchEqns eqs xs
    case e' of
      Just (NVClos env tm) -> eval th env tm
      _ -> return e
  _ -> do
    return e

tryMatchEqns :: [Equation] -> [Val] -> Err (Maybe NotVal)
tryMatchEqns ((pts,repl):eqs) xs = do
  me' <- tryMatchEqn [] pts xs repl
  case me' of
    Nothing -> tryMatchEqns eqs xs
    Just e' -> return $ Just e'
tryMatchEqns [] xs = return Nothing

tryMatchEqn :: Env -> [Patt] -> [Val] -> Term -> Err (Maybe NotVal)
tryMatchEqn env [] [] repl = return $ Just $ NVClos env repl
tryMatchEqn env (PW: ps) (v: vs) repl =
  tryMatchEqn env ps vs repl
tryMatchEqn env (PV c: ps) (v: vs) repl =
  tryMatchEqn ((c,v):env) ps vs repl -- TODO: Is this sound?
tryMatchEqn env (PP (q,p) pp: ps) (VApp (VCn (r,f) _) tt: vs) repl
  | p == f && length pp == length tt = tryMatchEqn env (pp ++ ps) (tt ++ vs) repl
-- tryMatchEqn env (p: ps) (v: vs) repl = _
tryMatchEqn env (a:_) (b:_) _ = do
  return Nothing
tryMatchEqn env _ _ _ = Bad "tryMatchEqn: Non-matching length"

eval :: Theory -> Env -> Term -> Err Val
eval th env e = ---- errIn ("eval" +++ prt e +++ "in" +++ prEnv env) $
             case e of
  Vr x    -> lookupVar env x
  Q c   -> do
    (_ty, defEqns) <- lookupConst th c
    return $ VCn c defEqns
  QC c  -> do
    (_ty, defEqns) <- lookupConst th c
    return $ VCn c defEqns ---- == Q ?
  Sort c  -> return $ VType --- the only sort is Type
  App f a -> do
    join $ liftM2 (app th) (eval th env f) (eval th env a)
  RecType xs -> do xs <- mapM (\(l,e) -> eval th env e >>= \e -> return (l,e)) xs
                   return (VRecType xs)
  _ -> do
    return $ VClos env e

eqNVal :: Theory -> Int -> NotVal -> NotVal -> Err [(Val,Val)]
eqNVal th k u1 u2 = do
  w1 <- whnf th u1
  w2 <- whnf th u2
  eqVal th k w1 w2

eqVal ::  Theory -> Int -> Val -> Val -> Err [(Val,Val)]
eqVal th k u1 u2 = ---- errIn (prt u1 +++ "<>" +++ prBracket (show k) +++ prt u2) $
                do
  -- w1 <- whnf th u1
  -- w2 <- whnf th u2
  let (w1,w2) = (u1,u2)
  let v = VGen k
  case (w1,w2) of
    (VApp f1 a1, VApp f2 a2) -> liftM2 (++) (eqVal th k f1 f2) (fmap concat $ zipWithM (eqVal th k) a1 a2)
    (VClos env1 (Abs _ x1 e1), VClos env2 (Abs _ x2 e2)) ->
      eqNVal th (k+1) (NVClos ((x1,v x1):env1) e1) (NVClos ((x2,v x1):env2) e2)
    (VClos env1 (Prod _ x1 a1 e1), VClos env2 (Prod _ x2 a2 e2)) ->
      liftM2 (++)
        (eqNVal th k     (NVClos            env1  a1) (NVClos            env2  a2))
        (eqNVal th (k+1) (NVClos ((x1,v x1):env1) e1) (NVClos ((x2,v x1):env2) e2))
    (VGen i _, VGen j _) -> return [(w1,w2) | i /= j]
    (VCn (_, i) _, VCn (_,j) _) -> return [(w1,w2) | i /= j]
    --- thus ignore qualifications; valid because inheritance cannot
    --- be qualified. Simplifies annotation. AR 17/3/2005
    _ -> do
      return [(w1,w2) | w1 /= w2]
-- invariant: constraints are in whnf

checkType :: Theory -> TCEnv -> Term -> Err (AExp,[(Val,Val)])
checkType th tenv e = checkExp th tenv e vType

checkNExp ::  Theory -> TCEnv -> Term -> NotVal -> Err (AExp, [(Val,Val)])
checkNExp th tenv@(k,rho,gamma) e ty = do
  typ <- whnf th ty
  checkWhnf (pp "checkNExp:") th typ
  checkExp th tenv e typ

checkExp ::  Theory -> TCEnv -> Term -> Val -> Err (AExp, [(Val,Val)])
checkExp th tenv@(k,rho,gamma) e typ = do
  let v = VGen k
  case e of
    Meta m -> return $ (AMeta m typ,[])

    Abs _ x t -> case typ of
      VClos env (Prod _ y a b) -> do
                           a' <- whnf th $ NVClos env a ---
                           (t',cs) <- checkNExp th
                                          (k+1,(x,v x):rho, (x,a'):gamma) t (NVClos ((y,v x):env) b)
                           return (AAbs x a' t', cs)
      _ -> Bad (render ("function type expected for" <+> ppTerm Unqualified 0 e <+> "instead of" <+> ppValue Unqualified 0 typ))

    Let (x, (mb_typ, e1)) e2 -> do
      (val,e1,cs1) <- case mb_typ of
                        Just typ -> do (_,cs1) <- checkType th tenv typ
                                       val <- eval th rho typ
                                       (e1,cs2) <- checkExp th tenv e1 val
                                       return (val,e1,cs1++cs2)
                        Nothing  -> do (e1,val,cs) <- inferExp th tenv e1
                                       return (val,e1,cs)
      (e2,cs2) <- checkExp th (k,rho,(x,val):gamma) e2 typ
      return (ALet (x,(val,e1)) e2, cs1++cs2)

    Prod _ x a b -> do
      testErr (typ == vType) "expected Type"
      (a',csa) <- checkType th tenv a
      aWhnf <- whnf th (NVClos rho a)
      (b',csb) <- checkType th (k+1, (x,v x):rho, (x,aWhnf):gamma) b
      return (AProd x a' b', csa ++ csb)

    R xs ->
      case typ of
        VRecType ys -> do case [l | (l,_) <- ys, isNothing (lookup l xs)] of
                            [] -> return ()
                            ls -> fail (render ("no value given for label:" <+> fsep (punctuate ',' ls)))
                          r <- mapM (checkAssign th tenv ys) xs
                          let (xs,css) = unzip r
                          return (AR xs, concat css)
        _           -> Bad (render ("record type expected for" <+> ppTerm Unqualified 0 e <+> "instead of" <+> ppValue Unqualified 0 typ))

    P r l -> do (r',cs) <- checkExp th tenv r (VRecType [(l,typ)])
                return (AP r' l typ,cs)

    Glue x y -> do cs1 <- eqVal th k valAbsFloat typ
                   (x,cs2) <- checkExp th tenv x typ
                   (y,cs3) <- checkExp th tenv y typ
                   return (AGlue x y,cs1++cs2++cs3)
    _ -> checkInferExp th tenv e typ

checkInferExp ::  Theory -> TCEnv -> Term -> Val -> Err (AExp, [(Val,Val)])
checkInferExp th tenv@(k,_,_) e typ = do
  (e',w,cs1) <- inferExp th tenv e
  checkWhnf (pp "checkInferExp w:") th w
  checkWhnf (pp "checkInferExp typ:") th typ
  cs2 <- eqVal th k w typ
  return (e',cs1 ++ cs2)

inferExp :: Theory -> TCEnv -> Term -> Err (AExp, Val, [(Val,Val)])
inferExp th tenv@(k,rho,gamma) e =
  case e of
   Vr x -> mkAnnot (AVr x) $ noConstr $ lookupVar gamma x
   Q (m,c) | m == cPredefAbs && isPredefCat c
                     -> return (ACn (m,c) vType, vType, [])
           | otherwise -> mkAnnot (ACn (m,c)) $ noConstr $ fmap fst $ lookupConst th (m,c)
   QC c -> mkAnnot (ACn c) $ noConstr $ fmap fst $ lookupConst th c ----
   EInt i -> return (AInt i, valAbsInt, [])
   EFloat i -> return (AFloat i, valAbsFloat, [])
   K i -> return (AStr i, valAbsString, [])
   Sort _ -> return (AType, vType, [])
   RecType xs -> do r <- mapM (checkLabelling th tenv) xs
                    let (xs,css) = unzip r
                    return (ARecType xs, vType, concat css)
   Let (x, (mb_typ, e1)) e2 -> do
    (val1,e1,cs1) <- case mb_typ of
                       Just typ -> do (_,cs1) <- checkType th tenv typ
                                      val <- eval th rho typ
                                      (e1,cs2) <- checkExp th tenv e1 val
                                      return (val,e1,cs1++cs2)
                       Nothing  -> do (e1,val,cs) <- inferExp th tenv e1
                                      return (val,e1,cs)
    (e2,val2,cs2) <- inferExp th (k,rho,(x,val1):gamma) e2
    return (ALet (x,(val1,e1)) e2, val2, cs1++cs2)
   App f t -> do
    (f',w,csf) <- inferExp th tenv f
    -- typ <- whnf th w
    let typ = w
    case typ of
      VClos env (Prod _ x a b) -> do
        (a',csa) <- checkNExp th tenv t (NVClos env a)
        rhoT <- whnf th $ NVClos rho t -- New whnf
        b' <- whnf th $ NVClos ((x,rhoT):env) b
        return $ (AApp f' a' b', b', csf ++ csa)
      _ -> Bad (render ("Prod expected for function" <+> ppTerm Unqualified 0 f <+> "instead of" <+> ppValue Unqualified 0 typ))
   _ -> Bad (render ("cannot infer type of expression" <+> ppTerm Unqualified 0 e))

checkLabelling :: Theory -> TCEnv -> Labelling -> Err (ALabelling, [(Val,Val)])
checkLabelling th tenv (lbl,typ) = do
  (atyp,cs) <- checkType th tenv typ
  return ((lbl,atyp),cs)

checkAssign :: Theory -> TCEnv -> [(Label,Val)] -> Assign -> Err (AAssign, [(Val,Val)])
checkAssign th tenv@(k,rho,gamma) typs (lbl,(Just typ,exp)) = do
  (atyp,cs1) <- checkType th tenv typ
  val <- eval th rho typ
  cs2 <- case lookup lbl typs of
           Nothing   -> return []
           Just val0 -> eqVal th k val val0
  (aexp,cs3) <- checkExp th tenv exp val
  return ((lbl,(val,aexp)),cs1++cs2++cs3)
checkAssign th tenv@(k,rho,gamma) typs (lbl,(Nothing,exp)) = do
  case lookup lbl typs of
    Nothing  -> do (aexp,val,cs) <- inferExp th tenv exp
                   return ((lbl,(val,aexp)),cs)
    Just val -> do (aexp,cs) <- checkExp th tenv exp val
                   return ((lbl,(val,aexp)),cs)

checkBranch :: Theory -> TCEnv -> Equation -> NotVal -> Err (([Term],AExp),[(Val,Val)])
checkBranch th tenv b@(ps,t) ty = errIn ("branch" +++ show b) $
                                  chB tenv' ps' ty
 where

  (ps',_,rho2,k') = ps2ts k ps
  tenv' = (k, rho2++rho, gamma) ---- k' ?
  (k,rho,gamma) = tenv

  chB :: (Int, [(Ident, Val)], [(Ident, Val)]) -> [Term] -> NotVal -> Err (([Term], AExp), [(Val, Val)])
  chB tenv@(k,rho,gamma) ps ty = case ps of
    p:ps2 -> do
      typ <- whnf th ty
      case typ of
        VClos env (Prod _ y a b) -> do
          a' <- whnf th $ NVClos env a
          (p', sigma, binds, cs1) <- checkP tenv p y a'
          let tenv' = (length binds, sigma ++ rho, binds ++ gamma)
          ((ps',exp),cs2) <- chB tenv' ps2 (NVClos ((y,p'):env) b)
          return ((p:ps',exp), cs1 ++ cs2) -- don't change the patt
        _ -> Bad (render ("Product expected for definiens" <+> ppTerm Unqualified 0 t <+> "instead of" <+> ppValue Unqualified 0 typ))
    [] -> do
      (e,cs) <- checkNExp th tenv t ty
      return (([],e),cs)
  checkP env@(k,rho,gamma) t x a = do
     (delta,cs) <- checkPatt th env t a
     let sigma = [(x, VGen i x) | ((x,_),i) <- zip delta [k..]]
    --  traceM . render $ ("\ncheckP:" <+>) 
    --     $ "Making closure:" <+> vcat ["" <+> ppTerm Unqualified 0 t , pp $ show (VClos sigma t)]
    --     $$ "In context:" <+> show tenv
     sigmaT <- whnf th $ NVClos sigma t -- New whnf
     return (sigmaT, sigma, delta, cs)

  ps2ts k = foldr p2t ([],0,[],k)
  p2t p (ps,i,g,k) = case p of
     PW      -> (Meta i : ps, i+1,g,k)
     PV x    -> (Vr x   : ps, i, upd x k g,k+1)
     PAs x p -> p2t p (ps,i,g,k)
     PString s -> (K s : ps, i, g, k)
     PInt n -> (EInt n : ps, i, g, k)
     PFloat n -> (EFloat n : ps, i, g, k)
     PP c xs -> (mkApp (Q c) xss : ps, j, g',k')
                    where (xss,j,g',k') = foldr p2t ([],i,g,k) xs
     PImplArg p -> p2t p (ps,i,g,k)
     PTilde t -> (t : ps, i, g, k)
     _ -> error $ render ("undefined p2t case" <+> ppPatt Unqualified 0 p <+> "in checkBranch")

  upd x k g = (x, VGen k x) : g --- hack to recognize pattern variables


checkPatt :: Theory -> TCEnv -> Term -> Val -> Err (Binds,[(Val,Val)])
checkPatt th tenv exp val = do
  (aexp,_,cs) <- checkExpP tenv exp val
  let binds = extrBinds aexp
  return (binds,cs)
 where
   extrBinds aexp = case aexp of
     AVr i v    -> [(i,v)]
     AApp f a _ -> extrBinds f ++ extrBinds a
     _ -> [] -- no other cases are possible

--- ad hoc, to find types of variables
   checkExpP :: (a, Env, c) -> Term -> Val -> Err (AExp, Val, [a2])
   checkExpP tenv@(k,rho,gamma) exp val = case exp of
     Meta m -> return $ (AMeta m val, val, [])
     Vr x   -> return $ (AVr   x val, val, [])
     EInt i -> return (AInt i, valAbsInt, [])
     EFloat i -> return (AFloat i, valAbsFloat, [])
     K s    -> return (AStr s, valAbsString, [])

     Q c  -> do
       (typ, _eqn) <- lookupConst th c
       return $ (ACn c typ, typ, [])
     QC c  -> do
       (typ, _eqn) <- lookupConst th c
       return $ (ACn c typ, typ, []) ----
     App f t -> do
       (f',w,csf) <- checkExpP tenv f val
      --  typ <- whnf th w
       let typ = w -- Already normalized
       case typ of
         VClos env (Prod _ x a b) -> do
          --  traceM . render $ ("\ncheckPatt:" <+>) 
          --     $ "Making closure:" <+> vcat ["" <+> ppTerm Unqualified 0 t , pp $ show (VClos env t)]
           envA <- whnf th $ NVClos env a -- New whnf
           (a',_,csa) <- checkExpP tenv t envA
           rhoT <- whnf th $ NVClos rho t -- New whnf
           b' <- whnf th $ NVClos ((x,rhoT):env) b
           checkWhnf (pp "checkPatt") th b'
           return $ (AApp f' a' b', b', csf ++ csa)
         _ -> Bad (render ("Prod expected for function" <+> ppTerm Unqualified 0 f <+> "instead of" <+> ppValue Unqualified 0 typ))
     _ -> Bad (render ("cannot typecheck pattern" <+> ppTerm Unqualified 0 exp))

-- auxiliaries

noConstr :: Err Val -> Err (Val,[(Val,Val)])
noConstr er = er >>= (\v -> return (v,[]))

mkAnnot :: (Val -> AExp) -> Err (Val,[(Val,Val)]) -> Err (AExp,Val,[(Val,Val)])
mkAnnot a ti = do
  (v,cs) <- ti
  return (a v, v, cs)
