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
import Debug.Trace (traceM)

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

type Theory = QIdent -> Err (Val, [Equation])

lookupConst :: Theory -> QIdent -> Err (Val, [Equation])
lookupConst th f = th f

lookupVar :: Env -> Ident -> Err Val
lookupVar g x = maybe (Bad (render ("unknown variable" <+> x))) return $ lookup x ((identW,VClos [] (Meta 0)):g)
-- wild card IW: no error produced, ?0 instead.

type TCEnv = (Int,Env,Env)

--emptyTCEnv :: TCEnv
--emptyTCEnv = (0,[],[])

whnf :: Theory -> Val -> Err Val
whnf th v = ---- errIn ("whnf" +++ prt v) $ ---- debug
         case v of
  VApp u w -> do
    u' <- whnf th u
    w' <- mapM (whnf th) w
    v' <- foldM app u' w'
    traceM . render $ "\nwhnf: Normalized app" <+> vcat (
      ["" <+> ppValue Unqualified 0 u <+> "to" <+> ppValue Unqualified 0 u' ] ++
      zipWith (\w w' -> ppValue Unqualified 0 w <+> "to" <+> ppValue Unqualified 0 w') w w' ++
      [ ppValue Unqualified 0 v <+> "to" <+> ppValue Unqualified 0 v' ])
    return v
  VClos env e -> do
    e' <- eval env e
    traceM . render $ "\nwhnf: Normalized Closure" <+> vcat
      ["" <+> ppTerm Unqualified 0 e <+> "to" <+> ppValue Unqualified 0 e' ]
    return e'
  _ -> do
    traceM . render $ "\nwhnf: unchanged" <+> ppValue Unqualified 0 v
    return v

app :: Val -> Val -> Err Val
app u v = case u of
  VClos env (Abs _ x e) -> eval ((x,v):env) e
  VApp u' v' -> do
    let val = VApp u' (v' ++ [v])
    traceM . render $ "\napp: Extended app:" <+>
      (ppValue Unqualified 0 u <+> "applied to" <+> ppValue Unqualified 0 v
       $$ take 150 (show val))
    return val
  _ -> do
    traceM . render $ "\napp: Unchanged app:" <+>
      (ppValue Unqualified 0 u <+> "applied to" <+> ppValue Unqualified 0 v
       $$ take 100 (show u))
    return $ VApp u [v]

eval :: Env -> Term -> Err Val
eval env e = ---- errIn ("eval" +++ prt e +++ "in" +++ prEnv env) $
             case e of
  Vr x    -> lookupVar env x
  Q c   -> return $ VCn c
  QC c  -> return $ VCn c ---- == Q ?
  Sort c  -> return $ VType --- the only sort is Type
  App f a -> join $ liftM2 app (eval env f) (eval env a)
  RecType xs -> do xs <- mapM (\(l,e) -> eval env e >>= \e -> return (l,e)) xs
                   return (VRecType xs)
  _ -> return $ VClos env e

eqVal :: Theory -> Int -> Val -> Val -> Err [(Val,Val)]
eqVal th k u1 u2 = ---- errIn (prt u1 +++ "<>" +++ prBracket (show k) +++ prt u2) $
                do
  w1 <- whnf th u1
  w2 <- whnf th u2
  let v = VGen k
  case (w1,w2) of
    (VApp f1 a1, VApp f2 a2) -> liftM2 (++) (eqVal th k f1 f2) (fmap concat $ zipWithM (eqVal th k) a1 a2)
    (VClos env1 (Abs _ x1 e1), VClos env2 (Abs _ x2 e2)) ->
      eqVal th (k+1) (VClos ((x1,v x1):env1) e1) (VClos ((x2,v x1):env2) e2)
    (VClos env1 (Prod _ x1 a1 e1), VClos env2 (Prod _ x2 a2 e2)) ->
      liftM2 (++)
        (eqVal th k     (VClos            env1  a1) (VClos            env2  a2))
        (eqVal th (k+1) (VClos ((x1,v x1):env1) e1) (VClos ((x2,v x1):env2) e2))
    (VGen i _, VGen j _) -> return [(w1,w2) | i /= j]
    (VCn (_, i), VCn (_,j)) -> return [(w1,w2) | i /= j]
    --- thus ignore qualifications; valid because inheritance cannot
    --- be qualified. Simplifies annotation. AR 17/3/2005
    _ -> return [(w1,w2) | w1 /= w2]
-- invariant: constraints are in whnf

checkType :: Theory -> TCEnv -> Term -> Err (AExp,[(Val,Val)])
checkType th tenv e = checkExp th tenv e vType

checkExp :: Theory -> TCEnv -> Term -> Val -> Err (AExp, [(Val,Val)])
checkExp th tenv@(k,rho,gamma) e ty = do
  typ <- whnf th ty
  traceM . render $ "\ncheckExp: Normalized" <+> vcat ["" <+> ppValue Unqualified 0 ty , ppValue Unqualified 0 typ]
  let v = VGen k
  traceM . render $ "\ncheckExp: checking" <+> vcat ["" <+> e , pp (take 30 $ show e)]
  case e of
    Meta m -> return $ (AMeta m typ,[])

    Abs _ x t -> case typ of
      VClos env (Prod _ y a b) -> do
                           a' <- whnf th $ VClos env a ---
                           (t',cs) <- checkExp th
                                          (k+1,(x,v x):rho, (x,a'):gamma) t (VClos ((y,v x):env) b)
                           return (AAbs x a' t', cs)
      _ -> Bad (render ("function type expected for" <+> ppTerm Unqualified 0 e <+> "instead of" <+> ppValue Unqualified 0 typ))

    Let (x, (mb_typ, e1)) e2 -> do
      (val,e1,cs1) <- case mb_typ of
                        Just typ -> do (_,cs1) <- checkType th tenv typ
                                       val <- eval rho typ
                                       (e1,cs2) <- checkExp th tenv e1 val
                                       return (val,e1,cs1++cs2)
                        Nothing  -> do (e1,val,cs) <- inferExp th tenv e1
                                       return (val,e1,cs)
      (e2,cs2) <- checkExp th (k,rho,(x,val):gamma) e2 typ
      return (ALet (x,(val,e1)) e2, cs1++cs2)

    Prod _ x a b -> do
      testErr (typ == vType) "expected Type"
      (a',csa) <- checkType th tenv a
      (b',csb) <- checkType th (k+1, (x,v x):rho, (x,VClos rho a):gamma) b
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

checkInferExp :: Theory -> TCEnv -> Term -> Val -> Err (AExp, [(Val,Val)])
checkInferExp th tenv@(k,_,_) e typ = do
  (e',w,cs1) <- inferExp th tenv e
  cs2 <- eqVal th k w typ
  return (e',cs1 ++ cs2)

inferExp :: Theory -> TCEnv -> Term -> Err (AExp, Val, [(Val,Val)])
inferExp th tenv@(k,rho,gamma) e = case e of
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
                                      val <- eval rho typ
                                      (e1,cs2) <- checkExp th tenv e1 val
                                      return (val,e1,cs1++cs2)
                       Nothing  -> do (e1,val,cs) <- inferExp th tenv e1
                                      return (val,e1,cs)
    (e2,val2,cs2) <- inferExp th (k,rho,(x,val1):gamma) e2
    return (ALet (x,(val1,e1)) e2, val2, cs1++cs2)
   App f t -> do
    (f',w,csf) <- inferExp th tenv f
    typ <- whnf th w
    case typ of
      VClos env (Prod _ x a b) -> do
        (a',csa) <- checkExp th tenv t (VClos env a)
        b' <- whnf th $ VClos ((x,VClos rho t):env) b
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
  val <- eval rho typ
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

checkBranch :: Theory -> TCEnv -> Equation -> Val -> Err (([Term],AExp),[(Val,Val)])
checkBranch th tenv b@(ps,t) ty = errIn ("branch" +++ show b) $
                                  chB tenv' ps' ty
 where

  (ps',_,rho2,k') = ps2ts k ps
  tenv' = (k, rho2++rho, gamma) ---- k' ?
  (k,rho,gamma) = tenv

  chB tenv@(k,rho,gamma) ps ty = case ps of
    p:ps2 -> do
      typ <- whnf th ty
      traceM . render $ "\ncheckBranch: Normalized " <+> vcat [ppValue Unqualified 0 ty , ppValue Unqualified 0 typ]
      case typ of
        VClos env (Prod _ y a b) -> do
          a' <- whnf th $ VClos env a
          (p', sigma, binds, cs1) <- checkP tenv p y a'
          let tenv' = (length binds, sigma ++ rho, binds ++ gamma)
          ((ps',exp),cs2) <- chB tenv' ps2 (VClos ((y,p'):env) b)
          return ((p:ps',exp), cs1 ++ cs2) -- don't change the patt
        _ -> Bad (render ("Product expected for definiens" <+> ppTerm Unqualified 0 t <+> "instead of" <+> ppValue Unqualified 0 typ))
    [] -> do
      (e,cs) <- checkExp th tenv t ty
      return (([],e),cs)
  checkP env@(k,rho,gamma) t x a = do
     (delta,cs) <- checkPatt th env t a
     let sigma = [(x, VGen i x) | ((x,_),i) <- zip delta [k..]]
     return (VClos sigma t, sigma, delta, cs)

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
       typ <- whnf th w
       case typ of
         VClos env (Prod _ x a b) -> do
           (a',_,csa) <- checkExpP tenv t (VClos env a)
           b' <- whnf th $ VClos ((x,VClos rho t):env) b
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
