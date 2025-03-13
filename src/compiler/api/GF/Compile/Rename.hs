----------------------------------------------------------------------
-- |
-- Module      : Rename
-- Maintainer  : AR
-- Stability   : (stable)
-- Portability : (portable)
--
-- > CVS $Date: 2005/05/30 18:39:44 $
-- > CVS $Author: aarne $
-- > CVS $Revision: 1.19 $
--
-- AR 14\/5\/2003
-- The top-level function 'renameGrammar' does several things:
--
--   - extends each module symbol table by indirections to extended module
--
--   - changes unqualified and as-qualified imports to absolutely qualified
--
--   - goes through the definitions and resolves names
--
-- Dependency analysis between modules has been performed before this pass.
-- Hence we can proceed by @fold@ing "from left to right".
-----------------------------------------------------------------------------

module GF.Compile.Rename (
     renameSourceTerm,
     renameModule
    ) where

import GF.Infra.Ident
import GF.Infra.CheckM
import GF.Grammar.Grammar
import GF.Grammar.Values
import GF.Grammar.Predef
import GF.Grammar.Lookup
import GF.Grammar.Macros
import GF.Grammar.Printer
import GF.Data.Operations
import PGF2(abstractName,functionType,categoryContext)

import Control.Monad
import Data.List (nub,(\\))
import qualified Data.List as L
import qualified Data.Map as Map
import Data.Maybe(mapMaybe)
import GF.Text.Pretty

-- | this gives top-level access to renaming term input in the cc command
renameSourceTerm :: Grammar -> ModuleName -> Term -> Check Term
renameSourceTerm g m t = do
  mi     <- lookupModule g m
  status <- buildStatus "" g (m,mi)
  renameTerm status [] t

renameModule :: FilePath -> Grammar -> Module -> Check Module
renameModule cwd gr mo@(m,mi) = do
  status <- buildStatus cwd gr mo
  js     <- checkMapRecover (renameInfo cwd status mo) (jments mi)
  return (m, mi{jments = js})

type Status = (StatusMap, [(OpenSpec, StatusMap)])
type StatusMap = Ident -> Maybe Term

-- Delays errors, allowing many errors to be detected and reported
renameIdentTerm env = accumulateError (renameIdentTerm' env)

-- Fails immediately on error, makes it possible to try other possibilities
renameIdentTerm' :: Status -> Term -> Check Term
renameIdentTerm' env@(act,imps) t0 =
  case t0 of
    Vr c -> ident predefAbs c
    Cn c -> ident (\_ s -> checkError s) c
    Q (m',c) | m' == cPredef {- && isInPredefined c -} -> return t0
    Q (m',c) -> do
      f <- lookupErr m' qualifs
      maybe (notFound c) return (f c)
    QC (m',c) | m' == cPredef {- && isInPredefined c -} -> return t0
    QC (m',c) -> do
      f <- lookupErr m' qualifs
      maybe (notFound c) return (f c)
    _ -> return t0
  where
    opens   = [st  | (OSimple _,st) <- imps]
    qualifs = [(m, st) | (OQualif m _, st) <- imps] ++
              [(m, st) | (OQualif _ m, st) <- imps] ++
              [(m, st) | (OSimple m, st) <- imps] -- qualif is always possible

    -- this facility is mainly for BWC with GF1: you need not import PredefAbs
    predefAbs c s
      | isPredefCat c = return (Q (cPredefAbs,c))
      | otherwise     = checkError s

    ident alt c =
      case act c of
        Just t -> return t
        _      -> case mapMaybe (\f -> f c) opens of
                    [t]  -> return t
                    []   -> alt c ("constant not found:" <+> c $$
                                   "given" <+> fsep (punctuate ',' (map fst qualifs)))
                    ts   -> case nub ts  of
                              [t]      -> return t
                              ts@(t:_) -> do checkWarn ("atomic term" <+> ppTerm Qualified 0 t0 $$
                                                        "conflict" <+> hsep (punctuate ',' (map (ppTerm Qualified 0) ts)) $$
                                                        "given" <+> fsep (punctuate ',' (map fst qualifs)))
                                             return t

info2status :: Maybe ModuleName -> Ident -> Info -> Term
info2status mq c i = case i of
  AbsFun _ _ Nothing _ -> maybe Con (curry QC) mq c
  ResValue _ _ -> maybe Con (curry QC) mq c
  ResParam _ _ -> maybe Con (curry QC) mq c
  AnyInd True m -> maybe Con (const (curry QC m)) mq c
  AnyInd False m -> maybe Cn (const (curry Q m)) mq c
  _           -> maybe Cn (curry Q) mq c

tree2status :: OpenSpec -> Map.Map Ident Info -> StatusMap
tree2status o map = case o of
  OSimple i   -> flip Map.lookup (Map.mapWithKey (info2status (Just i)) map)
  OQualif i j -> flip Map.lookup (Map.mapWithKey (info2status (Just j)) map)

buildStatus :: FilePath -> Grammar -> Module -> Check Status
buildStatus cwd gr mo@(m,mi) = checkInModule cwd mi NoLoc empty $ do
  let gr1  = prependModule gr mo
      exts = [(o,modInfo2status o mi) | (m,mi) <- allExtends gr1 m, let o = OSimple m]
  ops <- mapM (openSpec2status gr1) (mopens mi)
  let sts = exts++ops
  return (if isModCnc mi
            then (const Nothing,   reverse sts)  -- the module itself does not define any names
            else (self2status m mi,reverse sts))

openSpec2status gr o =
  do mi <- lookupModule gr (openedModule o)
     return (o,modInfo2status o mi)
  where
    mn = openedModule o

pgf2status o pgf id =
  case functionType pgf sid of
    Just _  -> Just (QC (mn, id))
    Nothing -> case categoryContext pgf sid of
                 Just _  -> Just (QC (mn, id))
                 Nothing -> Nothing
  where
    sid = showIdent id

    mn = case o of
           OSimple i   -> i
           OQualif i j -> j

modInfo2status :: OpenSpec -> ModuleInfo -> StatusMap
modInfo2status o (ModInfo{jments=jments}) = tree2status o jments
modInfo2status o (ModPGF pgf) = pgf2status o pgf

self2status :: ModuleName -> ModuleInfo -> StatusMap
self2status c m = flip Map.lookup (Map.mapWithKey (info2status (Just c)) (jments m))


renameInfo :: FilePath -> Status -> Module -> Ident -> Info -> Check Info
renameInfo cwd status (m,mi) i info =
  case info of
    AbsCat pco -> liftM AbsCat (renPerh (renameContext status) pco)
    AbsFun  pty pa ptr poper -> liftM4 AbsFun (renTerm pty) (return pa) (renMaybe (mapM (renLoc (renEquation status))) ptr) (return poper)
    ResOper pty ptr -> liftM2 ResOper (renTerm pty) (renTerm ptr)
    ResOverload os tysts -> liftM (ResOverload os) (mapM (renPair (renameTerm status [])) tysts)
    ResParam (Just pp) m -> do
      pp' <- renLoc (mapM (renParam status)) pp
      return (ResParam (Just pp') m)
    ResValue t i -> do
      t <- renLoc (renameTerm status []) t
      return (ResValue t i)
    CncCat mcat mdef mref mpr mpmcfg -> liftM5 CncCat (renTerm mcat) (renTerm mdef) (renTerm mref) (renTerm mpr) (return mpmcfg)
    CncFun mty mtr mpr mpmcfg -> liftM3 (CncFun mty)         (renTerm mtr) (renTerm mpr) (return mpmcfg)
    _ -> return info
  where
    renTerm = renPerh (renameTerm status [])

    renPerh ren = renMaybe (renLoc ren)

    renMaybe ren (Just x) = ren x >>= return . Just
    renMaybe ren Nothing  = return Nothing

    renLoc ren (L loc x) =
      checkInModule cwd mi loc ("Happened in the renaming of" <+> i) $ do
        x <- ren x
        return (L loc x)

    renPair ren (x, y) = do x <- renLoc ren x
                            y <- renLoc ren y
                            return (x, y)

    renEquation :: Status -> Equation -> Check Equation
    renEquation b (ps,t) = do
        (ps',vs) <- liftM unzip $ mapM (renamePattern b) ps
        t'       <- renameTerm b (concat vs) t
        return (ps',t')

    renParam :: Status -> Param -> Check Param
    renParam env (c,co) = do
      co' <- renameContext env co
      return (c,co')

renameTerm :: Status -> [Ident] -> Term -> Check Term
renameTerm env vars = ren vars where
  ren vs trm = case trm of
    Abs b x t    -> liftM  (Abs b x) (ren (x:vs) t)
    Prod bt x a b -> liftM2 (Prod bt x) (ren vs a) (ren (x:vs) b)
    Typed a b  -> liftM2 Typed (ren vs a) (ren vs b)
    Vr x
      | elem x vs -> return trm
      | otherwise -> renid trm
    Cn _   -> renid trm
    Con _  -> renid trm
    Q _    -> renid trm
    QC _   -> renid trm
    T i cs -> do
      i' <- case i of
        TTyped ty -> liftM TTyped $ ren vs ty -- the only annotation in source
        _ -> return i
      liftM (T i') $ mapM (renCase vs) cs

    Let (x,(m,a)) b -> do
      m' <- case m of
        Just ty -> liftM Just $ ren vs ty
        _ -> return m
      a' <- ren vs a
      b' <- ren (x:vs) b
      return $ Let (x,(m',a')) b'

    P t@(Vr r) l                                               -- Here we have $r.l$ and this is ambiguous it could be either
                                                               -- record projection from variable or constant $r$ or qualified expression with module $r$
      | elem r vs -> return trm                                -- try var proj first ..
      | otherwise -> checks [ renid' (Q (MN r,label2ident l))      -- .. and qualified expression second.
                            , renid' t >>= \t -> return (P t l) -- try as a constant at the end
                            , checkError ("unknown qualified constant" <+> trm)
                            ]

    EPatt minp maxp p -> do
      (p',_) <- renpatt p
      return $ EPatt minp maxp p'

    Reset ctl t -> do
      ctl <- case ctl of
               Coordination _ conj cat ->
                                 checks [ do t <- renid' (Cn conj)
                                             case t of
                                               Q  (mn,id) -> return (Coordination (Just mn) conj cat)
                                               QC (mn,id) -> return (Coordination (Just mn) conj cat)
                                               _          -> return (Coordination Nothing   conj cat)
                                        , if showIdent conj == "one"
                                            then return One
                                            else checkError ("Undefined control" <+> pp conj)
                                        ]
               ctl                     -> do return ctl
      t <- ren vs t
      return (Reset ctl t)

    _ -> composOp (ren vs) trm

  renid = renameIdentTerm env
  renid' = renameIdentTerm' env
  renCase vs (p,t) = do
    (p',vs') <- renpatt p
    t' <- ren (vs' ++ vs) t
    return (p',t')
  renpatt = renamePattern env

-- | vars not needed in env, since patterns always overshadow old vars
renamePattern :: Status -> Patt -> Check (Patt,[Ident])
renamePattern env patt =
    do r@(p',vs) <- renp patt
       let dupl = vs \\ nub vs
       unless (null dupl) $ checkError (hang ("[C.4.13] Pattern is not linear. All variable names on the left-hand side must be distinct.") 4
                                             patt)
       return r
  where
    renp patt = case patt of
      PMacro c -> do
        c' <- renid $ Vr c
        case c' of
          Q d -> renp $ PM d
          _ -> checkError ("unresolved pattern" <+> patt)

      PC c ps -> do
        c' <- renid $ Cn c
        case c' of
          QC c -> do psvss <- mapM renp ps
                     let (ps,vs) = unzip psvss
                     return (PP c ps, concat vs)
          Q  _ -> checkError ("data constructor expected but" <+> ppTerm Qualified 0 c' <+> "is found instead")
          _    -> checkError ("unresolved data constructor" <+> ppTerm Qualified 0 c')

      PP c ps -> do
        (QC c') <- renid (QC c)
        psvss <- mapM renp ps
        let (ps',vs) = unzip psvss
        return (PP c' ps', concat vs)

      PM c -> do
        x <- renid (Q c)
        c' <- case x of
                (Q c') -> return c'
                _      -> checkError ("not a pattern macro" <+> ppPatt Qualified 0 patt)
        return (PM c', [])

      PV x -> checks [ renid' (Vr x) >>= \t' -> case t' of
                                                 QC c -> return (PP c [],[])
                                                 _    -> checkError (pp "not a constructor")
                     , return (patt, [x])
                     ]

      PR r -> do
        let (ls,ps) = unzip r
        psvss <- mapM renp ps
        let (ps',vs') = unzip psvss
        return (PR (zip ls ps'), concat vs')

      PAlt p q -> do
        (p',vs) <- renp p
        (q',ws) <- renp q
        return (PAlt p' q', vs ++ ws)

      PSeq minp maxp p minq maxq q -> do
        (p',vs) <- renp p
        (q',ws) <- renp q
        return (PSeq minp maxp p' minq maxq q', vs ++ ws)

      PRep minp maxp p -> do
        (p',vs) <- renp p
        return (PRep minp maxp p', vs)

      PNeg p -> do
        (p',vs) <- renp p
        return (PNeg p', vs)

      PAs x p -> do
        (p',vs) <- renp p
        return (PAs x p', x:vs)

      _ -> return (patt,[])

    renid = renameIdentTerm env
    renid' = renameIdentTerm' env

renameContext :: Status -> Context -> Check Context
renameContext b = renc [] where
  renc vs cont = case cont of
    (bt,x,t) : xts
      | x == identW -> do
          t'   <- ren vs t
          xts' <- renc vs xts
          return $ (bt,x,t') : xts'
      | otherwise -> do
          t'   <- ren vs t
          let vs' = x:vs
          xts' <- renc vs' xts
          return $ (bt,x,t') : xts'
    _ -> return cont
  ren = renameTerm b
