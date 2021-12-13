{-# LANGUAGE BangPatterns, RankNTypes, FlexibleInstances, MultiParamTypeClasses, PatternGuards #-}
----------------------------------------------------------------------
-- |
-- Maintainer  : Krasimir Angelov
-- Stability   : (stable)
-- Portability : (portable)
--
-- Convert PGF grammar to PMCFG grammar.
--
-----------------------------------------------------------------------------

module GF.Compile.GeneratePMCFG
    (generatePMCFG, type2fields
    ) where

import GF.Grammar hiding (VApp,VRecType)
import GF.Grammar.Predef
import GF.Grammar.Lookup
import GF.Infra.CheckM
import GF.Infra.Option
import GF.Text.Pretty
import GF.Compile.Compute.Concrete
import GF.Data.Operations(Err(..))
import PGF2.Transactions
import qualified Data.Map.Strict as Map
import Control.Monad
import Data.List(mapAccumL,sortBy)
import Data.Maybe(fromMaybe)

generatePMCFG :: Options -> FilePath -> SourceGrammar -> SourceModule -> Check SourceModule
generatePMCFG opts cwd gr cmo@(cm,cmi)
  | isModCnc cmi = do let gr' = prependModule gr cmo
                      js <- mapM (addPMCFG opts cwd gr' cmi) (Map.toList (jments cmi))
                      return (cm,cmi{jments = (Map.fromAscList js)})
  | otherwise    = return cmo

addPMCFG opts cwd gr cmi (id,CncCat mty@(Just (L loc ty)) mdef mref mprn Nothing) = do
  defs <- case mdef of
            Nothing           -> checkInModule cwd cmi loc ("Happened in the PMCFG generation for the lindef of" <+> id) $ do
                                   term <- mkLinDefault gr ty
                                   pmcfgForm gr term [(Explicit,identW,typeStr)] ty
            Just (L loc term) -> checkInModule cwd cmi loc ("Happened in the PMCFG generation for the lindef of" <+> id) $ do
                                   pmcfgForm gr term [(Explicit,identW,typeStr)] ty
  refs <- case mref of
            Nothing           -> checkInModule cwd cmi loc ("Happened in the PMCFG generation for the linref of" <+> id) $ do
                                   term <- mkLinReference gr ty
                                   pmcfgForm gr term [(Explicit,identW,ty)] typeStr
            Just (L loc term) -> checkInModule cwd cmi loc ("Happened in the PMCFG generation for the linref of" <+> id) $ do
                                   pmcfgForm gr term [(Explicit,identW,ty)] typeStr
  mprn  <- case mprn of
             Nothing          -> return Nothing
             Just (L loc prn) -> checkInModule cwd cmi loc ("Happened in the computation of the print name for" <+> id) $ do
                                   prn <- normalForm gr prn
                                   return (Just (L loc prn))
  return (id,CncCat mty mdef mref mprn (Just (defs,refs)))
addPMCFG opts cwd gr cmi (id,CncFun mty@(Just (_,cat,ctxt,val)) mlin@(Just (L loc term)) mprn Nothing) = do
  rules <- checkInModule cwd cmi loc ("Happened in the PMCFG generation for" <+> id) $
             pmcfgForm gr term ctxt val
  mprn  <- case mprn of
             Nothing          -> return Nothing
             Just (L loc prn) -> checkInModule cwd cmi loc ("Happened in the computation of the print name for" <+> id) $ do
                                   prn <- normalForm gr prn
                                   return (Just (L loc prn))
  return (id,CncFun mty mlin mprn (Just rules))
addPMCFG opts cwd gr cmi id_info = return id_info

pmcfgForm :: Grammar -> Term -> Context -> Type -> Check [Production]
pmcfgForm gr t ctxt ty =
  runEvalM gr $ do
    ((_,ms),args) <- mapAccumM (\(d,ms) (_,_,ty) -> do
                                   let (ms',_,t) = type2metaTerm gr d ms 0 [] ty
                                   tnk <- newThunk [] t
                                   return ((d+1,ms'),tnk))
                               (0,Map.empty) ctxt
    sequence_ [newNarrowing i ty | (i,ty) <- Map.toList ms]
    v <- eval [] t args
    (lins,params) <- flatten v ty ([],[])
    lins <- mapM str2lin lins
    (r,rs,_) <- compute params
    args <- zipWithM tnk2lparam args ctxt
    vars <- getVariables
    return (Production vars args (LParam r (order rs)) (reverse lins))
    where
      tnk2lparam tnk (_,_,ty) = do
        v <- force tnk
        (_,params) <- flatten v ty ([],[])
        (r,rs,_) <- compute params
        return (PArg [] (LParam r (order rs)))

      compute []     = return (0,[],1)
      compute (v:vs) = do
        (r, rs ,cnt ) <- param2int v
        (r',rs',cnt') <- compute vs
        return (r*cnt'+r',combine cnt' rs rs',cnt*cnt')

type2metaTerm :: SourceGrammar -> Int -> Map.Map MetaId Type -> LIndex -> [(LIndex,Ident)] -> Type -> (Map.Map MetaId Type,Int,Term)
type2metaTerm gr d ms r rs (Sort s) | s == cStr =
  (ms,r+1,TSymCat d r rs)
type2metaTerm gr d ms r rs (RecType lbls) =
  let ((ms',r'),ass) = mapAccumL (\(ms,r) (lbl,ty) -> case lbl of
                                                        LVar j -> ((ms,r),(lbl,(Just ty,TSymVar d j)))
                                                        lbl    -> let (ms',r',t) = type2metaTerm gr d ms r rs ty
                                                                  in ((ms',r'),(lbl,(Just ty,t))))
                                 (ms,r) lbls
  in (ms',r',R ass)  
type2metaTerm gr d ms r rs (Table p q) =
  let pv = identS ('p':show (length rs))
      (ms',r',t) = type2metaTerm gr d ms r ((r'-r,pv):rs) q
      count = case allParamValues gr p of
                Ok ts   -> length ts
                Bad msg -> error msg
  in (ms',r+(r'-r)*count,T (TTyped p) [(PV pv,t)])
type2metaTerm gr d ms r rs ty@(QC q) =
  let i = Map.size ms + 1
  in (Map.insert i ty ms,r,Meta i)

flatten (VR as) (RecType lbls) st = do
  foldM collect st lbls
  where
    collect st (lbl,ty) =
      case lookup lbl as of
        Just tnk -> do v <- force tnk
                       flatten v ty st
        Nothing  -> evalError ("Missing value for label" <+> pp lbl $$
                               "among" <+> hsep (punctuate (pp ',') (map fst as)))
flatten v@(VT _ env cs) (Table p q) st = do
  ts <- getAllParamValues p
  foldM collect st ts
  where
    collect st t = do
      tnk <- newThunk [] t
      let v0 = VS v tnk []
      v <- patternMatch v0 (map (\(p,t) -> (env,[p],[tnk],t)) cs)
      flatten v q st
flatten (VV _ tnks) (Table _ q) st = do
  foldM collect st tnks
  where
    collect st tnk = do
      v <- force tnk
      flatten v q st
flatten v (Sort s) (lins,params) | s == cStr = do
  return (v:lins,params)
flatten v (QC q) (lins,params) = do
  return (lins,v:params)
flatten v _ _ = do
  error (showValue v)

str2lin (VApp q [])
  | q == (cPredef, cBIND)       = return [SymBIND]
  | q == (cPredef, cNonExist)   = return [SymNE]
  | q == (cPredef, cSOFT_BIND)  = return [SymSOFT_BIND]
  | q == (cPredef, cSOFT_SPACE) = return [SymSOFT_SPACE]
  | q == (cPredef, cCAPIT)      = return [SymCAPIT]
  | q == (cPredef, cALL_CAPIT)  = return [SymALL_CAPIT]
str2lin (VStr s)         = return [SymKS s]
str2lin (VSymCat d r rs) = do (r, rs) <- compute r rs
                              return [SymCat d (LParam r (order rs))]
  where
    compute r' []                = return (r',[])
    compute r' ((cnt',tnk):tnks) = do
      (r, rs,_) <- force tnk >>= param2int
      (r',rs' ) <- compute r' tnks
      return (r*cnt'+r',combine cnt' rs rs')
str2lin (VSymVar d r)    = return [SymVar d r]
str2lin (VC vs)          = fmap concat (mapM str2lin vs)
str2lin (VAlts def alts) = do def <- str2lin def
                              alts <- forM alts $ \(v,VStrs vs) -> do
                                lin <- str2lin v
                                return (lin,[s | VStr s <- vs])
                              return [SymKP def alts]
str2lin v                = do t <- value2term 0 v
                              evalError ("the string:" <+> ppTerm Unqualified 0 t $$
                                         "cannot be evaluated at compile time.")

param2int (VR as) = compute as
  where
    compute []             = return (0,[],1)
    compute ((lbl,tnk):as) = do
      (r, rs ,cnt ) <- force tnk >>= param2int
      (r',rs',cnt') <- compute as
      return (r*cnt'+r',combine cnt' rs rs',cnt*cnt')
param2int (VApp q tnks) = do
  (r ,    cnt ) <- getIdxCnt q
  (r',rs',cnt') <- compute tnks
  return (r+r',rs',cnt*cnt')
  where
    getIdxCnt q = do
      (_,ResValue (L _ ty) idx) <- getInfo q
      let QC p = valTypeCnc ty
      (_,ResParam _ (Just (_,cnt))) <- getInfo p
      return (idx,cnt)

    compute []         = return (0,[],1)
    compute (tnk:tnks) = do
      (r, rs ,cnt ) <- force tnk >>= param2int
      (r',rs',cnt') <- compute tnks
      return (r*cnt'+r',combine cnt' rs rs',cnt*cnt')
param2int (VMeta tnk _ _) = do
  tnk_st <- getRef tnk
  case tnk_st of
    Evaluated v    -> param2int v
    Narrowing j ty -> do let QC q = valTypeCnc ty
                         (_,ResParam _ (Just (_,cnt))) <- getInfo q
                         return (0,[(1,j-1)],cnt)
param2int v               = do t <- value2term 0 v
                               evalError ("the parameter:" <+> ppTerm Unqualified 0 t $$
                                          "cannot be evaluated at compile time.")

combine cnt'          []            rs' = rs'
combine cnt'         rs              [] = [(r*cnt',pv) | (r,pv) <- rs]
combine cnt' ((r,pv):rs) ((r',pv'):rs') =
  case compare pv pv' of
    LT -> (r*cnt',   pv ) : combine cnt' rs ((r',pv'):rs')
    EQ -> (r*cnt'+r',pv ) : combine cnt' rs ((r',pv'):rs')
    GT -> (       r',pv') : combine cnt' ((r,pv):rs) rs'

order = sortBy (\(r1,_) (r2,_) -> compare r2 r1)

mapAccumM f a []     = return (a,[])
mapAccumM f a (x:xs) = do (a, y) <- f a x
                          (a,ys) <- mapAccumM f a xs
                          return (a,y:ys)

type2fields :: SourceGrammar -> Type -> [String]
type2fields gr = type2fields empty
  where
    type2fields d (Sort s) | s == cStr = [show d]
    type2fields d (RecType lbls) =
      concatMap (\(lbl,ty) -> type2fields (d <+> pp lbl) ty) lbls
    type2fields d (Table p q) =
      let Ok ts = allParamValues gr p
      in concatMap (\t -> type2fields (d <+> ppTerm Unqualified 5 t) q) ts
    type2fields d _ = []

mkLinDefault :: SourceGrammar -> Type -> Check Term
mkLinDefault gr typ = liftM (Abs Explicit varStr) $ mkDefField typ
 where
   mkDefField ty =
     case ty of
       Table p t  -> do t' <- mkDefField t
                        let T _ cs = mkWildCases t'
                        return $ T (TWild p) cs
       Sort s | s == cStr -> return (Vr varStr)
       QC p       -> case lookupParamValues gr p of
                       Ok []    -> checkError ("no parameter values given to type" <+> ppQIdent Qualified p)
                       Ok (v:_) -> return v
                       Bad msg  -> fail msg
       RecType r -> do
         let (ls,ts) = unzip r
         ts <- mapM mkDefField ts
         return $ R (zipWith assign ls ts)
       _ | Just _ <- isTypeInts typ -> return $ EInt 0 -- exists in all as first val
       _ -> checkError ("linearization type field cannot be" <+> ty)

mkLinReference :: SourceGrammar -> Type -> Check Term
mkLinReference gr typ = do
  mb_term <- mkRefField typ (Vr varStr)
  return (Abs Explicit varStr (fromMaybe Empty mb_term))
  where
    mkRefField ty trm =
      case ty of
        Table pty ty -> case allParamValues gr pty of
                          Ok []     -> checkError ("no parameter values given to type" <+> pty)
                          Ok (p:ps) -> mkRefField ty (S trm p)
                          Bad msg   -> fail msg
        Sort s | s == cStr -> return (Just trm)
        QC p       -> return Nothing
        RecType [] -> return Nothing
        RecType rs -> traverse rs trm
        _ | Just _ <- isTypeInts typ -> return Nothing
        _ -> checkError ("linearization type field cannot be" <+> typ)

    traverse []          trm = return Nothing
    traverse ((l,ty):rs) trm = do res <- mkRefField ty (P trm l)
                                  case res of
                                    Just trm -> return (Just trm)
                                    Nothing  -> traverse rs trm
