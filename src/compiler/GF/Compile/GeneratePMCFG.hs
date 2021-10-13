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
    (generatePMCFG, pgfCncCat, addPMCFG
    ) where

import GF.Grammar hiding (VApp)
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
import Data.List(mapAccumL)

generatePMCFG :: Options -> FilePath -> SourceGrammar -> SourceModule -> Check SourceModule
generatePMCFG opts cwd gr cmo@(cm,cmi) = do
  let gr' = prependModule gr cmo
  js <- mapM (addPMCFG opts cwd gr' cmi) (Map.toList (jments cmi))
  return (cm,cmi{jments = (Map.fromAscList js)})

addPMCFG opts cwd gr cmi (id,CncFun mty@(Just (_,cat,ctxt,val)) mlin@(Just (L loc term)) mprn Nothing) =
  checkInModule cwd cmi loc ("Happened in the PMCFG generation for" <+> id) $ do
    rules <- pmcfgForm gr term ctxt val
    return (id,CncFun mty mlin mprn (Just rules))
addPMCFG opts cwd gr cmi id_info = return id_info

pmcfgForm :: Grammar -> Term -> Context -> Type -> Check [PMCFGRule]
pmcfgForm gr t ctxt ty =
  runEvalM gr $ do
    ((_,ms),args) <- mapAccumM (\(d,ms) (_,_,ty) -> do
                                   let (ms',_,t) = type2metaTerm gr d ms 0 [] ty
                                   tnk <- newThunk [] t
                                   return ((d+1,ms'),tnk))
                               (0,Map.empty) ctxt
    sequence_ [newMeta (Just ty) i | (i,ty) <- Map.toList ms]
    v <- eval [] t args
    (lins,params) <- flatten v ty ([],[])
    lins <- mapM str2lin lins
    (r,rs,_) <- compute params
    args <- zipWithM tnk2pmcfgcat args ctxt
    return (PMCFGRule (PMCFGCat r rs) args (reverse lins))
    where
      tnk2pmcfgcat tnk (_,_,ty) = do
        v <- force tnk []
        (_,params) <- flatten v ty ([],[])
        (r,rs,_) <- compute params
        return (PMCFGCat r rs)

      compute []     = return (0,[],1)
      compute (v:vs) = do
        (r, rs ,cnt ) <- param2int v
        (r',rs',cnt') <- compute vs
        return (r*cnt'+r',combine cnt' rs rs',cnt*cnt')

type2metaTerm :: SourceGrammar -> Int -> Map.Map MetaId Type -> LIndex -> [(LIndex,Ident)] -> Type -> (Map.Map MetaId Type,Int,Term)
type2metaTerm gr d ms r rs (Sort s) | s == cStr =
  (ms,r+1,TSymCat d r rs)
type2metaTerm gr d ms r rs (RecType lbls) =
  let ((ms',r'),ass) = mapAccumL (\(ms,r) (lbl,ty) -> let (ms',r',t) = type2metaTerm gr d ms r rs ty
                                                      in ((ms',r'),(lbl,(Just ty,t))))
                                 (ms,r) lbls
  in (ms',r',R ass)  
type2metaTerm gr d ms r rs (Table p q) =
  let pv = identS ('p':show (length rs))
      (ms',r',t) = type2metaTerm gr d ms r ((r'-r,pv):rs) q
      count = case allParamValues gr p of
                Ok ts   -> length ts
                Bad msg -> error msg
  in (ms',(r'-r)*count,T (TTyped p) [(PV pv,t)])
type2metaTerm gr d ms r rs ty@(QC q) =
  let i = Map.size ms + 1
  in (Map.insert i ty ms,r,Meta i)


flatten (VSusp tnk env vs k) ty st = do
  tnk_st <- getMeta tnk
  case tnk_st of
    Evaluated v             -> do v <- apply v vs
                                  flatten v ty st
    Unbound (Just (QC q)) _ -> do (m,ResParam (Just (L _ ps)) _) <- getInfo q
                                  msum [bind tnk m p | p <- ps]
                                  v <- k tnk
                                  flatten v ty st
  where
    bind tnk m (p, ctxt) = do
      tnks <- mapM (\(_,_,ty) -> newMeta (Just ty) 0) ctxt
      setMeta tnk (Evaluated (VApp (m,p) tnks))
flatten (VR as) (RecType lbls) st = do
  foldM collect st lbls
  where
    collect st (lbl,ty) =
      case lookup lbl as of
        Just tnk -> do v <- force tnk []
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
      v <- force tnk []
      flatten v q st
flatten v (Sort s) (lins,params) | s == cStr = do
  return (v:lins,params)
flatten v (QC q) (lins,params) = do
  return (lins,v:params)

str2lin (VStr s)         = return [SymKS s]
str2lin (VSymCat d r rs) = do (r, rs) <- compute r rs
                              return [SymCat d r rs]
  where
    compute r' []                = return (r',[])
    compute r' ((cnt',tnk):tnks) = do
      (r, rs,_) <- force tnk [] >>= param2int
      (r',rs' ) <- compute r' tnks
      return (r*cnt'+r',combine cnt' rs rs')
str2lin (VC vs)          = fmap concat (mapM str2lin vs)
str2lin v                = do t <- value2term 0 v
                              evalError ("the term" <+> ppTerm Unqualified 0 t $$
                                         "cannot be evaluated at compile time.")

param2int (VApp q tnks) = do
  (r ,    cnt ) <- getIdxCnt q
  (r',rs',cnt') <- compute tnks
  return (r*cnt' + r',rs',cnt*cnt')
  where
    getIdxCnt q = do
      (_,ResValue (L _ ty) idx) <- getInfo q
      let QC p = valTypeCnc ty
      (_,ResParam _ (Just (_,cnt))) <- getInfo p
      return (idx,cnt)

    compute []         = return (0,[],1)
    compute (tnk:tnks) = do
      (r, rs ,cnt ) <- force tnk [] >>= param2int
      (r',rs',cnt') <- compute tnks
      return (r*cnt'+r',combine cnt' rs rs',cnt*cnt')
param2int (VMeta tnk _ _) = do
  tnk_st <- getMeta tnk
  case tnk_st of
    Evaluated v         -> param2int v
    Unbound (Just ty) j -> do let QC q = valTypeCnc ty
                              (_,ResParam _ (Just (_,cnt))) <- getInfo q
                              return (0,[(1,j)],cnt)

combine cnt'          []            rs' = rs'
combine cnt'         rs              [] = [(r*cnt',pv) | (r,pv) <- rs]
combine cnt' ((r,pv):rs) ((r',pv'):rs') =
  case compare pv pv' of
    LT -> (r*cnt',   pv ) : combine cnt' rs ((r',pv'):rs')
    EQ -> (r*cnt'+r',pv ) : combine cnt' rs ((r',pv'):rs')
    GT -> (       r',pv') : combine cnt' ((r,pv):rs) rs'

mapAccumM f a []     = return (a,[])
mapAccumM f a (x:xs) = do (a, y) <- f a x
                          (a,ys) <- mapAccumM f a xs
                          return (a,y:ys)

pgfCncCat = error "TODO: pgfCncCat"
