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

import GF.Grammar
import GF.Grammar.Predef
import GF.Infra.CheckM
import GF.Infra.Option
import GF.Compile.Compute.Concrete
import PGF2.Transactions
import qualified Data.Map.Strict as Map

generatePMCFG :: Options -> SourceGrammar -> SourceModule -> Check SourceModule
generatePMCFG opts gr cmo@(cm,cmi) = do
  let gr' = prependModule gr cmo
  js <- mapM (addPMCFG opts gr') (Map.toList (jments cmi))
  return (cm,cmi{jments = (Map.fromAscList js)})

addPMCFG opts gr (id,CncFun mty@(Just (cat,ctxt,val)) mlin@(Just (L loc term)) mprn Nothing) = do
  lins <- pmcfgForm gr (L loc id) term ctxt
  return (id,CncFun mty mlin mprn (Just (PMCFG lins)))
addPMCFG opts gr id_info = return id_info

pmcfgForm :: Grammar -> L Ident -> Term -> Context -> Check [[[Symbol]]]
pmcfgForm gr _ t ctxt =
  runEvalM gr $ do
    (_,args) <- mapAccumM (\(d,r) (_,_,ty) -> do (r,v) <- type2metaValue d r ty
                                                 return ((d+1,r),v))
                          (0,0) ctxt
    v <- eval [] t args
    (lins,_) <- value2pmcfg v []
    return (reverse lins)

type2metaValue :: Int -> Int -> Type -> EvalM s (Int,Thunk s)
type2metaValue d r (Sort s) | s == cStr = do
  tnk <- newEvaluatedThunk (VSymCat d r)
  return (r+1,tnk)
type2metaValue d r (RecType lbls) = do
  (r,lbls) <- mapAccumM (\i (lbl,ty) -> do (i,tnk) <- type2metaValue d i ty
                                           return (i,(lbl,tnk)))
                        r lbls
  tnk <- newEvaluatedThunk (VR lbls)
  return (r,tnk)
type2metaValue d r (Table p q) = do
  ts <- getAllParamValues p
  (r,vs) <- mapAccumM (\r _ -> type2metaValue d r q) r ts
  tnk <- newEvaluatedThunk (VV p vs)
  return (r, tnk)
type2metaValue d r (QC q) = do
  tnk <- newMeta 0
  return (r, tnk)

value2pmcfg (VR as) lins = do
  (lins,as) <- collectFields lins as
  return (lins,VR as)
  where
    collectFields lins []             = do
      return (lins,[])
    collectFields lins ((lbl,tnk):as) = do
      v <- force tnk []
      (lins,v) <- value2pmcfg v lins
      case v of
        VR [] -> collectFields lins as
        _     -> do (lins,as) <- collectFields lins as
                    tnk <- newEvaluatedThunk v
                    return (lins,(lbl,tnk):as)
value2pmcfg v lins = do
  lin <- value2lin v
  return (lin:lins,VR [])

value2lin (VStr s) =
  return [SymKS s]
value2lin (VC vs) =
  fmap concat (mapM value2lin vs)
value2lin (VSymCat d r) =
  return [SymCat d r]


mapAccumM f a []     = return (a,[])
mapAccumM f a (x:xs) = do (a, y) <- f a x
                          (a,ys) <- mapAccumM f a xs
                          return (a,y:ys)

pgfCncCat = error "TODO: pgfCncCat"
