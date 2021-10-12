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
import Debug.Trace

generatePMCFG :: Options -> FilePath -> SourceGrammar -> SourceModule -> Check SourceModule
generatePMCFG opts cwd gr cmo@(cm,cmi) = do
  let gr' = prependModule gr cmo
  js <- mapM (addPMCFG opts cwd gr' cmi) (Map.toList (jments cmi))
  return (cm,cmi{jments = (Map.fromAscList js)})

addPMCFG opts cwd gr cmi (id,CncFun mty@(Just (cat,ctxt,val)) mlin@(Just (L loc term)) mprn Nothing) =
  checkInModule cwd cmi loc ("Happened in the PMCFG generation for" <+> id) $ do
    lins <- pmcfgForm gr (L loc id) term ctxt
    return (id,CncFun mty mlin mprn (Just (PMCFG lins)))
addPMCFG opts cwd gr cmi id_info = return id_info

pmcfgForm :: Grammar -> L Ident -> Term -> Context -> Check [[[Symbol]]]
pmcfgForm gr _ t ctxt =
  runEvalM gr $ do
    ((_,ms),args) <- mapAccumM (\(d,ms) (_,_,ty) -> do
                                   let (ms',_,t) = type2metaTerm gr d ms 0 [] ty
                                   tnk <- trace (show (ppTerm Unqualified 0 t)) $ newThunk [] t
                                   return ((d+1,ms'),tnk))
                               (0,Map.empty) ctxt
    sequence_ [newMeta (Just ty) i | (i,ty) <- Map.toList ms]
    v <- eval [] t args
    (lins,_) <- value2pmcfg v []
    return (reverse lins)

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

value2pmcfg (VSusp tnk env vs k) lins = do
  st <- getMeta tnk
  case st of
    Unevaluated _ t         -> do v <- eval env t vs
                                  value2pmcfg v lins
    Evaluated v             -> do v <- apply v vs
                                  value2pmcfg v lins
    Unbound (Just (QC q)) _ -> do (m,ps) <- lookupParams q
                                  msum [bind tnk m p | p <- ps]
                                  v <- k tnk
                                  value2pmcfg v lins
  where
    bind tnk m (p, ctxt) = do
      tnks <- mapM (\(_,_,ty) -> newMeta (Just ty) 0) ctxt
      setMeta tnk (Evaluated (VApp (m,p) tnks))
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

value2lin (VStr s)         = return [SymKS s]
value2lin (VSymCat d r rs) = do rs <- forM rs $ \(i,tnk) -> do
                                        v <- force tnk []
                                        j <- case v of
                                               VMeta tnk _ _ -> do st <- getMeta tnk
                                                                   case st of
                                                                     Unbound _ j -> return j
                                        return (i,j)
                                return [SymCat d r rs]
value2lin (VC vs)          = fmap concat (mapM value2lin vs)
value2lin v                = do t <- value2term 0 v
                                evalError ("the term" <+> ppTerm Unqualified 0 t $$
                                           "cannot be evaluated at compile time.")

mapAccumM f a []     = return (a,[])
mapAccumM f a (x:xs) = do (a, y) <- f a x
                          (a,ys) <- mapAccumM f a xs
                          return (a,y:ys)

pgfCncCat = error "TODO: pgfCncCat"
