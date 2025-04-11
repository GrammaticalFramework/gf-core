-- | Translate grammars to Canonical form
-- (a common intermediate representation to simplify export to other formats)
module GF.Compile.GrammarToCanonical(
       grammar2canonical
       ) where

import GF.Data.ErrM
import GF.Grammar
import GF.Grammar.Lookup(allOrigInfos,lookupOrigInfo)
import GF.Infra.Option(Options,noOptions)
import GF.Infra.CheckM
import GF.Compile.Compute.Concrete2
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe(mapMaybe,fromMaybe)
import Control.Monad (forM)

-- | Generate Canonical code for the named abstract syntax and all associated
-- concrete syntaxes
grammar2canonical :: Options -> ModuleName -> Grammar -> Check Grammar
grammar2canonical opts absname gr = do
  abs <- abstract2canonical absname gr
  cncs <- concretes2canonical opts absname gr
  return (mGrammar (abs:cncs))

-- | Generate Canonical code for the named abstract syntax
abstract2canonical :: ModuleName -> Grammar -> Check Module
abstract2canonical absname gr = do
  let infos = [(id,info) | ((mn,id),info) <- allOrigInfos gr absname]
  return (absname, ModInfo {
                     mtype   = MTAbstract,
                     mstatus = MSComplete,
                     mflags  = convFlags gr absname,
                     mextend = [],
                     mwith   = Nothing,
                     mopens  = [],
                     mexdeps = [],
                     msrc    = "",
                     mseqs   = Nothing,
                     jments  = Map.fromList infos
                   })

-- | Generate Canonical code for the all concrete syntaxes associated with
-- the named abstract syntax in given the grammar.
concretes2canonical :: Options -> ModuleName -> Grammar -> Check [Module]
concretes2canonical opts absname gr = do
  res <- sequence
           [concrete2canonical gr absname cnc modinfo
               | cnc<-allConcretes gr absname,
                 let Ok modinfo = lookupModule gr cnc]
  let pts = Set.unions (map fst res)
  ms <- closure pts (Set.toList pts) (Map.fromList (map snd res))
  return (Map.toList ms)
  where
    closure pts []            ms = return ms
    closure pts (q@(m,id):qs) ms = do
      (_,info@(ResParam (Just (L _ ps)) _)) <- lookupOrigInfo gr q
      let pts'    = Set.unions [paramTypes ty | (_,ctx) <- ps, (_,_,ty) <- ctx]
          new_pts = Set.difference pts' pts
      closure (Set.union new_pts pts) (Set.toList new_pts++qs) (insert q info ms)

    insert (m,id) info ms =
      let mi0 = fromMaybe emptyRes (Map.lookup m ms)
          mi  = mi0{jments=Map.insert id info (jments mi0)}
      in Map.insert m mi ms

    emptyRes =
      ModInfo {
        mtype   = MTResource,
        mstatus = MSComplete,
        mflags  = noOptions,
        mextend = [],
        mwith   = Nothing,
        mopens  = [],
        mexdeps = [],
        msrc    = "",
        mseqs   = Nothing,
        jments  = Map.empty
      }

type QSet = Set.Set (ModuleName,Ident)

-- | Generate Canonical GF for the given concrete module.
concrete2canonical :: Grammar -> ModuleName -> ModuleName -> ModuleInfo -> Check (QSet,Module)
concrete2canonical gr absname cncname modinfo = do
  let g = Gl gr (stdPredef g)
  infos <- mapM (convInfo g) (allOrigInfos gr cncname)
  let pts = Set.unions (map fst infos)
  return (pts,
          (cncname, ModInfo {
                      mtype   = MTConcrete absname,
                      mstatus = MSComplete,
                      mflags  = convFlags gr cncname,
                      mextend = [],
                      mwith   = Nothing,
                      mopens  = [],
                      mexdeps = [],
                      msrc    = "",
                      mseqs   = Nothing,
                      jments  = Map.fromList (mapMaybe snd infos)
                    }))
  where
    convInfo g ((mn,id), CncCat (Just (L loc typ)) lindef linref pprn mb_prods) = do
      typ <- normalForm g typ
      let pts = paramTypes typ
      return (pts,Just (id,CncCat (Just (L loc typ)) lindef linref pprn mb_prods))
    convInfo g ((mn,id), CncFun mb_ty@(Just r@(_,cat,ctx,lincat)) (Just (L loc def)) pprn mb_prods) = do
      def <- normalForm g (eta_expand def ctx)
      return (Set.empty,Just (id,CncFun mb_ty (Just (L loc def)) pprn mb_prods))
    convInfo g  _ = return (Set.empty,Nothing)

    eta_expand t []                   = t
    eta_expand t ((Implicit,x,_):ctx) = Abs Implicit x (eta_expand (App t (ImplArg (Vr x))) ctx)
    eta_expand t ((Explicit,x,_):ctx) = Abs Explicit x (eta_expand (App t (Vr x))           ctx)


paramTypes (RecType fs) = Set.unions (map (paramTypes.snd) fs)
paramTypes (Table t1 t2) = Set.union (paramTypes t1) (paramTypes t2)
paramTypes (App tf ta) = Set.union (paramTypes tf) (paramTypes ta)
paramTypes (Sort _) = Set.empty
paramTypes (EInt _) = Set.empty
paramTypes (QC q) = Set.singleton q
paramTypes (FV ts) = Set.unions (map paramTypes ts)
paramTypes _ = Set.empty


convFlags :: Grammar -> ModuleName -> Options
convFlags gr mn = err (const noOptions) mflags (lookupModule gr mn)
