module GF.Compile.GrammarToLPGF (mkCanon2lpgf) where

import LPGF (LPGF (..))
import qualified LPGF as L

import PGF.CId
import GF.Grammar.Predef
import GF.Grammar.Grammar
import qualified GF.Grammar.Lookup as Look
import qualified GF.Grammar as A
import qualified GF.Grammar.Macros as GM

import GF.Infra.Ident
import GF.Infra.Option
import GF.Infra.UseIO (IOE)
import GF.Data.Operations

import Control.Monad (forM_)
import Data.List (elemIndex)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

mkCanon2lpgf :: Options -> SourceGrammar -> ModuleName -> IOE LPGF
mkCanon2lpgf opts gr am = do
  (an,abs) <- mkAbstr am
  cncs     <- mapM mkConcr (allConcretes gr am)
  return $ LPGF {
    L.absname = an,
    L.abstract = abs,
    L.concretes = Map.fromList cncs
  }
  where
    mkAbstr :: ModuleName -> IOE (CId, L.Abstr)
    mkAbstr am = do
      let
        adefs =
            [((cPredefAbs,c), AbsCat (Just (L NoLoc []))) | c <- [cFloat,cInt,cString]] ++
            Look.allOrigInfos gr am

        -- funs = Map.fromList [ (i2i f, mkType [] ty)
        --                     | ((m,f),AbsFun (Just (L _ ty)) ma mdef _) <- adefs
        --                     , let arity = mkArity ma mdef ty
        --                     ]
        --
        -- cats = Map.fromList [ (i2i c, ())
        --                     | ((m,c),AbsCat (Just (L _ cont))) <- adefs
        --                     ]

      return (mi2i am, L.Abstr {
        -- L.cats = cats,
        -- L.funs = funs
      })

    mkConcr :: ModuleName -> IOE (CId, L.Concr)
    mkConcr cm = do

      let
        js = fromErr [] $ do
          mo <- lookupModule gr cm
          -- return [((m,c),i) | (c,_) <- Map.toList (jments mo), Ok (m,i) <- [Look.lookupOrigInfo gr (cm,c)]]
          return $ Map.toList (jments mo)

        -- lincats = Map.fromList []
        lins = Map.fromList $ mapMaybe mkLin js

        mkLin :: (Ident, Info) -> Maybe (CId, L.LinFun)
        mkLin (i, info) = case info of
          CncFun typ def@(Just (L (Local n _) term)) pn pmcfg -> do
            lin <- term2lin [] Nothing term
            return (i2i i, lin)
          _ -> Nothing

        term2lin :: [Ident] -> Maybe Type -> Term -> Maybe L.LinFun
        term2lin cxt mtype t = case t of
          Abs Explicit arg term -> term2lin (arg:cxt) mtype term
          C t1 t2 -> do
            t1' <- term2lin cxt Nothing t1
            t2' <- term2lin cxt Nothing t2
            return $ L.LFConcat t1' t2'
          K s -> Just $ L.LFToken s
          Vr arg -> do
            ix <- elemIndex arg (reverse cxt)
            return $ L.LFArgument (ix+1)
          R asgns -> do
            ts <- sequence [ term2lin cxt mtype term | (_, (mtype, term)) <- asgns ]
            return $ L.LFTuple ts
          QC qiV -> do
            QC qiP <- mtype
            let vs = [ ic | QC ic <- fromErr [] $ Look.lookupParamValues gr qiP ]
            ix <- elemIndex qiV vs
            return $ L.LFInt (ix+1)
          _ -> Nothing

      return (mi2i cm, L.Concr {
        -- L.lincats = lincats,
        L.lins = lins
      })

i2i :: Ident -> CId
i2i = utf8CId . ident2utf8

mi2i :: ModuleName -> CId
mi2i (MN i) = i2i i

-- mkType :: [Ident] -> A.Type -> L.Type
-- mkType scope t =
--   case GM.typeForm t of
--     (hyps,(_,cat),args) -> L.Type (map (\(bt,i,t) -> i2i i) hyps) (i2i cat)

-- mkArity (Just a) _        ty = a   -- known arity, i.e. defined function
-- mkArity Nothing  (Just _) ty = 0   -- defined function with no arity - must be an axiom
-- mkArity Nothing  _        ty = let (ctxt, _, _) = GM.typeForm ty  -- constructor
--                                in length ctxt
