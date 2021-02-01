module GF.Compile.GrammarToLPGF (mkCanon2lpgf) where

import LPGF (LPGF (..))
import qualified LPGF as L

import PGF.CId
-- import GF.Grammar.Predef
import GF.Grammar.Grammar
-- import qualified GF.Grammar.Lookup as Look
-- import qualified GF.Grammar as A
-- import qualified GF.Grammar.Macros as GM
import qualified GF.Grammar.Canonical as C
import GF.Compile.GrammarToCanonical (grammar2canonical)

import GF.Infra.Ident
import GF.Infra.Option
import GF.Infra.UseIO (IOE)
-- import GF.Data.Operations

-- import Control.Monad (forM_)
import Data.Either (lefts, rights)
import Data.List (elemIndex)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Text.Printf (printf)

mkCanon2lpgf :: Options -> SourceGrammar -> ModuleName -> IOE LPGF
mkCanon2lpgf opts gr am = do
  let grcn@(C.Grammar ab cncs) = grammar2canonical opts am gr
  (an,abs) <- mkAbstr ab
  cncs     <- mapM mkConcr cncs
  let lpgf = LPGF {
    L.absname = an,
    L.abstract = abs,
    L.concretes = Map.fromList cncs
  }
  print lpgf
  return lpgf
  where
    mkAbstr :: C.Abstract -> IOE (CId, L.Abstr)
    mkAbstr (C.Abstract modId flags cats funs) = return (mdi2i modId, L.Abstr {})

    mkConcr :: C.Concrete -> IOE (CId, L.Concr)
    mkConcr (C.Concrete modId absModId flags params lincats lindefs) = do
      -- print modId
      -- print absModId
      -- print flags
      -- print params
      -- print lincats
      -- print lindefs

      let
        es = map mkLin lindefs
        lins = Map.fromList $ rights es

        mkLin :: C.LinDef -> Either String (CId, L.LinFun)
        mkLin ld@(C.LinDef funId varIds linValue) = do
          lf <- val2lin varIds linValue
          return (fi2i funId, lf)

        val2lin :: [C.VarId] -> C.LinValue -> Either String L.LinFun
        val2lin vids lv = case lv of

          C.ConcatValue v1 v2 -> do
            v1' <- val2lin vids v1
            v2' <- val2lin vids v2
            return $ L.LFConcat v1' v2'

          C.LiteralValue ll -> case ll of
            C.FloatConstant f -> return $ L.LFToken (show f)
            C.IntConstant i -> return $ L.LFToken (show i) -- LFInt ?
            C.StrConstant s -> return $ L.LFToken s

          C.ErrorValue err -> return $ L.LFError err

          C.ParamConstant p@(C.Param (C.ParamId (C.Qual _ _)) _) -> do
            let
              mixs =
                [ elemIndex p pvs
                | C.ParamDef pid pvds <- params
                , let pvs = map (\(C.Param pid []) -> C.Param pid []) pvds -- TODO assumption of [] probably wrong
                ] -- look in all paramdefs
            case catMaybes mixs of
              ix:_ -> return $ L.LFInt (ix+1)
              _ -> Left $ printf "Cannot find param value: %s" (show p)

          -- PredefValue PredefId -- TODO predef not supported

          C.RecordValue rrvs -> do
            ts <- sequence [ val2lin vids lv | C.RecordRow lid lv <- rrvs ]
            return $ L.LFTuple ts

          C.TableValue lt trvs -> do
            ts <- sequence [ val2lin vids lv | C.TableRow lpatt lv <- trvs ] -- TODO variables in lhs
            return $ L.LFTuple ts

          C.TupleValue lvs -> do
            ts <- mapM (val2lin vids) lvs
            return $ L.LFTuple ts

          C.VariantValue [] -> return L.LFEmpty
          C.VariantValue (vr:_) -> val2lin vids vr -- TODO variants not supported, just pick first

          C.VarValue (C.VarValueId (C.Unqual v)) -> do
            ix <- eitherElemIndex (C.VarId v) vids
            return $ L.LFArgument (ix+1)

          -- PreValue [([String], LinValue)] LinValue -- TODO pre not supported

          C.Projection v1 (C.LabelId lbl) -> do
            v1' <- val2lin vids v1
            let lblIx = case lbl of -- TODO
                  "s" -> 0
                  "n" -> 1
                  "p" -> 2
            return $ L.LFProjection v1' (L.LFInt (lblIx+1))

          C.Selection v1 v2 -> do
            v1' <- val2lin vids v1
            v2' <- val2lin vids v2
            return $ L.LFProjection v1' v2'

          C.CommentedValue cmnt lv -> val2lin vids lv

          v -> Left $ printf "val2lin not implemented for: %s" (show v)

      mapM_ putStrLn (lefts es)

      return (mdi2i modId, L.Concr {
        -- L.lincats = lincats,
        L.lins = lins
      })

eitherElemIndex :: (Eq a, Show a) => a -> [a] -> Either String Int
eitherElemIndex x xs = case elemIndex x xs of
  Just ix -> Right ix
  Nothing -> Left $ printf "Cannot find: %s" (show x)

i2i :: Ident -> CId
i2i = utf8CId . ident2utf8

mi2i :: ModuleName -> CId
mi2i (MN i) = i2i i

mdi2i :: C.ModId -> CId
mdi2i (C.ModId i) = mkCId i

fi2i :: C.FunId -> CId
fi2i (C.FunId i) = mkCId i
