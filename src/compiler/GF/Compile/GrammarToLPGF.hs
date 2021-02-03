module GF.Compile.GrammarToLPGF (mkCanon2lpgf) where

import LPGF (LPGF (..))
import qualified LPGF as L

import PGF.CId
import GF.Grammar.Grammar
import qualified GF.Grammar.Canonical as C
import GF.Compile.GrammarToCanonical (grammar2canonical)

import GF.Infra.Option
import GF.Infra.UseIO (IOE)

import Control.Monad (unless)
import Data.Either (lefts, rights)
import Data.List (elemIndex)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Text.Printf (printf)

mkCanon2lpgf :: Options -> SourceGrammar -> ModuleName -> IOE LPGF
mkCanon2lpgf opts gr am = do
  (an,abs) <- mkAbstr ab
  cncs     <- mapM mkConcr cncs
  let lpgf = LPGF {
    L.absname = an,
    L.abstract = abs,
    L.concretes = Map.fromList cncs
  }
  -- print lpgf
  return lpgf
  where
    C.Grammar ab cncs = grammar2canonical opts am gr

    mkAbstr :: C.Abstract -> IOE (CId, L.Abstr)
    mkAbstr (C.Abstract modId flags cats funs) = return (mdi2i modId, L.Abstr {})

    mkConcr :: C.Concrete -> IOE (CId, L.Concr)
    mkConcr (C.Concrete modId absModId flags params lincats lindefs) = do
      -- print params
      -- print lindefs
      let
        paramMap = mkParamMap params
        es = map mkLin lindefs
        lins = Map.fromList $ rights es

        mkLin :: C.LinDef -> Either String (CId, L.LinFun)
        mkLin (C.LinDef funId varIds linValue) = do
          lf <- val2lin linValue
          return (fi2i funId, lf)
          where
            val2lin :: C.LinValue -> Either String L.LinFun
            val2lin lv = case lv of

              C.ConcatValue v1 v2 -> do
                v1' <- val2lin v1
                v2' <- val2lin v2
                return $ L.LFConcat v1' v2'

              C.LiteralValue ll -> case ll of
                C.FloatConstant f -> return $ L.LFToken (show f)
                C.IntConstant i -> return $ L.LFToken (show i)
                C.StrConstant s -> return $ L.LFToken s

              C.ErrorValue err -> return $ L.LFError err

              C.ParamConstant _ -> do -- TODO only works when param value can be known at compile time
                let mixs = map (elemIndex lv) paramMap
                case catMaybes mixs of
                  ix:_ -> return $ L.LFInt (ix+1)
                  _ -> Left $ printf "Cannot find param value: %s" (show lv)

              -- PredefValue PredefId -- TODO predef not supported

              C.RecordValue rrvs -> do
                ts <- sequence [ val2lin lv | C.RecordRow lid lv <- rrvs ]
                return $ L.LFTuple ts

              C.TableValue lt trvs -> do -- lt is type
                ts <- sequence [ val2lin lv | C.TableRow lpatt lv <- trvs ] -- TODO variables in lhs ?
                return $ L.LFTuple ts

              C.TupleValue lvs -> do
                ts <- mapM val2lin lvs
                return $ L.LFTuple ts

              C.VariantValue [] -> return L.LFEmpty
              C.VariantValue (vr:_) -> val2lin vr -- TODO variants not supported, just pick first

              C.VarValue (C.VarValueId (C.Unqual v)) -> do
                ix <- eitherElemIndex (C.VarId v) varIds
                return $ L.LFArgument (ix+1)

              -- PreValue [([String], LinValue)] LinValue -- TODO pre not supported

              -- specific case when lhs is variable into function
              C.Projection (C.VarValue (C.VarValueId (C.Unqual v))) lblId -> do
                -- lookup argument index
                argIx <- eitherElemIndex (C.VarId v) varIds
                -- lookup type for function
                let (C.Abstract _ _ _ funs) = ab
                (C.Type args _) <- case [ ftype | C.FunDef fid ftype <- funs, fid == funId ] of t:_ -> Right t ; _ -> Left $ printf "Cannot find type for: %s" v
                -- lookup type for argument
                let C.TypeBinding _ (C.Type _ (C.TypeApp catId _)) = args !! argIx
                -- lookup label index in argument type
                rrs <- case [ rrs | C.LincatDef cid (C.RecordType rrs) <- lincats, cid == catId ] of t:_ -> Right t ; _ -> Left $ printf "Cannot find type for: %s" (show catId)
                let rrs' = [ lid | C.RecordRow lid _ <- rrs ]
                lblIx <- eitherElemIndex lblId rrs'

                return $ L.LFProjection (L.LFArgument (argIx+1)) (L.LFInt (lblIx+1))

              -- C.Projection v1 (C.LabelId lbl) -> do -- TODO how to handle general case?
                -- v1' <- val2lin v1
                -- let lblIx = undefined
                -- return $ L.LFProjection v1' (L.LFInt (lblIx+1))

              C.Selection v1 v2 -> do
                v1' <- val2lin v1
                v2' <- val2lin v2
                return $ L.LFProjection v1' v2'

              C.CommentedValue cmnt lv -> val2lin lv

              v -> Left $ printf "val2lin not implemented for: %s" (show v)

      unless (null $ lefts es) (error $ unlines (lefts es))

      return (mdi2i modId, L.Concr {
        L.lins = lins
      })

eitherElemIndex :: (Eq a, Show a) => a -> [a] -> Either String Int
eitherElemIndex x xs = case elemIndex x xs of
  Just ix -> Right ix
  Nothing -> Left $ printf "Cannot find: %s" (show x)

mdi2i :: C.ModId -> CId
mdi2i (C.ModId i) = mkCId i

fi2i :: C.FunId -> CId
fi2i (C.FunId i) = mkCId i

-- | Enumerate all paramvalue combinations for looking up index numbers
mkParamMap :: [C.ParamDef] -> [[C.LinValue]]
mkParamMap defs = map mk' defs
  where
    mk' :: C.ParamDef -> [C.LinValue]
    mk' (C.ParamDef _ pids) = concatMap mk'' pids
    mk' (C.ParamAliasDef _ _) = [] -- TODO

    mk'' :: C.ParamValueDef -> [C.LinValue]
    mk'' (C.Param pid []) = [C.ParamConstant (C.Param pid [])]
    mk'' (C.Param pid pids) =
      [ C.ParamConstant (C.Param pid k) | k <- sequence kids ]
      where
        kids =
          [ mk' def
          | p <- pids
          , def <- [ d | d@(C.ParamDef pid _) <- defs, pid == p ]
          ] :: [[C.LinValue]]
