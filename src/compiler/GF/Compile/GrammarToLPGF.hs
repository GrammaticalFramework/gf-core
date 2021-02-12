module GF.Compile.GrammarToLPGF (mkCanon2lpgf) where

import LPGF (LPGF (..))
import qualified LPGF as L

import PGF.CId
import GF.Grammar.Grammar
import qualified GF.Grammar.Canonical as C
import GF.Compile.GrammarToCanonical (grammar2canonical)

import GF.Infra.Option
import GF.Infra.UseIO (IOE)
import GF.Text.Pretty (pp, render)

import qualified Control.Monad.State as CMS
import Control.Monad (unless, forM_)
import Data.Either (lefts, rights)
import Data.List (elemIndex, find, groupBy)
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
  -- ppCanonical canon
  -- dumpCanonical canon
  -- dumpLPGF lpgf
  return lpgf
  where
    canon@(C.Grammar ab cncs) = grammar2canonical opts am gr

    mkAbstr :: C.Abstract -> IOE (CId, L.Abstr)
    mkAbstr (C.Abstract modId flags cats funs) = return (mdi2i modId, L.Abstr {})

    mkConcr :: C.Concrete -> IOE (CId, L.Concr)
    mkConcr (C.Concrete modId absModId flags params lincats lindefs) = do
      let
        paramMap = mkParamMap params
        paramTuples = mkParamTuples params
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

              -- when param value can be known at compile time
              -- this case is actually covered below and can be omitted, but it will result in smaller LPGF
              -- and should thus be seen as an optimisation
              C.ParamConstant _ | isParamConstant lv -> do
                let mixs = map (elemIndex lv) paramMap
                case catMaybes mixs of
                  ix:_ -> return $ L.LFInt (ix+1)
                  _ -> Left $ printf "Cannot find param value: %s" (show lv)

              -- when param value is dynamic
              C.ParamConstant (C.Param pid pids) -> do
                -- get param group index and defn for this constructor
                let defs = [ (gix,d) | (gix,d@(C.ParamDef _ ps)) <- zip [0..] params, any (\(C.Param p _) -> p == pid) ps ] :: [(Int,C.ParamDef)]
                (gix,def) <- if null defs then Left (printf "Cannot find param group: %s" (show pid)) else Right $ head defs
                let (C.ParamDef _ defpids) = def

                pidIx <- eitherElemIndex pid [ p | C.Param p _ <- defpids ]
                pids' <- mapM val2lin pids
                let
                  tuple = paramTuples !! gix
                  term = foldl L.LFProjection tuple (L.LFInt (pidIx+1):pids')
                return term

              -- PredefValue PredefId -- TODO predef not supported

              C.RecordValue rrvs -> do
                ts <- sequence [ val2lin lv | C.RecordRow lid lv <- rrvs ]
                return $ L.LFTuple ts

              C.TableValue lt trvs | isRecordType lt -> go trvs
                where
                  go :: [C.TableRowValue] -> Either String L.LinFun
                  go [C.TableRow _ lv] = val2lin lv
                  go trvs = do
                    let grps = groupBy (\(C.TableRow (C.RecordPattern rps1) _) (C.TableRow (C.RecordPattern rps2) _) -> head rps1 == head rps2) trvs
                    ts <- mapM (go . map (\(C.TableRow (C.RecordPattern rps) lv) -> C.TableRow (C.RecordPattern (tail rps)) lv)) grps
                    return $ L.LFTuple ts

              -- C.TableValue lt trvs | isParamType lt -> do
              C.TableValue _ trvs -> do
                ts <- sequence [ val2lin lv | C.TableRow _ lv <- trvs ] -- TODO variables in lhs ?
                return $ L.LFTuple ts

              C.TupleValue lvs -> do
                ts <- mapM val2lin lvs
                return $ L.LFTuple ts

              C.VariantValue [] -> return L.LFEmpty
              C.VariantValue (vr:_) -> val2lin vr -- NOTE variants not supported, just pick first

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


-- | Enumerate all paramvalue combinations for looking up index numbers
mkParamMap :: [C.ParamDef] -> [[C.LinValue]]
mkParamMap defs = map mk' defs
  where
    mk' :: C.ParamDef -> [C.LinValue]
    mk' (C.ParamDef _ pids) = concatMap mk'' pids
    mk' (C.ParamAliasDef _ _) = error "mkParamMap not implemented for ParamAliasDef" -- TODO

    mk'' :: C.ParamValueDef -> [C.LinValue]
    mk'' (C.Param pid []) = [C.ParamConstant (C.Param pid [])]
    mk'' (C.Param pid pids) =
      [ C.ParamConstant (C.Param pid k) | k <- sequence kids ]
      where
        kids =
          [ mk' def
          | p <- pids
          , let Just def = find (\(C.ParamDef pid _) -> pid == p) defs
          ] :: [[C.LinValue]]

-- | Build LPGF tuple of param values, needed when param index is looked up dynamically
mkParamTuples :: [C.ParamDef] -> [L.LinFun]
mkParamTuples defs = map (\def -> CMS.evalState (mk' def) 1) defs
  where
    mk' :: C.ParamDef -> CMS.State Int L.LinFun
    mk' (C.ParamDef _ pids) = do
      ms <- mapM mk'' pids
      return $ L.LFTuple ms
    mk' (C.ParamAliasDef _ _) = error "mkParamTuples not implemented for ParamAliasDef" -- TODO

    mk'' :: C.ParamValueDef -> CMS.State Int L.LinFun
    mk'' (C.Param _ []) = do
      ix <- CMS.get
      CMS.modify (+1)
      return $ L.LFInt ix
    mk'' (C.Param p0 (pid:pids)) = do
      let Just def = find (\(C.ParamDef p _) -> pid == p) defs
      let ms = CMS.evalState (mk' def) 1
      let L.LFTuple ms' = ms
      ns <- sequence
          [ mk'' (C.Param p0 pids)
          | m <- ms'
          ]
      return $ L.LFTuple ns

isParamType :: C.LinType -> Bool
isParamType (C.ParamType _) = True
isParamType _ = False

isRecordType :: C.LinType -> Bool
isRecordType (C.RecordType _) = True
isRecordType _ = False

-- | Is a param value completely constant/static?
isParamConstant :: C.LinValue -> Bool
isParamConstant (C.ParamConstant (C.Param _ lvs)) = all isParamConstant lvs
isParamConstant _ = False

isLFInt :: L.LinFun -> Bool
isLFInt (L.LFInt _) = True
isLFInt _ = False

-- | Convert Maybe to Either value with error
m2e :: String -> Maybe a -> Either String a
m2e err = maybe (Left err) Right

-- | Wrap elemIndex into Either value
eitherElemIndex :: (Eq a, Show a) => a -> [a] -> Either String Int
eitherElemIndex x xs = m2e (printf "Cannot find: %s" (show x)) (elemIndex x xs)

mdi2i :: C.ModId -> CId
mdi2i (C.ModId i) = mkCId i

fi2i :: C.FunId -> CId
fi2i (C.FunId i) = mkCId i

-- | Pretty-print canonical grammar, for debugging
ppCanonical :: C.Grammar -> IO ()
ppCanonical = putStrLn . render . pp

-- | Dump canonical grammar, for debugging
dumpCanonical :: C.Grammar -> IO ()
dumpCanonical (C.Grammar ab cncs) = do
  putStrLn ""
  forM_ cncs $ \(C.Concrete modId absModId flags params lincats lindefs) -> do
    mapM_ print params
    putStrLn ""
    mapM_ print lindefs
    putStrLn ""

-- | Dump LPGF, for debugging
dumpLPGF :: LPGF -> IO ()
dumpLPGF lpgf =
  forM_ (Map.toList $ L.concretes lpgf) $ \(cid,concr) ->
    mapM_ print (Map.toList $ L.lins concr)
