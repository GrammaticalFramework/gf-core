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

import Control.Applicative ((<|>))
import qualified Control.Monad.State as CMS
import Control.Monad (when, unless, forM, forM_)
import Data.Either (lefts, rights)
import Data.List (elemIndex)
import qualified Data.List as L
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust)
import qualified Data.Text as T
import System.Environment (lookupEnv)
import System.FilePath ((</>), (<.>))
import Text.Printf (printf)

mkCanon2lpgf :: Options -> SourceGrammar -> ModuleName -> IOE LPGF
mkCanon2lpgf opts gr am = do
  debug <- isJust <$> lookupEnv "DEBUG"
  when debug $ do
    writeCanonical "DEBUG" canon
    dumpCanonical "DEBUG" canon
  (an,abs) <- mkAbstract ab
  cncs     <- mapM mkConcrete cncs
  let lpgf = LPGF {
    L.absname = an,
    L.abstract = abs,
    L.concretes = Map.fromList cncs
  }
  when debug $ dumpLPGF lpgf
  return lpgf
  where
    canon@(C.Grammar ab cncs) = grammar2canonical opts am gr

    mkAbstract :: C.Abstract -> IOE (CId, L.Abstract)
    mkAbstract (C.Abstract modId flags cats funs) = return (mdi2i modId, L.Abstract {})

    mkConcrete :: C.Concrete -> IOE (CId, L.Concrete)
    mkConcrete (C.Concrete modId absModId flags params lincats lindefs) = do
      let
        paramMap = mkParamMap params
        paramTuples = mkParamTuples params
        es = map mkLin lindefs
        lins = Map.fromList $ rights es

        mkLin :: C.LinDef -> Either String (CId, L.LinFun)
        mkLin (C.LinDef funId varIds linValue) = do
          (lf, _) <- val2lin linValue
          return (fi2i funId, lf)
          where
            val2lin :: C.LinValue -> Either String (L.LinFun, Maybe C.LinType)
            val2lin lv = case lv of

              C.ConcatValue v1 v2 -> do
                (v1',t1) <- val2lin v1
                (v2',t2) <- val2lin v2
                return (L.LFConcat v1' v2', t1 <|> t2) -- t1 else t2

              C.LiteralValue ll -> case ll of
                C.FloatConstant f -> return (L.LFToken $ T.pack $ show f, Just C.FloatType)
                C.IntConstant i -> return (L.LFToken $ T.pack $ show i, Just C.IntType)
                C.StrConstant s -> return (L.LFToken $ T.pack s, Just C.StrType)

              C.ErrorValue err -> return (L.LFError err, Nothing)

              -- when param value can be known at compile time
              -- this case is actually covered below and can be omitted, but it will result in smaller LPGF
              -- and should thus be seen as an optimisation
              C.ParamConstant _ | isParamConstant lv -> do
                (gix,ix) <- [ (gix,ix) | (gix,lvs) <- zip [0..] paramMap, Just ix <- [elemIndex lv lvs] ]
                            `headOrLeft` printf "Cannot find param value: %s" (show lv)
                let (C.ParamDef tpid _) = params !! gix
                return (L.LFInt (ix+1), Just $ C.ParamType (C.ParamTypeId tpid))

              -- when param value is dynamic
              C.ParamConstant (C.Param pid pids) -> do
                -- get param group index and defn for this constructor
                (gix,def) <- [ (gix,d) | (gix,d@(C.ParamDef _ ps)) <- zip [0..] params, any (\(C.Param p _) -> p == pid) ps ]
                              `headOrLeft` printf "Cannot find param group: %s" (show pid)
                let (C.ParamDef tpid defpids) = def

                pidIx <- eitherElemIndex pid [ p | C.Param p _ <- defpids ]
                pids' <- mapM val2lin pids
                let
                  tuple = paramTuples !! gix
                  term = foldl L.LFProjection tuple (L.LFInt (pidIx+1):map fst pids')
                return (term, Just $ C.ParamType (C.ParamTypeId tpid))

              -- https://www.aclweb.org/anthology/W15-3305.pdf
              C.PredefValue (C.PredefId pid) -> case pid of
                "BIND" -> return (L.LFBind, Nothing)
                "SOFT_BIND" -> return (L.LFBind, Nothing)
                "SOFT_SPACE" -> return (L.LFSpace, Nothing)
                "CAPIT" -> return (L.LFCapit, Nothing)
                "ALL_CAPIT" -> return (L.LFAllCapit, Nothing)
                _ -> Left $ printf "Unknown predef function: %s" pid

              C.RecordValue rrvs -> do
                let rrvs' = sortRecordRows rrvs
                ts <- sequence [ val2lin lv | C.RecordRow lid lv <- rrvs' ]
                return (L.LFTuple (map fst ts), Just $ C.RecordType [ C.RecordRow lid lt | (C.RecordRow lid _, (_, Just lt)) <- zip rrvs' ts])

              C.TableValue lt trvs | isRecordType lt -> go trvs
                where
                  go :: [C.TableRowValue] -> Either String (L.LinFun, Maybe C.LinType)
                  go [C.TableRow _ lv] = val2lin lv
                  go trvs = do
                    let grps = L.groupBy (\(C.TableRow (C.RecordPattern rps1) _) (C.TableRow (C.RecordPattern rps2) _) -> head rps1 == head rps2) trvs
                    ts <- mapM (go . map (\(C.TableRow (C.RecordPattern rps) lv) -> C.TableRow (C.RecordPattern (tail rps)) lv)) grps
                    return (L.LFTuple (map fst ts), Just lt)

              C.TableValue lt trvs | isParamType lt -> do
                ts <- sequence [ val2lin lv | C.TableRow _ lv <- trvs ]
                return (L.LFTuple (map fst ts), Just lt)

              -- TODO TuplePattern, WildPattern?

              C.TupleValue lvs -> do
                ts <- mapM val2lin lvs
                return (L.LFTuple (map fst ts), Just $ C.TupleType (map (fromJust.snd) ts))

              C.VariantValue [] -> return (L.LFEmpty, Nothing)
              C.VariantValue (vr:_) -> val2lin vr -- NOTE variants not supported, just pick first

              C.VarValue (C.VarValueId (C.Unqual v)) -> do
                -- lookup argument index
                ix <- eitherElemIndex (C.VarId v) varIds
                -- lookup type for function
                let (C.Abstract _ _ _ funs) = ab
                (C.Type args _) <- [ ftype | C.FunDef fid ftype <- funs, fid == funId ] `headOrLeft` printf "Cannot find type for: %s" v
                -- lookup category for argument
                let C.TypeBinding _ (C.Type _ (C.TypeApp catId _)) = args !! ix
                -- lookup lintype for category
                lt <- [ lt | C.LincatDef cid lt <- lincats, cid == catId ] `headOrLeft` printf "Cannot find type for: %s" (show catId)
                return (L.LFArgument (ix+1), Just lt)

              C.PreValue pts df -> do
                pts' <- forM pts $ \(pfxs, lv) -> do
                  (lv', _) <- val2lin lv
                  return (map T.pack pfxs, lv')
                (df', lt) <- val2lin df
                return (L.LFPre pts' df', lt)

              C.Projection v1 lblId -> do
                (v1', mtyp) <- val2lin v1
                -- find label index in argument type
                let Just (C.RecordType rrs) = mtyp
                let rrs' = [ lid | C.RecordRow lid _ <- rrs ]
                lblIx <- eitherElemIndex lblId rrs'
                -- lookup lintype for record row
                let C.RecordRow _ lt = rrs !! lblIx
                return (L.LFProjection v1' (L.LFInt (lblIx+1)), Just lt)

              C.Selection v1 v2 -> do
                (v1', t1) <- val2lin v1
                (v2', t2) <- val2lin v2
                let Just (C.TableType t11 t12) = t1
                return (L.LFProjection v1' v2', Just t12)

              C.CommentedValue cmnt lv -> val2lin lv

              v -> Left $ printf "val2lin not implemented for: %s" (show v)

      unless (null $ lefts es) (error $ unlines (lefts es))

      return (mdi2i modId, L.Concrete {
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
      let Just def = L.find (\(C.ParamDef p _) -> pid == p) pdefs
      let ms = CMS.evalState (mk' def) 1
      let L.LFTuple ms' = ms
      ns <- sequence
          [ mk'' (C.Param p0 pids)
          | m <- ms'
          ]
      return $ L.LFTuple ns

-- | Always put 's' reocord field first, then sort alphabetically
-- This seems to be done inconsistently in the canonical format
-- Based on GF.Granmar.Macros.sortRec
sortRecord :: C.LinValue -> C.LinValue
sortRecord (C.RecordValue rrvs) = C.RecordValue (sortRecordRows rrvs)
sortRecord lv = lv

sortRecordRows :: [C.RecordRowValue] -> [C.RecordRowValue]
sortRecordRows = L.sortBy ordLabel
  where
    ordLabel (C.RecordRow (C.LabelId l1) _) (C.RecordRow (C.LabelId l2) _) =
      case (l1,l2) of
        ("s",_) -> LT
        (_,"s") -> GT
        (s1,s2) -> compare s1 s2

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

-- | If list is non-empty return its head, else a fallback value
headOrLeft :: [a] -> b -> Either b a
headOrLeft (a:_) _ = Right a
headOrLeft _ b = Left b


-- | Convert Maybe to Either value with error
m2e :: String -> Maybe a -> Either String a
m2e err = maybe (Left err) Right

-- | Wrap elemIndex into Either value
eitherElemIndex :: (Eq a, Show a) => a -> [a] -> Either String Int
eitherElemIndex x xs = m2e (printf "Cannot find: %s in %s" (show x) (show xs)) (elemIndex x xs)

mdi2s :: C.ModId -> String
mdi2s (C.ModId i) = i

mdi2i :: C.ModId -> CId
mdi2i (C.ModId i) = mkCId i

fi2i :: C.FunId -> CId
fi2i (C.FunId i) = mkCId i

-- Debugging

-- -- | Pretty-print canonical grammar to console
-- ppCanonical :: C.Grammar -> IO ()
-- ppCanonical = putStrLn . render . pp

-- | Pretty-print canonical grammars to file
writeCanonical :: FilePath -> C.Grammar -> IO ()
writeCanonical path (C.Grammar ab cncs) = do
  let (C.Abstract modId flags cats funs) = ab
  writeFile (path </> mdi2s modId <.> "canonical.gf") (render $ pp ab)
  forM_ cncs $ \cnc@(C.Concrete modId absModId flags params lincats lindefs) ->
    writeFile (path </> mdi2s modId <.> "canonical.gf") (render $ pp cnc)

-- | Dump canonical grammars to file
dumpCanonical :: FilePath -> C.Grammar -> IO ()
dumpCanonical path (C.Grammar ab cncs) = do
  let (C.Abstract modId flags cats funs) = ab
  let body = unlines $ map show cats ++ [""] ++ map show funs
  writeFile (path </> mdi2s modId <.> "canonical.dump") body

  forM_ cncs $ \(C.Concrete modId absModId flags params lincats lindefs) -> do
    let body = unlines $ concat [
          map show params,
          [""],
          map show lincats,
          [""],
          map show lindefs
          ]
    writeFile (path </> mdi2s modId <.> "canonical.dump") body

-- | Dump LPGF to console
dumpLPGF :: LPGF -> IO ()
dumpLPGF lpgf =
  forM_ (Map.toList $ L.concretes lpgf) $ \(cid,concr) ->
    mapM_ print (Map.toList $ L.lins concr)
