module GF.Compile.GrammarToLPGF (mkCanon2lpgf) where

import LPGF (LPGF (..))
import qualified LPGF as L

import PGF.CId
import GF.Grammar.Grammar
import qualified GF.Grammar.Canonical as C
import GF.Compile.GrammarToCanonical (grammar2canonical)

import GF.Data.Operations (ErrorMonad (..))
import qualified GF.Data.IntMapBuilder as IntMapBuilder
import GF.Infra.Option (Options)
import GF.Infra.UseIO (IOE)
import GF.Text.Pretty (pp, render)

import Control.Applicative ((<|>))
import Control.Monad (when, unless, forM, forM_)
import qualified Control.Monad.State as CMS
import Data.Either (lefts, rights)
import qualified Data.IntMap as IntMap
import Data.List (elemIndex)
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)
import System.FilePath ((</>), (<.>))
import Text.Printf (printf)

mkCanon2lpgf :: Options -> SourceGrammar -> ModuleName -> IOE LPGF
mkCanon2lpgf opts gr am = do
  debug <- isJust <$> lookupEnv "DEBUG"
  when debug $ do
    ppCanonical debugDir canon
    dumpCanonical debugDir canon
  (an,abs) <- mkAbstract ab
  cncs     <- mapM mkConcrete cncs
  let lpgf = LPGF {
    L.absname = an,
    L.abstract = abs,
    L.concretes = Map.fromList cncs
  }
  when debug $ ppLPGF debugDir lpgf
  return lpgf
  where
    canon@(C.Grammar ab cncs) = grammar2canonical opts am gr

    mkAbstract :: (ErrorMonad err) => C.Abstract -> err (CId, L.Abstract)
    mkAbstract (C.Abstract modId flags cats funs) = return (mdi2i modId, L.Abstract {})

    mkConcrete :: (ErrorMonad err) => C.Concrete -> err (CId, L.Concrete)
    mkConcrete (C.Concrete modId absModId flags params' lincats lindefs) = do
      let
        (C.Abstract _ _ _ funs) = ab
        params = inlineParamAliases params'

        -- Builds maps for lookups

        paramValueMap :: Map.Map C.ParamId C.ParamDef -- constructor -> definition
        paramValueMap = Map.fromList [ (v,d) | d@(C.ParamDef _ vs) <- params, (C.Param v _) <- vs ]

        lincatMap :: Map.Map C.CatId C.LincatDef
        lincatMap = Map.fromList [ (cid,d) | d@(C.LincatDef cid _) <- lincats ]

        funMap :: Map.Map C.FunId C.FunDef
        funMap = Map.fromList [ (fid,d) | d@(C.FunDef fid _) <- funs ]

        -- | Lookup lintype for a function
        lookupLinType :: C.FunId -> Either String C.LinType
        lookupLinType funId = do
          fun <- m2e (printf "Cannot find type for: %s" (show funId)) (Map.lookup funId funMap)
          let (C.FunDef _ (C.Type _ (C.TypeApp catId _))) = fun
          lincat <- m2e (printf "Cannot find lincat for: %s" (show catId)) (Map.lookup catId lincatMap)
          let (C.LincatDef _ lt) = lincat
          return lt

        -- | Lookup lintype for a function's argument
        lookupLinTypeArg :: C.FunId -> Int -> Either String C.LinType
        lookupLinTypeArg funId argIx = do
          fun <- m2e (printf "Cannot find type for: %s" (show funId)) (Map.lookup funId funMap)
          let (C.FunDef _ (C.Type args _)) = fun
          let (C.TypeBinding _ (C.Type _ (C.TypeApp catId _))) = args !! argIx
          lincat <- m2e (printf "Cannot find lincat for: %s" (show catId)) (Map.lookup catId lincatMap)
          let (C.LincatDef _ lt) = lincat
          return lt

        -- filter out record fields from defn which don't appear in lincat
        -- this seems to be an inconsistency in the canonical representation
        lindefs' =
          [ C.LinDef funId varIds linValue'
          | (C.LinDef funId varIds linValue) <- lindefs
          , let linValue' = case (linValue, lookupLinType funId) of
                  (C.RecordValue rrvs, Right (C.RecordType rrs)) ->
                    let defnFields = [ lid | (C.RecordRow lid _) <- rrs ]
                    in C.RecordValue [ rrv | rrv@(C.RecordRow lid _) <- rrvs, lid `elem` defnFields ]
                  (x,_) -> x
          ]
        es = map mkLin lindefs'
        lins = Map.fromList $ rights es

        -- | Main code generation function
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
                return (L.Concat v1' v2', t1 <|> t2) -- t1 else t2

              C.LiteralValue ll -> case ll of
                C.FloatConstant f -> return (L.Token $ T.pack $ show f, Just C.FloatType)
                C.IntConstant i -> return (L.Token $ T.pack $ show i, Just C.IntType)
                C.StrConstant s -> return (L.Token $ T.pack s, Just C.StrType)

              C.ErrorValue err -> return (L.Error err, Nothing)

              C.ParamConstant (C.Param pid lvs) -> do
                let
                  collectProjections :: C.LinValue -> Either String [L.LinFun]
                  collectProjections (C.ParamConstant (C.Param pid lvs)) = do
                    def <- m2e (printf "Cannot find param definition: %s" (show pid)) (Map.lookup pid paramValueMap)
                    let (C.ParamDef tpid defpids) = def
                    pidIx <- eitherElemIndex pid [ p | C.Param p _ <- defpids ]
                    rest <- mapM collectProjections lvs
                    return $ L.Ix (pidIx+1) : concat rest
                  collectProjections lv = do
                    (lf,_) <- val2lin lv
                    return [lf]
                lfs <- collectProjections lv
                let term = L.Tuple lfs
                def <- m2e (printf "Cannot find param definition: %s" (show pid)) (Map.lookup pid paramValueMap)
                let (C.ParamDef tpid _) = def
                return (term, Just $ C.ParamType (C.ParamTypeId tpid))

              C.PredefValue (C.PredefId pid) -> case pid of
                "BIND" -> return (L.Bind, Nothing)
                "SOFT_BIND" -> return (L.Bind, Nothing)
                "SOFT_SPACE" -> return (L.Space, Nothing)
                "CAPIT" -> return (L.Capit, Nothing)
                "ALL_CAPIT" -> return (L.AllCapit, Nothing)
                _ -> Left $ printf "Unknown predef function: %s" pid

              C.RecordValue rrvs -> do
                let rrvs' = sortRecordRows rrvs
                ts <- sequence [ val2lin lv | C.RecordRow lid lv <- rrvs' ]
                return (L.Tuple (map fst ts), Just $ C.RecordType [ C.RecordRow lid lt | (C.RecordRow lid _, (_, Just lt)) <- zip rrvs' ts])

              C.TableValue lt trvs | isRecordType lt -> go trvs
                where
                  go :: [C.TableRowValue] -> Either String (L.LinFun, Maybe C.LinType)
                  go [C.TableRow _ lv] = val2lin lv
                  go trvs = do
                    let grps = L.groupBy (\(C.TableRow (C.RecordPattern rps1) _) (C.TableRow (C.RecordPattern rps2) _) -> head rps1 == head rps2) trvs
                    -- ts <- mapM (go . map (\(C.TableRow (C.RecordPattern rps) lv) -> C.TableRow (C.RecordPattern (tail rps)) lv)) grps
                    ts <- forM grps $ \grp ->
                      go $ map (\(C.TableRow (C.RecordPattern rps) lv) -> C.TableRow (C.RecordPattern (tail rps)) lv) grp
                    let typ = case ts of
                          (_, Just tst):_ -> Just $ C.TableType lt tst
                          _ -> Nothing
                    return (L.Tuple (map fst ts), typ)

              C.TableValue lt trvs | isParamType lt -> go trvs
                where
                  go :: [C.TableRowValue] -> Either String (L.LinFun, Maybe C.LinType)
                  go [C.TableRow _ lv] = val2lin lv
                  go trvs = do
                    let grps = L.groupBy (\(C.TableRow (C.ParamPattern (C.Param pid1 _)) _) (C.TableRow (C.ParamPattern (C.Param pid2 _)) _) -> pid1 == pid2) trvs
                    ts <- forM grps $ \grp ->
                      go =<< forM grp (\row ->
                        case row of
                          C.TableRow (C.ParamPattern (C.Param _ [])) lv -> return row
                          C.TableRow (C.ParamPattern (C.Param _ patts)) lv -> return $ C.TableRow (C.ParamPattern (C.Param pid' patts')) lv
                            where
                              C.ParamPattern (C.Param pid1 patts1) = head patts
                              pid' = pid1
                              patts' = patts1 ++ tail patts
                          _ -> Left $ printf "Unhandled table row: %s" (show row)
                        )
                    let typ = case ts of
                          (_, Just tst):_ -> Just $ C.TableType lt tst
                          _ -> Nothing
                    return (L.Tuple (map fst ts), typ)

              -- TODO TuplePattern, WildPattern?

              C.TupleValue lvs -> do
                ts <- mapM val2lin lvs
                return (L.Tuple (map fst ts), Just $ C.TupleType (map (fromJust.snd) ts))

              C.VariantValue [] -> return (L.Empty, Nothing) -- TODO Just C.StrType ?
              C.VariantValue (vr:_) -> val2lin vr -- NOTE variants not supported, just pick first

              C.VarValue (C.VarValueId (C.Unqual v)) -> do
                ix <- eitherElemIndex (C.VarId v) varIds
                lt <- lookupLinTypeArg funId ix
                return (L.Argument (ix+1), Just lt)

              C.PreValue pts df -> do
                pts' <- forM pts $ \(pfxs, lv) -> do
                  (lv', _) <- val2lin lv
                  return (map T.pack pfxs, lv')
                (df', lt) <- val2lin df
                return (L.Pre pts' df', lt)

              C.Projection v1 lblId -> do
                (v1', mtyp) <- val2lin v1
                -- find label index in argument type
                let Just (C.RecordType rrs) = mtyp
                let rrs' = [ lid | C.RecordRow lid _ <- rrs ]
                -- lblIx <- eitherElemIndex lblId rrs'
                let
                  lblIx = case eitherElemIndex lblId rrs' of
                    Right x -> x
                    Left _ -> 0 -- corresponds to Prelude.False
                -- lookup lintype for record row
                let C.RecordRow _ lt = rrs !! lblIx
                return (L.Projection v1' (L.Ix (lblIx+1)), Just lt)

              C.Selection v1 v2 -> do
                (v1', t1) <- val2lin v1
                (v2', t2) <- val2lin v2
                let Just (C.TableType t11 t12) = t1 -- t11 == t2
                return (L.Projection v1' v2', Just t12)

              -- C.CommentedValue cmnt lv -> val2lin lv
              C.CommentedValue cmnt lv -> case cmnt of
                "impossible" -> val2lin lv >>= \(_, typ) -> return (L.Empty, typ) -- TODO untested optimisation
                _ -> val2lin lv

              v -> Left $ printf "val2lin not implemented for: %s" (show v)

      unless (null $ lefts es) (raise $ unlines (lefts es))

      let concr = extractStrings $ L.Concrete {
        L.toks = IntMap.empty,
        L.lins = lins
      }
      return (mdi2i modId, concr)

-- | Remove ParamAliasDefs by inlining their definitions
inlineParamAliases :: [C.ParamDef] -> [C.ParamDef]
inlineParamAliases defs = if null aliases then defs else map rp' pdefs
  where
    (aliases,pdefs) = L.partition isParamAliasDef defs

    rp' :: C.ParamDef -> C.ParamDef
    rp' (C.ParamDef pid pids) = C.ParamDef pid (map rp'' pids)
    rp' (C.ParamAliasDef _ _) = error "inlineParamAliases called on ParamAliasDef" -- impossible

    rp'' :: C.ParamValueDef -> C.ParamValueDef
    rp'' (C.Param pid pids) = C.Param pid (map rp''' pids)

    rp''' :: C.ParamId -> C.ParamId
    rp''' pid = case L.find (\(C.ParamAliasDef p _) -> p == pid) aliases of
      Just (C.ParamAliasDef _ (C.ParamType (C.ParamTypeId p))) -> p
      _ -> pid

-- | Always put 's' reocord field first, then sort alphabetically
-- This seems to be done inconsistently in the canonical format
-- Based on GF.Granmar.Macros.sortRec
sortRecordRows :: [C.RecordRowValue] -> [C.RecordRowValue]
sortRecordRows = L.sortBy ordLabel
  where
    ordLabel (C.RecordRow (C.LabelId l1) _) (C.RecordRow (C.LabelId l2) _) =
      case (l1,l2) of
        ("s",_) -> LT
        (_,"s") -> GT
        (s1,s2) -> compare s1 s2

-- sortRecord :: C.LinValue -> C.LinValue
-- sortRecord (C.RecordValue rrvs) = C.RecordValue (sortRecordRows rrvs)
-- sortRecord lv = lv

isParamAliasDef :: C.ParamDef -> Bool
isParamAliasDef (C.ParamAliasDef _ _) = True
isParamAliasDef _ = False

isParamType :: C.LinType -> Bool
isParamType (C.ParamType _) = True
isParamType _ = False

isRecordType :: C.LinType -> Bool
isRecordType (C.RecordType _) = True
isRecordType _ = False

-- | Find all token strings, put them in a map and replace with token indexes
extractStrings :: L.Concrete -> L.Concrete
extractStrings concr = L.Concrete { L.toks = toks', L.lins = lins' }
  where
    imb = IntMapBuilder.fromIntMap (L.toks concr)
    (lins',imb') = CMS.runState (go0 (L.lins concr)) imb
    toks' = IntMapBuilder.toIntMap imb'

    go0 :: Map.Map CId L.LinFun -> CMS.State (IntMapBuilder.IMB Text) (Map.Map CId L.LinFun)
    go0 mp = do
      xs <- mapM (\(cid,lin) -> go lin >>= \lin' -> return (cid,lin')) (Map.toList mp)
      return $ Map.fromList xs

    go :: L.LinFun -> CMS.State (IntMapBuilder.IMB Text) L.LinFun
    go lf = case lf of
      L.Token str -> do
        imb <- CMS.get
        let (ix,imb') = IntMapBuilder.insert' str imb
        CMS.put imb'
        return $ L.TokenIx ix

      L.Pre pts df -> do
        -- pts' <- mapM (\(pfxs,lv) -> go lv >>= \lv' -> return (pfxs,lv')) pts
        pts' <- forM pts $ \(pfxs,lv) -> do
          imb <- CMS.get
          let str = T.pack $ show pfxs
          let (ix,imb') = IntMapBuilder.insert' str imb
          CMS.put imb'
          lv' <- go lv
          return (ix,lv')
        df' <- go df
        return $ L.PreIx pts' df'
      L.Concat s t -> do
        s' <- go s
        t' <- go t
        return $ L.Concat s' t'
      L.Tuple ts -> do
        ts' <- mapM go ts
        return $ L.Tuple ts'
      L.Projection t u -> do
        t' <- go t
        u' <- go u
        return $ L.Projection t' u'
      t -> return t

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

debugDir :: FilePath
debugDir = "DEBUG"

-- | Pretty-print canonical grammars to file
ppCanonical :: FilePath -> C.Grammar -> IO ()
ppCanonical path (C.Grammar ab cncs) = do
  let (C.Abstract modId flags cats funs) = ab
  writeFile (path </> mdi2s modId <.> "canonical.gf") (render $ pp ab)
  forM_ cncs $ \cnc@(C.Concrete modId absModId flags params lincats lindefs) ->
    writeFile' (path </> mdi2s modId <.> "canonical.gf") (render $ pp cnc)

-- | Dump canonical grammars to file
dumpCanonical :: FilePath -> C.Grammar -> IO ()
dumpCanonical path (C.Grammar ab cncs) = do
  let (C.Abstract modId flags cats funs) = ab
  let body = unlines $ map show cats ++ [""] ++ map show funs
  writeFile' (path </> mdi2s modId <.> "canonical.dump") body

  forM_ cncs $ \(C.Concrete modId absModId flags params lincats lindefs) -> do
    let body = unlines $ concat [
          map show params,
          [""],
          map show lincats,
          [""],
          map show lindefs
          ]
    writeFile' (path </> mdi2s modId <.> "canonical.dump") body

-- | Pretty-print LPGF to file
ppLPGF :: FilePath -> LPGF -> IO ()
ppLPGF path lpgf =
  forM_ (Map.toList $ L.concretes lpgf) $ \(cid,concr) ->
    writeFile' (path </> showCId cid <.> "lpgf.txt") (L.render $ L.pp concr)

-- | Dump LPGF to file
dumpLPGF :: FilePath -> LPGF -> IO ()
dumpLPGF path lpgf =
  forM_ (Map.toList $ L.concretes lpgf) $ \(cid,concr) -> do
    let body = unlines $ map show (Map.toList $ L.lins concr)
    writeFile' (path </> showCId cid <.> "lpgf.dump") body

-- | Write a file and report it to console
writeFile' :: FilePath -> String -> IO ()
writeFile' p b = do
  writeFile p b
  putStrLn $ "Wrote " ++ p
