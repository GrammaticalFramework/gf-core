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
import qualified Control.Monad.State.Strict as CMS
import Data.Either (lefts, rights)
import Data.List (elemIndex)
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)
import System.FilePath ((</>), (<.>))
import Text.Printf (printf)

import qualified Debug.Trace
trace x = Debug.Trace.trace ("> " ++ show x) (return ())

mkCanon2lpgf :: Options -> SourceGrammar -> ModuleName -> IOE LPGF
mkCanon2lpgf opts gr am = do
  debug <- isJust <$> lookupEnv "DEBUG"
  when debug $ do
    ppCanonical debugDir canon
    dumpCanonical debugDir canon
  (an,abs) <- mkAbstract ab
  cncs     <- mapM (mkConcrete debug ab) cncs
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

mkConcrete :: (ErrorMonad err) => Bool -> C.Abstract -> C.Concrete -> err (CId, L.Concrete)
mkConcrete debug (C.Abstract _ _ _ funs) (C.Concrete modId absModId flags params0 lincats0 lindefs0) = do
  let
    -- Some transformations on canonical grammar

    params :: [C.ParamDef]
    params = inlineParamAliases params0

    lincats :: [C.LincatDef]
    lincats = s:i:f:lincats0
      where
        ss = C.RecordType [C.RecordRow (C.LabelId "s") C.StrType]
        s = C.LincatDef (C.CatId "String") ss
        i = C.LincatDef (C.CatId "Int") ss
        f = C.LincatDef (C.CatId "Float") ss

    lindefs :: [C.LinDef]
    lindefs =
      [ C.LinDef funId varIds linValue'
      | (C.LinDef funId varIds linValue) <- lindefs0
      , let Right linType = lookupLinType funId
      , let linValue' = cleanupRecordFields linValue linType
      ]

    -- Filter out record fields from definitions which don't appear in lincat.
    -- Workaround for https://github.com/GrammaticalFramework/gf-core/issues/101
    cleanupRecordFields :: C.LinValue -> C.LinType -> C.LinValue
    cleanupRecordFields (C.RecordValue rrvs) (C.RecordType rrs) =
      let defnFields = Map.fromList [ (lid, lt) | (C.RecordRow lid lt) <- rrs ]
      in C.RecordValue
          [ C.RecordRow lid lv'
          | C.RecordRow lid lv <- rrvs
          , Map.member lid defnFields
          , let Just lt = Map.lookup lid defnFields
          , let lv' = cleanupRecordFields lv lt
          ]
    cleanupRecordFields lv _ = lv

    -- Builds maps for lookups

    paramValueMap :: Map.Map C.ParamId C.ParamDef -- constructor -> definition
    paramValueMap = Map.fromList [ (v,d) | d@(C.ParamDef _ vs) <- params, (C.Param v _) <- vs ]

    lincatMap :: Map.Map C.CatId C.LincatDef
    lincatMap = Map.fromList [ (cid,d) | d@(C.LincatDef cid _) <- lincats ]

    funMap :: Map.Map C.FunId C.FunDef
    funMap = Map.fromList [ (fid,d) | d@(C.FunDef fid _) <- funs ]

    -- | Lookup paramdef, providing dummy fallback when not found
    -- Workaround for https://github.com/GrammaticalFramework/gf-core/issues/100
    lookupParamDef :: C.ParamId -> Either String C.ParamDef
    lookupParamDef pid = case Map.lookup pid paramValueMap of
      Just d -> Right d
      Nothing ->
        -- Left $ printf "Cannot find param definition: %s" (show pid)
        Right $ C.ParamDef (C.ParamId (C.Unqual "DUMMY")) [C.Param pid []]

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

    -- Code generation

    -- | Main code generation function
    mkLin :: C.LinDef -> CodeGen (CId, L.LinFun)
    mkLin (C.LinDef funId varIds linValue) = do
      -- when debug $ trace funId
      (lf, _) <- val2lin linValue
      return (fi2i funId, lf)
      where
        val2lin :: C.LinValue -> CodeGen (L.LinFun, Maybe C.LinType)
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
              collectProjections :: C.LinValue -> CodeGen [L.LinFun]
              collectProjections (C.ParamConstant (C.Param pid lvs)) = do
                def <- lookupParamDef pid
                let (C.ParamDef tpid defpids) = def
                pidIx <- eitherElemIndex pid [ p | C.Param p _ <- defpids ]
                rest <- mapM collectProjections lvs
                return $ L.Ix (pidIx+1) : concat rest
              collectProjections lv = do
                (lf,_) <- val2lin lv
                return [lf]
            lfs <- collectProjections lv
            let term = L.Tuple lfs
            def <- lookupParamDef pid
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

          C.TableValue lt trvs -> do
            -- group the rows by "left-most" value
            let
              groupRow :: C.TableRowValue -> C.TableRowValue -> Bool
              groupRow (C.TableRow p1 _) (C.TableRow p2 _) = groupPattern p1 p2

              groupPattern :: C.LinPattern -> C.LinPattern -> Bool
              groupPattern p1 p2 = case (p1,p2) of
                (C.ParamPattern (C.Param pid1 _), C.ParamPattern (C.Param pid2 _)) -> pid1 == pid2 -- compare only constructors
                (C.RecordPattern (C.RecordRow lid1 patt1:_), C.RecordPattern (C.RecordRow lid2 patt2:_)) -> groupPattern patt1 patt2 -- lid1 == lid2 necessarily
                _ -> error $ printf "Mismatched patterns in grouping:\n%s\n%s" (show p1) (show p2)

              grps :: [[C.TableRowValue]]
              grps = L.groupBy groupRow trvs

            -- remove one level of depth and recurse
            let
              handleGroup :: [C.TableRowValue] -> CodeGen (L.LinFun, Maybe C.LinType)
              handleGroup [C.TableRow patt lv] =
                case reducePattern patt of
                  Just patt' -> do
                    (lf,lt) <- handleGroup [C.TableRow patt' lv]
                    return (L.Tuple [lf],lt)
                  Nothing    -> val2lin lv
              handleGroup rows = do
                let rows' = map reduceRow rows
                val2lin (C.TableValue lt rows') -- lt is wrong here, but is unused

              reducePattern :: C.LinPattern -> Maybe C.LinPattern
              reducePattern patt =
                case patt of
                  C.ParamPattern (C.Param _ []) -> Nothing
                  C.ParamPattern (C.Param _ patts) -> Just $ C.ParamPattern (C.Param pid' patts')
                    where
                      C.ParamPattern (C.Param pid1 patts1) = head patts
                      pid' = pid1
                      patts' = patts1 ++ tail patts

                  C.RecordPattern [] -> Nothing
                  C.RecordPattern (C.RecordRow lid patt:rrs) ->
                    case reducePattern patt of
                      Just patt' -> Just $ C.RecordPattern (C.RecordRow lid patt':rrs)
                      Nothing    -> if null rrs then Nothing else Just $ C.RecordPattern rrs

                  _ -> error $ printf "Unhandled pattern in reducing: %s" (show patt)

              reduceRow :: C.TableRowValue -> C.TableRowValue
              reduceRow (C.TableRow patt lv) =
                let Just patt' = reducePattern patt
                in  C.TableRow patt' lv

            -- ts :: [(L.LinFun, Maybe C.LinType)]
            ts <- mapM handleGroup grps

            -- return
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
            "impossible" -> return (L.Empty, Nothing)
            -- "impossible" -> val2lin lv >>= \(_, typ) -> return (L.Empty, typ)
            _ -> val2lin lv

          v -> Left $ printf "val2lin not implemented for: %s" (show v)

  -- Invoke code generation

  let es = map mkLin lindefs
  unless (null $ lefts es) (raise $ unlines (lefts es))

  let maybeOptimise = if debug then id else extractStrings
  let concr = maybeOptimise $ L.Concrete {
    L.toks = IntMapBuilder.emptyIntMap,
    L.lins = Map.fromList (rights es)
  }
  return (mdi2i modId, concr)

type CodeGen a = Either String a

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

-- | Always put 's' reocord field first, then sort alphabetically.
-- Workaround for https://github.com/GrammaticalFramework/gf-core/issues/102
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
