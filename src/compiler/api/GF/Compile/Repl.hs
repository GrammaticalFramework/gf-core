{-# LANGUAGE LambdaCase, TupleSections, NamedFieldPuns #-}

module GF.Compile.Repl (ReplOpts(..), defaultReplOpts, replOptDescrs, getReplOpts, runRepl, runRepl') where

import Control.Monad (join, when, unless, forM_, foldM)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.Char8 as BS
import Data.Char (isSpace)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (find)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

import System.Console.GetOpt (ArgOrder(RequireOrder), OptDescr(..), ArgDescr(..), getOpt, usageInfo)
import System.Console.Haskeline (InputT, Settings(..), noCompletion, runInputT, getInputLine, outputStrLn)
import System.Directory (getAppUserDataDirectory)

import GF.Compile (batchCompile)
import GF.Compile.Compute.Concrete2
  ( Choice(..)
  , ChoiceMap
  , Globals(Gl)
  , OptionInfo(..)
  , stdPredef
  , unit
  , eval
  , cleanOptions
  , runEvalMWithOpts
  , value2termM
  , ppValue
  )
import GF.Compile.Rename (renameSourceTerm)
import GF.Compile.TypeCheck.ConcreteNew (inferLType)
import GF.Data.ErrM (Err(..))
import GF.Data.Utilities (maybeAt, orLeft)
import GF.Grammar.Grammar
  ( Grammar
  , mGrammar
  , Info
  , Module
  , ModuleName
  , ModuleInfo(..)
  , ModuleType(MTResource)
  , ModuleStatus(MSComplete)
  , OpenSpec(OSimple)
  , Location (NoLoc)
  , Term(Typed)
  , prependModule
  )
import GF.Grammar.Lexer (Posn(..), Lang(..), runLangP)
import GF.Grammar.Parser (pTerm)
import GF.Grammar.Printer (TermPrintQual(Unqualified), ppTerm)
import GF.Infra.CheckM (Check, runCheck)
import GF.Infra.Ident (moduleNameS)
import GF.Infra.Option (noOptions)
import GF.Infra.UseIO (justModuleName)
import GF.Text.Pretty (render)
import Debug.Trace

data ReplOpts = ReplOpts
  { lang :: Lang
  , noPrelude :: Bool
  , inputFiles :: [String]
  , evalToFlat :: Bool
  }

defaultReplOpts :: ReplOpts
defaultReplOpts = ReplOpts
  { lang = GF
  , noPrelude = False
  , inputFiles = []
  , evalToFlat = True
  }

type Errs a = Either [String] a
type ReplOptsOp = ReplOpts -> Errs ReplOpts

replOptDescrs :: [OptDescr ReplOptsOp]
replOptDescrs =
  [ Option ['h'] ["help"] (NoArg $ \o -> Left [usageInfo "gfci" replOptDescrs]) "Display help."
  , Option [] ["no-prelude"] (flag $ \o -> o { noPrelude = True }) "Don't load the prelude."
  , Option [] ["lang"] (ReqArg (\s o -> case s of
                                          "gf"   -> Right (o { lang = GF })
                                          "bnfc" -> Right (o { lang = BNFC })
                                          "nlg"  -> Right (o { lang = NLG })
                                          _      -> Left ["Unknown language variant: " ++ s])
                               "{gf,bnfc,nlg}")
           "Set the active language variant."
  , Option [] ["no-flat"] (flag $ \o -> o { evalToFlat = False }) "Do not evaluate to flat form."
  ]
  where
    flag f = NoArg $ \o -> pure (f o)

getReplOpts :: [String] -> Errs ReplOpts
getReplOpts args = case errs of
  [] -> foldM (&) defaultReplOpts flags <&> \o -> o { inputFiles = inputFiles }
  _  -> Left errs
  where
    (flags, inputFiles, errs) = getOpt RequireOrder replOptDescrs args

execCheck :: MonadIO m => Check a -> (a -> InputT m b) -> InputT m (Maybe b)
execCheck c k = case runCheck c of
  Ok (a, warn) -> do
    unless (null warn) $ outputStrLn warn
    Just <$> k a
  Bad err -> do
    outputStrLn err
    return Nothing

replModNameStr :: String
replModNameStr = "<repl>"

replModName :: ModuleName
replModName = moduleNameS replModNameStr

parseThen :: MonadIO m => Lang -> Grammar -> String -> (Term -> InputT m b) -> InputT m (Maybe b)
parseThen l g s k = case runLangP l pTerm (BS.pack s) of
  Left (Pn l c, err) -> do
    outputStrLn $ err ++ " (" ++ show l ++ ":" ++ show c ++ ")"
    return Nothing
  Right t -> execCheck (renameSourceTerm g replModName t) $ \t -> k t

data ResultState = ResultState
  { srsResult  :: Term
  , srsChoices :: ChoiceMap
  , srsOptInfo :: [OptionInfo]
  , srsOpts    :: ChoiceMap
  }
data OptionState = OptionState
  { osTerm     :: Term
  , osResults  :: [ResultState]
  , osSelected :: Maybe ResultState
  }
newtype ReplState = ReplState
  { rsOpts :: Maybe OptionState
  }

initState :: ReplState
initState = ReplState Nothing

runRepl' :: ReplOpts -> Globals -> IO ()
runRepl' opts@ReplOpts { lang, evalToFlat } gl@(Gl g _) = do
  historyFile <- getAppUserDataDirectory "gfci_history"
  runInputT (Settings noCompletion (Just historyFile) True) (repl initState) -- TODO tab completion
  where
    repl st = do
      getInputLine "gfci> " >>= \case
        Nothing -> repl st
        Just (':' : l) -> let (cmd, arg) = break isSpace l in command st cmd (dropWhile isSpace arg)
        Just code -> evalPrintLoop st code

    nlrepl st = outputStrLn "" >> repl st

    -- Show help text
    command st "?" arg = do
      outputStrLn ":? -- show help text."
      outputStrLn ":t <expr> -- show the inferred type of <expr>."
      outputStrLn ":r -- show the results of the last eval."
      outputStrLn ":s <index> -- select the result at <index>."
      outputStrLn ":c -- show the current selected result."
      outputStrLn ":o <choice> <value> -- set option <choice> to <value>."
      outputStrLn ":q -- quit the REPL."
      nlrepl st

    -- Show the inferred type of an expression
    command st "t" arg = do
      parseThen lang g arg $ \main ->
        execCheck (inferLType gl main) $ \res ->
          forM_ res $ \(t, ty) ->
            let t' = case t of
                       Typed _ _ -> t
                       t         -> Typed t ty
            in outputStrLn $ render (ppTerm Unqualified 0 t')
      nlrepl st
    
    -- Show the results of the last evaluated expression
    command st "r" arg = do
      case rsOpts st of
        Nothing -> do
          outputStrLn "No results to show!"
        Just (OptionState t rs _) -> do
          outputStrLn $ "> " ++ render (ppTerm Unqualified 0 t)
          outputResults rs
      nlrepl st

    -- Select a result to "focus" by its index
    command st "s" arg = do
      let e = do (OptionState t rs _) <- orLeft "No results to select!" $ rsOpts st
                 s <- orLeft "Could not parse result index!" $ readMaybe arg
                 (ResultState r cs ois os) <- orLeft "Result index out of bounds!" $ rs `maybeAt` (s - 1)
                 return (t, rs, r, cs, ois, os)
      case e of
        Left err -> do
          outputStrLn err
          nlrepl st
        Right (t, rs, r, cs, ois, os) -> do
          outputStrLn $ render (ppTerm Unqualified 0 r)
          outputOptions ois os
          nlrepl (st { rsOpts = Just (OptionState t rs (Just (ResultState r cs ois os))) })

    -- Show the current selected result
    command st "c" arg = do
      let e = do (OptionState t _ sel) <- orLeft "No results to select!" $ rsOpts st
                 (ResultState r _ ois os) <- orLeft "No result selected!" sel
                 return (t, r, ois, os)
      case e of
        Left err -> outputStrLn err
        Right (t, r, ois, os) -> do
          outputStrLn $ "> " ++ render (ppTerm Unqualified 0 t)
          outputStrLn $ render (ppTerm Unqualified 0 r)
          outputOptions ois os
      nlrepl st

    -- Set an option for the selected result
    command st "o" arg = do
      let e = do (OptionState t _ sel) <- orLeft "No results to select!" $ rsOpts st
                 (ResultState _ cs ois os) <- orLeft "No result selected!" sel
                 (c, i) <- case words arg of
                             [argc, argi] -> do
                               c <- orLeft "Could not parse option choice!" $ readMaybe argc
                               i <- orLeft "Could not parse option value!" $ readMaybe argi
                               return (c, i)
                             _ -> Left "Expected two arguments!"
                 when (i < 1) $ Left "Option value must be positive!"
                 oi <- orLeft "No such option!" $ find (\oi -> unchoice (optChoice oi) == c) ois
                 when (i > length (optChoices oi)) $ Left "Option value out of bounds!"
                 return (t, cs, ois, os, c, i)
      case e of
        Left err -> do
          outputStrLn err
          nlrepl st
        Right (t, cs, ois, os, c, i) -> do
          let os' = Map.insert (Choice c) (i - 1) os
          nfs <- execCheck (doEval st t (Map.union os' cs)) pure
          case nfs of
            Nothing -> nlrepl st
            Just [] -> do
              outputStrLn "No results!"
              nlrepl st
            Just [(r, cs, ois')] -> do
              outputStrLn $ render (ppTerm Unqualified 0 r)
              let os'' = cleanOptions ois' os'
              outputOptions ois' os''
              let rst = ResultState r (Map.difference cs os') ois' os''
              nlrepl (st { rsOpts = Just (OptionState t [rst] (Just rst)) })
            Just rs -> do
              let rsts = rs <&> \(r, cs, ois') ->
                           ResultState r (Map.difference cs os') ois' (cleanOptions ois' os')
              outputResults rsts
              nlrepl (st { rsOpts = Just (OptionState t rsts Nothing) })
    
    -- Quit the REPL
    command _ "q" _ = outputStrLn "Bye!"

    command st cmd _ = do
      outputStrLn $ "Unknown REPL command \"" ++ cmd ++ "\"! Use :? for help."
      nlrepl st

    evalPrintLoop st code = do -- TODO bindings
      c <- parseThen lang g code $ \main -> do
        rsts <- execCheck (doEval st main Map.empty) $ \nfs -> do
          if null nfs then do
            outputStrLn "No results!"
            return Nothing
          else do
            let rsts = nfs <&> \(r, cs, ois) -> ResultState r cs ois Map.empty
            outputResults rsts
            return $ Just rsts
        return $ (main,) <$> join rsts
      case join c of
        Just (t, rs) -> nlrepl (ReplState (Just (OptionState t rs Nothing)))
        Nothing      -> nlrepl st

    doEval st t opts = inferLType gl t >>= \case
      []          -> fail $ "No result while checking type: " ++ render (ppTerm Unqualified 0 t)
      ((t', _):_) -> runEvalMWithOpts gl opts (value2termM evalToFlat [] (eval gl [] unit t' []))
    
    outputResults rs =
      forM_ (zip [1..] rs) $ \(i, ResultState r _ opts _) ->
        outputStrLn $ show i ++ (if null opts then ". " else "*. ") ++ render (ppTerm Unqualified 0 r)
    
    outputOptions ois os =
      forM_ ois $ \(OptionInfo c n ls) -> do
        outputStrLn ""
        outputStrLn $ show (unchoice c) ++ ") " ++ render (ppValue Unqualified 0 n)
        let sel = fromMaybe 0 (Map.lookup c os) + 1
        forM_ (zip [1..] ls) $ \(i, l) ->
          outputStrLn $ (if i == sel then "->" else "  ") ++ show i ++ ". " ++ render (ppValue Unqualified 0 l)

runRepl :: ReplOpts -> IO ()
runRepl opts@ReplOpts { noPrelude, inputFiles } = do
  -- TODO accept an ngf grammar
  let toLoad = if noPrelude then inputFiles else "prelude/Predef.gfo" : inputFiles
  (g0, opens) <- case toLoad of
    [] -> pure (mGrammar [], [])
    _ -> do
      (_, g0) <- batchCompile noOptions Nothing toLoad
      pure (g0, OSimple . moduleNameS . justModuleName <$> toLoad)
  let
    modInfo = ModInfo
      { mtype   = MTResource
      , mstatus = MSComplete
      , mflags  = noOptions
      , mextend = []
      , mwith   = Nothing
      , mopens  = opens
      , mexdeps = []
      , msrc    = replModNameStr
      , mseqs   = Nothing
      , jments  = Map.empty
      }
    g = Gl (prependModule g0 (replModName, modInfo)) (if noPrelude then Map.empty else stdPredef g)
  runRepl' opts g
