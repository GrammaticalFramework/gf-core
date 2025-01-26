{-# LANGUAGE LambdaCase #-}

module GF.Compile.Repl (ReplOpts(..), defaultReplOpts, replOptDescrs, getReplOpts, runRepl, runRepl') where

import Control.Monad (unless, forM_)
import Control.Monad.IO.Class (MonadIO)
import Data.Char (isSpace)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map

import System.Console.GetOpt (getOpt, ArgOrder(RequireOrder), OptDescr(..), ArgDescr(..))
import System.Console.Haskeline (InputT, Settings(..), noCompletion, runInputT, getInputLine, outputStrLn)
import System.Directory (getAppUserDataDirectory)

import GF.Compile (batchCompile)
import GF.Compile.Compute.Concrete (Globals(Gl), normalFlatForm)
import GF.Compile.Rename (renameSourceTerm)
import GF.Compile.TypeCheck.ConcreteNew (inferLType)
import GF.Data.ErrM (Err(..))
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
  , Term
  , prependModule
  )
import GF.Grammar.Lexer (Posn(..), Lang(GF), runLangP)
import GF.Grammar.Parser (pTerm)
import GF.Grammar.Printer (TermPrintQual(Unqualified), ppTerm)
import GF.Infra.CheckM (Check, runCheck)
import GF.Infra.Ident (moduleNameS)
import GF.Infra.Option (noOptions)
import GF.Text.Pretty (render)

data ReplOpts = ReplOpts
  { noPrelude :: Bool
  , inputFiles :: [String]
  }

defaultReplOpts :: ReplOpts
defaultReplOpts = ReplOpts False []

replOptDescrs :: [OptDescr (ReplOpts -> ReplOpts)]
replOptDescrs =
  [ Option [] ["no-prelude"] (NoArg $ \o -> o { noPrelude = True }) "Don't load the prelude."
  ]

getReplOpts :: [String] -> Either [String] ReplOpts
getReplOpts args = case errs of
  [] -> Right $ (foldr ($) defaultReplOpts flags) { inputFiles = inputFiles }
  _  -> Left errs
  where
    (flags, inputFiles, errs) = getOpt RequireOrder [] args

execCheck :: MonadIO m => Check a -> (a -> InputT m ()) -> InputT m ()
execCheck c k = case runCheck c of
  Ok (a, warn) -> do
    unless (null warn) $ outputStrLn warn
    k a
  Bad err -> outputStrLn err

replModNameStr :: String
replModNameStr = "<repl>"

replModName :: ModuleName
replModName = moduleNameS replModNameStr

parseThen :: MonadIO m => Grammar -> String -> (Term -> InputT m ()) -> InputT m ()
parseThen g s k = case runLangP GF pTerm (BS.pack s) of
  Left (Pn l c, err) -> outputStrLn $ err ++ " (" ++ show l ++ ":" ++ show c ++ ")"
  Right t -> execCheck (renameSourceTerm g replModName t) $ \t -> k t

runRepl' :: Globals -> IO ()
runRepl' gl@(Gl g _) = do
  historyFile <- getAppUserDataDirectory "gfci_history"
  runInputT (Settings noCompletion (Just historyFile) True) repl -- TODO tab completion
  where
    repl = do
      getInputLine "gfci> " >>= \case
        Nothing -> repl
        Just (':' : l) -> let (cmd, arg) = break isSpace l in command cmd (dropWhile isSpace arg)
        Just code -> evalPrintLoop code

    command "t" arg = do
      parseThen g arg $ \main ->
        execCheck (inferLType gl main) $ \(_, ty) ->
          outputStrLn $ render (ppTerm Unqualified 0 ty)
      outputStrLn "" >> repl
    
    command "q" _ = outputStrLn "Bye!"

    command cmd _ = do
      outputStrLn $ "Unknown REPL command: " ++ cmd
      outputStrLn "" >> repl

    evalPrintLoop code = do -- TODO bindings
      parseThen g code $ \main ->
        execCheck (inferLType gl main >>= \(t, _) -> normalFlatForm gl t) $ \nfs ->
          forM_ (zip [1..] nfs) $ \(i, nf) ->
            outputStrLn $ show i ++ ". " ++ render (ppTerm Unqualified 0 nf)
      outputStrLn "" >> repl

runRepl :: ReplOpts -> IO ()
runRepl (ReplOpts noPrelude inputFiles) = do
  -- TODO accept an ngf grammar
  -- TODO load prelude
  (g0, opens) <- case inputFiles of
    [] -> pure (mGrammar [], [])
    _ -> do
      (_, (pModName, g0)) <- batchCompile noOptions Nothing inputFiles
      pure (g0, [OSimple pModName])
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
  runRepl' (Gl (prependModule g0 (replModName, modInfo)) Map.empty)
