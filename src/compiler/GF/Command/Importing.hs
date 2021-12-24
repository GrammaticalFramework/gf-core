module GF.Command.Importing (importGrammar, importSource) where

import PGF2
import PGF2.Transactions

import GF.Compile
import GF.Compile.Multi (readMulti)
import GF.Compile.GetGrammar (getBNFCRules, getEBNFRules)
import GF.Grammar (SourceGrammar) -- for cc command
import GF.Grammar.BNFC
import GF.Grammar.EBNF
import GF.Grammar.CFG
import GF.Compile.CFGtoPGF
import GF.Infra.UseIO(die,tryIOE)
import GF.Infra.Option
import GF.Data.ErrM

import System.FilePath
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad(foldM)

-- import a grammar in an environment where it extends an existing grammar
importGrammar :: Maybe PGF -> Options -> [FilePath] -> IO (Maybe PGF)
importGrammar pgf0 opts  _
  | Just name <- flag optBlank opts = do
        mb_ngf_file <- if snd (flag optLinkTargets opts)
                         then do let fname = name <.> ".ngf"
                                 putStr ("(Boot image "++fname++") ")
                                 return (Just fname)
                         else do return Nothing
        pgf <- newNGF name mb_ngf_file
        putStrLn (abstractName pgf)
        return (Just pgf)
importGrammar pgf0 _    [] = return pgf0
importGrammar pgf0 opts fs
  | all (extensionIs ".cf")   fs = fmap Just $ importCF opts fs getBNFCRules bnfc2cf
  | all (extensionIs ".ebnf") fs = fmap Just $ importCF opts fs getEBNFRules ebnf2cf
  | all (extensionIs ".gfm")  fs = do
        ascss <- mapM readMulti fs
        let cs = concatMap snd ascss
        importGrammar pgf0 opts cs
  | all (\f -> extensionIs ".gf" f || extensionIs ".gfo" f) fs = do
        res <- tryIOE $ compileToPGF opts pgf0 fs
        case res of
          Ok pgf  -> return (Just pgf)
          Bad msg -> do putStrLn ('\n':'\n':msg)
                        return pgf0
  | all (extensionIs ".pgf") fs = foldM importPGF pgf0 fs
  | all (extensionIs ".ngf") fs = do
        case fs of
          [f] -> fmap Just $ readNGF f
          _   -> die $ "Only one .ngf file could be loaded at a time"
  | otherwise =  die $ "Don't know what to do with these input files: " ++ unwords fs
  where
    extensionIs ext = (== ext) .  takeExtension

importPGF :: Maybe PGF -> FilePath -> IO (Maybe PGF)
importPGF Nothing    f = fmap Just (readPGF f)
importPGF (Just pgf) f = fmap Just (modifyPGF pgf (mergePGF f))

importSource :: Options -> [FilePath] -> IO SourceGrammar
importSource opts files = fmap (snd.snd) (batchCompile opts files)

-- for different cf formats
importCF opts files get convert = impCF
  where
    impCF = do
      rules <- fmap (convert . concat) $ mapM (get opts) files
      startCat <- case rules of
                    (Rule cat _ _ : _) -> return cat
                    _                  -> fail "empty CFG"
      probs <- maybe (return Map.empty) readProbabilitiesFromFile (flag optProbsFile opts)
      let pgf = cf2pgf opts (last files) (mkCFG startCat Set.empty rules) probs
      return pgf
