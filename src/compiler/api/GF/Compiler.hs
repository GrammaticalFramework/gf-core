module GF.Compiler (mainGFC, writeGrammar, writeOutputs) where

import PGF2
import PGF2.Transactions
import GF.Compile as S(batchCompile,link,srcAbsName)
import GF.CompileInParallel as P(parallelBatchCompile)
import GF.Compile.Export
import GF.Compile.ConcreteToHaskell(concretes2haskell)
import GF.Compile.GrammarToCanonical
import GF.Compile.CFGtoPGF
import GF.Compile.GetGrammar
import GF.Grammar.BNFC
import GF.Grammar.CFG
import GF.Grammar.Grammar
import GF.Grammar.JSON(grammar2json)
import GF.Grammar.Printer(TermPrintQual(..),ppModule)

--import GF.Infra.Ident(showIdent)
import GF.Infra.UseIO
import GF.Infra.Option
import GF.Infra.CheckM
import GF.Data.ErrM
import GF.System.Directory
import GF.Text.Pretty(render,render80)

import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy as BSL
import Text.JSON (encode)
import System.FilePath
import Control.Monad(when,unless,forM_,foldM)

-- | Compile the given GF grammar files. The result is a number of @.gfo@ files
-- and, depending on the options, a @.pgf@ file. (@gf -batch@, @gf -make@)
mainGFC :: Options -> [FilePath] -> IO ()
mainGFC opts fs = do
  r <- tryIOE (case () of
                 _ | null fs -> fail $ "No input files."
                 _ | all (extensionIs ".cf")  fs -> compileCFFiles opts fs
                 _ | all (\f -> extensionIs ".gf" f || extensionIs ".gfo" f)  fs -> compileSourceFiles opts fs
                 _ | all (extensionIs ".pgf") fs -> unionPGFFiles opts fs
                 _ -> fail $ "Don't know what to do with these input files: " ++ unwords fs)
  case r of
    Ok x    -> return x
    Bad msg -> die $ if flag optVerbosity opts == Normal
                       then ('\n':msg)
                       else msg
 where
   extensionIs ext = (== ext) .  takeExtension

compileSourceFiles :: Options -> [FilePath] -> IOE ()
compileSourceFiles opts fs = 
  do cnc_gr@(cnc,gr) <- S.batchCompile opts Nothing fs
     let absname = srcAbsName gr cnc
     exportCanonical absname gr
     unless (flag optStopAfterPhase opts == Compile) $ do
       let pgfFile = outputPath opts (grammarName' opts (render absname)<.>"pgf")
       pgf <- link opts Nothing cnc_gr
       writeGrammar opts pgf
       writeOutputs opts pgf
  where
    exportCanonical absname gr =
      do when (FmtHaskell `elem` ofmts && haskellOption opts HaskellConcrete) $ do
           (res,_) <- runCheck (concretes2haskell opts absname gr)
           mapM_ writeExport res
         when (FmtCanonicalGF `elem` ofmts) $
           do createDirectoryIfMissing False "canonical"
              (gr_canon,_) <- runCheck (grammar2canonical opts absname gr)
              forM_ (modules gr_canon) $ \m@(mn,_) -> do
                writeExport ("canonical/"++render mn++".gf",render80 (ppModule Unqualified m))
         when (FmtCanonicalJson `elem` ofmts) $
           do (gr_canon,_) <- runCheck (grammar2canonical opts absname gr)
              writeExport (render absname ++ ".json", encode (grammar2json gr_canon))
         when (FmtSourceJson `elem` ofmts) $
           do writeExport (render absname ++ ".json", encode (grammar2json gr))
      where
        ofmts = flag optOutputFormats opts

    writeExport (path,s) = writing opts path $ writeUTF8File path s


-- | Create a @.pgf@ file (and possibly files in other formats, if specified
-- in the 'Options') from the output of 'parallelBatchCompile'.
-- If a @.pgf@ file by the same name already exists and it is newer than the
-- source grammar files (as indicated by the 'UTCTime' argument), it is not
-- recreated. Calls 'writeGrammar' and 'writeOutputs'.
linkGrammars opts (t_src,cnc_gr@(cnc,gr)) =
    do let abs = render (srcAbsName gr cnc)
           pgfFile = outputPath opts (grammarName' opts abs<.>"pgf")
       t_pgf <- if outputJustPGF opts
                then maybeIO $ getModificationTime pgfFile
                else return Nothing
       if t_pgf >= Just t_src
         then putIfVerb opts $ pgfFile ++ " is up-to-date."
         else do pgf <- link opts Nothing cnc_gr
                 writeGrammar opts pgf
                 writeOutputs opts pgf

compileCFFiles :: Options -> [FilePath] -> IOE ()
compileCFFiles opts fs = do
  bnfc_rules <- fmap concat $ mapM (getBNFCRules opts) fs
  let rules = bnfc2cf bnfc_rules
  startCat <- case rules of
                (Rule cat _ _ : _) -> return cat
                _                  -> fail "empty CFG"
  probs <- liftIO (maybe (return Map.empty) readProbabilitiesFromFile (flag optProbsFile opts))
  let pgf = cf2pgf opts (last fs) (mkCFG startCat Set.empty rules) probs
  unless (flag optStopAfterPhase opts == Compile) $
     do writeGrammar opts pgf
        writeOutputs opts pgf

unionPGFFiles :: Options -> [FilePath] -> IOE ()
unionPGFFiles opts fs =
    if outputJustPGF opts
    then maybe doIt checkFirst (flag optName opts)
    else doIt
  where
    checkFirst name =
      do let pgfFile = outputPath opts (name <.> "pgf")
         sourceTime <- maximum `fmap` mapM getModificationTime fs
         targetTime <- maybeIO $ getModificationTime pgfFile
         if targetTime >= Just sourceTime
           then putIfVerb opts $ pgfFile ++ " is up-to-date."
           else doIt

    doIt =
      case fs of
        []     -> return ()
        (f:fs) -> do mb_probs <- case flag optProbsFile opts of
                                   Nothing   -> return Nothing
                                   Just file -> fmap Just (readProbabilitiesFromFile file)
                     pgf <- if snd (flag optLinkTargets opts)
                              then case flag optName opts of
                                     Just name -> do let fname = maybe id (</>) (flag optOutputDir opts) (name<.>"ngf")
                                                     putStrLnE ("(Boot image "++fname++")")
                                                     exists <- doesFileExist fname
                                                     if exists
                                                       then removeFile fname
                                                       else return ()
                                                     echo (\f -> bootNGFWithProbs f mb_probs fname) f
                                     Nothing   -> do putStrLnE $ "To boot from a list of .pgf files add option -name"
                                                     echo (\f -> readPGFWithProbs f mb_probs) f
                              else echo (\f -> readPGFWithProbs f mb_probs) f
                     pgf <- foldM (\pgf -> echo (modifyPGF pgf . mergePGF)) pgf fs
                     let pgfFile = outputPath opts (grammarName opts pgf <.> "pgf")
                     if pgfFile `elem` fs
                       then putStrLnE $ "Refusing to overwrite " ++ pgfFile
                       else writeGrammar opts pgf
                     writeOutputs opts pgf

    echo read f = putPointE Normal opts ("Reading " ++ f ++ "...") (liftIO (read f))


-- | Export the PGF to the 'OutputFormat's specified in the 'Options'.
-- Calls 'exportPGF'.
writeOutputs :: Options -> PGF -> IOE ()
writeOutputs opts pgf = do
  sequence_ [writeOutput opts name str 
                 | fmt <- flag optOutputFormats opts,
                   (name,str) <- exportPGF opts fmt pgf]

-- | Write the result of compiling a grammar (e.g. with 'compileToPGF' or
-- 'link') to a @.pgf@ file.
-- A split PGF file is output if the @-split-pgf@ option is used.
writeGrammar :: Options -> PGF -> IOE ()
writeGrammar opts pgf =
  if fst (flag optLinkTargets opts)
    then do let outfile = outputPath opts (grammarName opts pgf <.> "pgf")
            writing opts outfile (writePGF outfile pgf Nothing)
    else return ()

writeOutput :: Options -> FilePath-> String -> IOE ()
writeOutput opts file str = writing opts path $ writeUTF8File path str
  where path = outputPath opts file

-- * Useful helper functions

grammarName :: Options -> PGF -> String
grammarName opts pgf = grammarName' opts (abstractName pgf)
grammarName' opts abs = fromMaybe abs (flag optName opts)

outputJustPGF opts = null (flag optOutputFormats opts)

outputPath opts file = maybe id (</>) (flag optOutputDir opts) file

writing opts path io =
    putPointE Normal opts ("Writing " ++ path ++ "...") $ liftIO io
