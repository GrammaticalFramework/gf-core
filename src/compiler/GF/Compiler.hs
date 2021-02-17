module GF.Compiler (mainGFC, linkGrammars, writePGF, writeLPGF, writeOutputs) where

import PGF
import PGF.Internal(concretes,optimizePGF,unionPGF)
import PGF.Internal(putSplitAbs,encodeFile,runPut)
import LPGF(LPGF)
import qualified LPGF
import GF.Compile as S(batchCompile,link,linkl,srcAbsName)
import GF.CompileInParallel as P(parallelBatchCompile)
import GF.Compile.Export
import GF.Compile.ConcreteToHaskell(concretes2haskell)
import GF.Compile.GrammarToCanonical--(concretes2canonical)
import GF.Compile.CFGtoPGF
import GF.Compile.GetGrammar
import GF.Grammar.BNFC
import GF.Grammar.CFG hiding (Grammar)
import GF.Grammar.Grammar (Grammar, ModuleName)

--import GF.Infra.Ident(showIdent)
import GF.Infra.UseIO
import GF.Infra.Option
import GF.Data.ErrM
import GF.System.Directory
import GF.Text.Pretty(render,render80)

import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Time(UTCTime)
import qualified Data.ByteString.Lazy as BSL
import GF.Grammar.CanonicalJSON (encodeJSON)
import System.FilePath
import Control.Monad(when,unless,forM,void)

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
    do output <- batchCompile opts fs
       exportCanonical output
       unless (flag optStopAfterPhase opts == Compile) $
           linkGrammars opts output
  where
    batchCompile = maybe batchCompile' parallelBatchCompile (flag optJobs opts)
    batchCompile' opts fs = do (t,cnc_gr) <- S.batchCompile opts fs
                               return (t,[cnc_gr])

    exportCanonical (_time, canonical) =
      do when (FmtHaskell `elem` ofmts && haskellOption opts HaskellConcrete) $
           mapM_ cnc2haskell canonical
         when (FmtCanonicalGF `elem` ofmts) $
           do createDirectoryIfMissing False "canonical"
              mapM_ abs2canonical canonical
              mapM_ cnc2canonical canonical
         when (FmtCanonicalJson `elem` ofmts) $ mapM_ grammar2json canonical
      where
        ofmts = flag optOutputFormats opts

    cnc2haskell (cnc,gr) =
      do mapM_ writeExport $ concretes2haskell opts (srcAbsName gr cnc) gr

    abs2canonical (cnc,gr) =
        writeExport ("canonical/"++render absname++".gf",render80 canAbs)
      where
        absname = srcAbsName gr cnc
        canAbs = abstract2canonical absname gr

    cnc2canonical (cnc,gr) =
      mapM_ (writeExport.fmap render80) $
            concretes2canonical opts (srcAbsName gr cnc) gr

    grammar2json (cnc,gr) = encodeJSON (render absname ++ ".json") gr_canon
      where absname = srcAbsName gr cnc
            gr_canon = grammar2canonical opts absname gr

    writeExport (path,s) = writing opts path $ writeUTF8File path s


-- | Create a @.pgf@ file (and possibly files in other formats, if specified
-- in the 'Options') from the output of 'parallelBatchCompile'.
-- If a @.pgf@ file by the same name already exists and it is newer than the
-- source grammar files (as indicated by the 'UTCTime' argument), it is not
-- recreated. Calls 'writePGF' and 'writeOutputs'.
linkGrammars :: Options -> (UTCTime,[(ModuleName, Grammar)]) -> IOE ()
linkGrammars opts (_,cnc_grs) | FmtLPGF `elem` flag optOutputFormats opts = do
  lpgf <- linkl opts (head cnc_grs)
  void $ writeLPGF opts lpgf
linkGrammars opts (t_src,~cnc_grs@(~(cnc,gr):_)) =
    do let abs = render (srcAbsName gr cnc)
           pgfFile = outputPath opts (grammarName' opts abs<.>"pgf")
       t_pgf <- if outputJustPGF opts
                then maybeIO $ getModificationTime pgfFile
                else return Nothing
       if t_pgf >= Just t_src
         then putIfVerb opts $ pgfFile ++ " is up-to-date."
         else do pgfs <- mapM (link opts) cnc_grs
                 let pgf0 = foldl1 unionPGF pgfs
                 probs <- maybe (return . defaultProbabilities) readProbabilitiesFromFile (flag optProbsFile opts) pgf0
                 let pgf = setProbabilities probs pgf0
                 writePGF opts pgf
                 writeOutputs opts pgf

compileCFFiles :: Options -> [FilePath] -> IOE ()
compileCFFiles opts fs = do
  bnfc_rules <- fmap concat $ mapM (getBNFCRules opts) fs
  let rules = bnfc2cf bnfc_rules
  startCat <- case rules of
                (Rule cat _ _ : _) -> return cat
                _                  -> fail "empty CFG"
  let pgf = cf2pgf (last fs) (mkCFG startCat Set.empty rules)
  unless (flag optStopAfterPhase opts == Compile) $
     do probs <- liftIO (maybe (return . defaultProbabilities) readProbabilitiesFromFile (flag optProbsFile opts) pgf)
        let pgf' = setProbabilities probs $ if flag optOptimizePGF opts then optimizePGF pgf else pgf
        writePGF opts pgf'
        writeOutputs opts pgf'

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
      do pgfs <- mapM readPGFVerbose fs
         let pgf0 = foldl1 unionPGF pgfs
             pgf1 = if flag optOptimizePGF opts then optimizePGF pgf0 else pgf0
         probs <- liftIO (maybe (return . defaultProbabilities) readProbabilitiesFromFile (flag optProbsFile opts) pgf1)
         let pgf  = setProbabilities probs pgf1
             pgfFile = outputPath opts (grammarName opts pgf <.> "pgf")
         if pgfFile `elem` fs
           then putStrLnE $ "Refusing to overwrite " ++ pgfFile
           else void $ writePGF opts pgf
         writeOutputs opts pgf

    readPGFVerbose f =
        putPointE Normal opts ("Reading " ++ f ++ "...") $ liftIO $ readPGF f

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
writePGF :: Options -> PGF -> IOE [FilePath]
writePGF opts pgf =
    if flag optSplitPGF opts then writeSplitPGF else writeNormalPGF
  where
    writeNormalPGF =
       do let outfile = outputPath opts (grammarName opts pgf <.> "pgf")
          writing opts outfile $ encodeFile outfile pgf
          return [outfile]

    writeSplitPGF =
      do let outfile = outputPath opts (grammarName opts pgf <.> "pgf")
         writing opts outfile $ BSL.writeFile outfile (runPut (putSplitAbs pgf))
                                --encodeFile_ outfile (putSplitAbs pgf)
         outfiles <- forM (Map.toList (concretes pgf)) $ \cnc -> do
           let outfile = outputPath opts (showCId (fst cnc) <.> "pgf_c")
           writing opts outfile $ encodeFile outfile cnc
           return outfile

         return (outfile:outfiles)

writeLPGF :: Options -> LPGF -> IOE FilePath
writeLPGF opts lpgf = do
  let
    grammarName = fromMaybe (showCId (LPGF.abstractName lpgf)) (flag optName opts)
    outfile = outputPath opts (grammarName <.> "lpgf")
  writing opts outfile $ liftIO $ LPGF.encodeFile outfile lpgf
  return outfile

writeOutput :: Options -> FilePath-> String -> IOE FilePath
writeOutput opts file str = do
  let outfile = outputPath opts file
  writing opts outfile $ writeUTF8File outfile str
  return outfile

-- * Useful helper functions

grammarName :: Options -> PGF -> String
grammarName opts pgf = grammarName' opts (showCId (abstractName pgf))
grammarName' opts abs = fromMaybe abs (flag optName opts)

outputJustPGF opts = null (flag optOutputFormats opts) && not (flag optSplitPGF opts)

outputPath opts file = maybe id (</>) (flag optOutputDir opts) file

writing opts path io =
    putPointE Normal opts ("Writing " ++ path ++ "...") $ liftIO io
