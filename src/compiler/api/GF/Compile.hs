module GF.Compile (compileToPGF, link, batchCompile, srcAbsName) where

import GF.Compile.GeneratePMCFG(generatePMCFG)
import GF.Compile.GrammarToPGF(grammar2PGF)
import GF.Compile.ReadFiles(ModEnv,getOptionsFromFile,getAllFiles,
                            importsOfModule)
import GF.CompileOne(compileOne)

import GF.Grammar.Grammar(Grammar,emptyGrammar,modules,mGrammar,
                          abstractOfConcrete,prependModule,ModuleInfo(..))

import GF.Infra.CheckM
import GF.Infra.Ident(ModuleName,moduleNameS)--,showIdent
import GF.Infra.Option
import GF.Infra.UseIO(IOE,FullPath,liftIO,getLibraryDirectory,putIfVerb,
                      justModuleName,extendPathEnv,putStrE,putPointE,warnOut)
import GF.Data.Operations(raise,(+++),err)

import Control.Monad(foldM,when,(<=<))
import GF.System.Directory(getCurrentDirectory,doesFileExist,getModificationTime)
import System.FilePath((</>),isRelative,dropFileName)
import qualified Data.Map as Map(empty,singleton,insert,elems)
import Data.List(nub)
import Data.Time(UTCTime)
import GF.Text.Pretty(render,($$),(<+>),nest)

import PGF2(PGF,abstractName,pgfFilePath,readProbabilitiesFromFile)

-- | Compiles a number of source files and builds a 'PGF' structure for them.
-- This is a composition of 'link' and 'batchCompile'.
compileToPGF :: Options -> Maybe PGF -> [FilePath] -> IOE PGF
compileToPGF opts mb_pgf fs = link opts mb_pgf =<< batchCompile opts mb_pgf fs

-- | Link a grammar into a 'PGF' that can be used to 'PGF.linearize' and
-- 'PGF.parse' with the "PGF" run-time system.
link :: Options -> Maybe PGF -> (ModuleName,Grammar) -> IOE PGF
link opts mb_pgf (cnc,gr) =
  putPointE Normal opts "linking ... " $ do
    let abs = srcAbsName gr cnc

    -- if a module was compiled with no-pmcfg then we generate now
    cwd <- getCurrentDirectory
    (gr',warnings) <- runCheck' opts (fmap mGrammar $ mapM (generatePMCFG opts cwd gr) (modules gr))
    warnOut opts warnings

    probs <- liftIO (maybe (return Map.empty) readProbabilitiesFromFile (flag optProbsFile opts))
    pgf <- grammar2PGF opts mb_pgf gr' abs probs
    when (verbAtLeast opts Normal) $ putStrE "OK"
    return pgf

-- | Returns the name of the abstract syntax corresponding to the named concrete syntax
srcAbsName gr cnc = err (const cnc) id $ abstractOfConcrete gr cnc

-- | Compile the given grammar files and everything they depend on.
-- Compiled modules are stored in @.gfo@ files (unless the @-tags@ option is
-- used, in which case tags files are produced instead).
-- Existing @.gfo@ files are reused if they are up-to-date
-- (unless the option @-src@ aka @-force-recomp@ is used).
batchCompile :: Options -> Maybe PGF -> [FilePath] -> IOE (ModuleName,Grammar)
batchCompile opts mb_pgf files = do
  menv <- emptyCompileEnv mb_pgf
  (gr,menv) <- foldM (compileModule opts) menv files
  let cnc = moduleNameS (justModuleName (last files))
  return (cnc,gr)

-- | compile with one module as starting point
-- command-line options override options (marked by --#) in the file
-- As for path: if it is read from file, the file path is prepended to each name.
-- If from command line, it is used as it is.
compileModule :: Options -- ^ Options from program command line and shell command.
              -> CompileEnv -> FilePath -> IOE CompileEnv
compileModule opts1 env@(_,rfs) file =
  do file <- getRealFile file
     opts0 <- getOptionsFromFile file
     let curr_dir = dropFileName file
     lib_dir  <- getLibraryDirectory (addOptions opts0 opts1)
     let opts = addOptions (fixRelativeLibPaths curr_dir lib_dir opts0) opts1
     ps0 <- extendPathEnv opts
     let ps = nub (curr_dir : ps0)
     putIfVerb opts $ "module search path:" +++ show ps ----
     files <- getAllFiles opts ps rfs file
     putIfVerb opts $ "files to read:" +++ show files ----
     let names = map justModuleName files
     putIfVerb opts $ "modules to include:" +++ show names ----
     foldM (compileOne' opts) env files
  where
    getRealFile file = do
      exists <- doesFileExist file
      if exists
        then return file
        else if isRelative file
               then do lib_dir <- getLibraryDirectory opts1
                       let file1 = lib_dir </> file
                       exists <- doesFileExist file1
                       if exists
                         then return file1
                         else raise (render ("None of these files exists:" $$ nest 2 (file $$ file1)))
               else raise (render ("File" <+> file <+> "does not exist."))

compileOne' :: Options -> CompileEnv -> FullPath -> IOE CompileEnv
compileOne' opts env@(gr,_) = extendCompileEnv env <=< compileOne opts gr

-- auxiliaries

-- | The environment
type CompileEnv = (Grammar,ModEnv)

emptyCompileEnv :: Maybe PGF -> IOE CompileEnv
emptyCompileEnv mb_pgf = do
  case mb_pgf of
    Just pgf -> do let abs_name = abstractName pgf
                   env <- case pgfFilePath pgf of
                            Just fpath -> do t <- getModificationTime fpath
                                             return (Map.singleton abs_name (fpath,t,[]))
                            Nothing    -> return Map.empty
                   return ( prependModule emptyGrammar (moduleNameS abs_name, ModPGF pgf)
                          , env
                          )
    Nothing  -> return (emptyGrammar,Map.empty)


extendCompileEnv (gr,menv) (mfile,mo) =
  do menv2 <- case mfile of
                Just file ->
                  do let (mod,imps) = importsOfModule mo
                     t <- getModificationTime file
                     return $ Map.insert mod (file,t,imps) menv
                _ -> return menv
     return (prependModule gr mo,menv2)
