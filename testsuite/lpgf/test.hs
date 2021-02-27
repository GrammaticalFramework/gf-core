module Main where

import LPGF
import PGF (showLanguage, readExpr)
import GF (compileToLPGF, writeLPGF)
import GF.Support (noOptions)

import Control.Monad (forM_)
import qualified Data.List as L
import qualified Data.Map as Map
import System.Environment (getArgs)
import System.Directory (listDirectory)
import System.FilePath ((</>), (<.>), takeDirectory, takeBaseName, takeExtension, dropExtension)
import Text.Printf (printf)

dir :: FilePath
dir = "testsuite" </> "lpgf"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      doGrammar "unittests" "Bind"
      doGrammar "unittests" "Missing"
      doGrammar "unittests" "Params1"
      doGrammar "unittests" "Params2"
      doGrammar "unittests" "Params3"
      doGrammar "unittests" "Pre"
      doGrammar "unittests" "Projection"
      doGrammar "unittests" "Tables"

      doGrammar "walking" "Walking"
      doGrammar "foods" "Foods"
      -- doGrammar "phrasebook" "Phrasebook"
    [absname] ->
      doGrammar (takeDirectory absname) (takeBaseName absname)
    absname:langs ->
      doGrammar' (takeDirectory absname) (takeBaseName absname) langs

doGrammar :: FilePath -> String -> IO ()
doGrammar path gname = doGrammar' path gname []

doGrammar' :: FilePath -> String -> [String] -> IO ()
doGrammar' path gname cncs = do
  -- Collect paths to concrete modules
  mods <- map ((dir </> path) </>)
        . filter (\p -> gname `L.isPrefixOf` takeBaseName p
                        && takeExtension p == ".gf"
                        && (null cncs || any (`L.isSuffixOf` dropExtension p) cncs)
                  )
        <$> listDirectory (dir </> path)

  -- Compile LPGF
  lpgf <- compileToLPGF noOptions mods
  pgfPath <- writeLPGF noOptions lpgf
  putStrLn ""

  -- Read back from file
  lpgf <- readLPGF pgfPath

  -- Read treebank
  gs <- groups . lines <$> readFile (dir </> path </> gname <.> "treebank")
  forM_ gs $ \grp -> do
    let ast = drop 2 $ dropWhile (/=':') $ head grp
    printf "%s: %s\n" gname ast
    let
      Just tree = readExpr ast
      -- Do some linearization
      outs =
        [ printf "%s: %s" (showLanguage lang) (linearizeConcrete concr tree)
        | (lang,concr) <- Map.toList (concretes lpgf)
        ]
    mapM_ putStrLn outs

    -- filter out missing langs from treebank
    let golds = [ g | o <- outs, g <- tail grp, takeWhile (/=':') o == takeWhile (/=':') g  ]
    if outs == golds
    then putStrLn "✅\n"
    else do
      putStrLn "❌ expected:"
      mapM_ putStrLn golds
      putStrLn ""
      error "Test failed"

-- | Group list of lines by blank lines
groups :: [String] -> [[String]]
groups [] = []
groups ss = let (a,b) = break (=="") ss in a : groups (drop 1 b)
