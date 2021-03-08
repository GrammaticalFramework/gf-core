{-# LANGUAGE LambdaCase #-}

module Main where

import LPGF
import PGF (showLanguage, readExpr)
import GF (compileToLPGF, writeLPGF)
import GF.Support (noOptions)

import Control.Monad (forM, forM_, when)
import Data.Either (isLeft)
import qualified Data.List as L
import qualified Data.Map as Map
import System.Console.ANSI
import System.Environment (getArgs)
import System.Exit (exitFailure)
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
      doGrammar "unittests" "Params4"
      doGrammar "unittests" "Params5"
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
  results <- forM gs $ \grp -> do
    let ast = drop 2 $ dropWhile (/=':') $ head grp
    let Just tree = readExpr ast

    -- Linearization into all languages
    outs <- forM (Map.toList (concretes lpgf)) $ \(lang,concr) -> do
      e <- try $ linearizeConcrete concr tree
      return $ case e of
        Right s -> printf "%s: %s" (showLanguage lang) s
        Left  e -> printf "%s: ERROR: %s" (showLanguage lang) e

    -- filter out missing langs from treebank
    let golds = [ g | o <- outs, g <- tail grp, takeWhile (/=':') o == takeWhile (/=':') g  ]

    -- rs :: [Maybe (Text,Text)] out,gold
    rs <- forM (zip outs golds) $ \(out,gold) ->
      if out == gold
      then return $ Right out -- pass
      else return $ Left (out,gold) -- fail

    -- Output if any failed
    when (any isLeft rs) $ do
      printf "- %s: %s\n" gname ast
      forM_ rs $ \case
        Right out -> do
          green "✓"
          printf " %s\n" out
          putStrLn out
        Left (out,gold) -> do
          red $ printf "✗ %s\n" out
          yellow $ printf "→ %s\n" gold
      putStrLn ""

    return $ map (either (const False) (const True)) rs

  let trees = length results
  let langs = length (head results)
  let total = length (concat results)
  let (ts,fs) = L.partition id (concat results)
  let passed = length ts
  let failed = length fs

  if failed == 0
  then do
    green $ printf "Passed %d" passed
    printf " | Failed %d" failed
  else do
    printf "Passed %d | " passed
    red $ printf "Failed %d" failed

  printf " | Total %d lins (%d trees, %d languages)\n" total trees langs

  when (failed > 0) exitFailure

-- | Group list of lines by blank lines
groups :: [String] -> [[String]]
groups [] = []
groups ss = let (a,b) = break (=="") ss in a : groups (drop 1 b)

green s = do
  setSGR [SetColor Foreground Vivid Green]
  putStr s
  setSGR [Reset]

yellow s = do
  setSGR [SetColor Foreground Dull Yellow]
  putStr s
  setSGR [Reset]

red s = do
  setSGR [SetColor Foreground Dull Red]
  putStr s
  setSGR [Reset]
