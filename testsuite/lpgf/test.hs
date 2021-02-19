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
import System.FilePath ((</>), (<.>), takeBaseName, takeExtension, dropExtension)
import Text.Printf (printf)

dir :: FilePath
dir = "testsuite" </> "lpgf"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      doGrammar "Bind"
      doGrammar "Tables"
      doGrammar "Params"
      doGrammar "Pre"
      doGrammar "Projection"
      doGrammar "Walking"
      doGrammar "Foods"
    [absname] ->
      doGrammar absname
    absname:langs ->
      doGrammar' absname langs

doGrammar :: String -> IO ()
doGrammar gname = doGrammar' gname []

doGrammar' :: String -> [String] -> IO ()
doGrammar' gname cncs = do
  -- Collect paths to concrete modules
  mods <- map (dir </>)
        . filter (\p -> gname `L.isPrefixOf` takeBaseName p
                        && takeExtension p == ".gf"
                        && (null cncs || any (`L.isSuffixOf` dropExtension p) cncs)
                  )
        <$> listDirectory dir

  -- Compile LPGF
  lpgf <- compileToLPGF noOptions mods
  writeLPGF noOptions lpgf
  putStrLn ""

  -- Read back from file
  lpgf <- readLPGF $ gname ++ ".lpgf"

  -- Read treebank
  gs <- groups . lines <$> readFile (dir </> gname <.> "treebank")
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
