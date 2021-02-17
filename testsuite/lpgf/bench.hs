{-# LANGUAGE BangPatterns #-}
module Main where

import qualified LPGF
import LPGF (LPGF)
import qualified PGF
import PGF (PGF)
import GF (compileToPGF, compileToLPGF, writePGF, writeLPGF)
import GF.Support (Options, Flags (..), Verbosity (..), noOptions, addOptions, modifyFlags)

import qualified Data.List as L
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Directory (listDirectory, getFileSize)
import System.FilePath ((</>), takeBaseName, takeExtension)
import Text.Printf (printf)

dir :: FilePath
dir = "testsuite" </> "lpgf"

grammarName :: String
grammarName = "Foods"

treesFile :: String
treesFile = "Foods-all.trees"

options :: Options
options = addOptions (modifyFlags (\f -> f{optVerbosity=Quiet})) noOptions

main :: IO ()
main = do
  -- Collect concrete modules
  mods <- map (dir </>)
        . filter (\p -> grammarName `L.isPrefixOf` takeBaseName p && takeExtension p == ".gf")
        <$> listDirectory dir

  -- Compile
  (pathPGF, pgf) <- time "compile PGF" (compilePGF mods)
  (pathLPGF, lpgf) <- time "compile LPGF" (compileLPGF mods)

  -- Compare filesizes
  sizePGF <- getFileSize pathPGF
  sizeLPGF <- getFileSize pathLPGF
  printf "- PGF size: %s\n" (convertSize sizePGF)
  printf "- LPGF size: %s\n" (convertSize sizeLPGF)

  -- Read trees
  lns <- lines <$> readFile (dir </> treesFile)
  let trees = map (fromJust . PGF.readExpr) lns
  printf "Read %d trees\n" (length trees)

  -- Linearise
  time "linearise PGF" (return $ length $ linPGF pgf trees)
  time "linearise LPGF" (return $ length $ linLPGF lpgf trees)

  return ()

time :: String -> IO a -> IO a
time desc io = do
  start <- getCurrentTime
  !r <- io
  end <- getCurrentTime
  printf "- %s: %s\n" desc (show (diffUTCTime end start))
  return r

-- timePure :: String -> a -> IO a
-- timePure desc val = do
--   start <- getCurrentTime
--   let !r = val
--   end <- getCurrentTime
--   printf "- %s: %s\n" desc (show (diffUTCTime end start))
--   return r

compilePGF :: [FilePath] -> IO (FilePath, PGF)
compilePGF mods = do
  pgf <- compileToPGF options mods
  files <- writePGF options pgf
  return (head files, pgf)

compileLPGF :: [FilePath] -> IO (FilePath, LPGF)
compileLPGF mods = do
  lpgf <- compileToLPGF options mods
  file <- writeLPGF options lpgf
  return (file, lpgf)

linPGF :: PGF -> [PGF.Expr] -> [[String]]
linPGF pgf trees =
  [ map (PGF.linearize pgf lang) trees | lang <- PGF.languages pgf ]

linLPGF :: LPGF -> [PGF.Expr] -> [[Text]]
linLPGF lpgf trees =
  [ map (LPGF.linearizeConcreteText concr) trees | (_,concr) <- Map.toList (LPGF.concretes lpgf) ]

-- | Produce human readable file size
-- Adapted from https://hackage.haskell.org/package/hrfsize
convertSize :: Integer -> String
convertSize = convertSize' . fromInteger
convertSize' :: Double -> String
convertSize' size
  | size < 1024.0 = printf "%.0v bytes" size
  | size < 1024.0 ^ (2 :: Int) = printf "%.2v KiB" $ size / 1024.0
  | size < 1024.0 ^ (3 :: Int) = printf "%.2v MiB" $ size / 1024.0 ^ (2 :: Int)
  | size < 1024.0 ^ (4 :: Int) = printf "%.2v GiB" $ size / 1024.0 ^ (3 :: Int)
  | otherwise = printf "%.2v TiB" $ size / 1024.0 ^ (4 :: Int)
