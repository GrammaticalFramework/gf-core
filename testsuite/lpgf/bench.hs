{-# LANGUAGE BangPatterns #-}
module Main where

import qualified LPGF
import qualified PGF
import qualified PGF2

import GF (compileToPGF, compileToLPGF, writePGF, writeLPGF)
import GF.Support (Options, Flags (..), Verbosity (..), noOptions, addOptions, modifyFlags)

import Control.Monad (when)
import qualified Data.List as L
import Data.Maybe (fromJust, isJust, isNothing)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Directory (listDirectory, getFileSize)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath ((</>), (<.>), takeBaseName, takeExtension, dropExtension)
import Text.Printf (printf)

import GHC.Stats

options :: Options
options = addOptions (modifyFlags (\f -> f{optVerbosity=Quiet})) noOptions

usage :: String
usage = "Arguments:\n\
\  compile [pgf|lpgf] FoodsEng.gf FoodsGer.gf ...\n\
\  run [pgf|pgf2|lpgf] Foods.pgf test.trees\
\"

main :: IO ()
main = do
  -- Parse command line arguments
  args <- getArgs
  let argc = length args
  when (argc < 1) (die usage)
  let (mode:_) = args
  when (mode `L.notElem` ["compile","run"]) (die usage)
  when (mode == "compile" && argc < 2) (die usage)
  when (mode == "run" && argc < 3) (die usage)
  let target = let a1 = args !! 1 in if a1 `elem` ["pgf", "pgf2", "lpgf"] then Just a1 else Nothing
  let mods = if mode == "compile" then drop (if isJust target then 2 else 1) args else []
  -- If * is supplied in module name, collect modules ourselves
  -- mods <- map (dir </>)
  --       . filter (\p -> grammarName `L.isPrefixOf` takeBaseName p && takeExtension p == ".gf")
  --       <$> listDirectory dir

  let binaryFile = if mode == "run" then Just $ args !! (if isJust target then 2 else 1) else Nothing
  let treesFile = if mode == "run" then Just $ args !! (if isJust target then 3 else 2) else Nothing

  let doPGF = isNothing target || target == Just "pgf"
  let doPGF2 = isNothing target || target == Just "pgf2"
  let doLPGF = isNothing target || target == Just "lpgf"

  -- Compilation
  when (mode == "compile") $ do
    when doPGF $ do
      putStrLn "PGF"
      (path, pgf) <- time "compile" (compilePGF mods)
      size <- getFileSize path
      printf "- size: %s  %s\n" (convertSize size) path

    when doLPGF $ do
      putStrLn "LPGF"
      (path, lpgf) <- time "compile" (compileLPGF mods)
      size <- getFileSize path
      printf "- size: %s  %s\n" (convertSize size) path

  -- Linearisation
  when (mode == "run") $ do
    -- Read trees
    lns <- lines <$> readFile (fromJust treesFile)
    let trees = map (fromJust . PGF.readExpr) lns
    let trees2 = map (fromJust . PGF2.readExpr) lns
    printf "Read %d trees\n" (length trees)

    when doPGF $ do
      putStrLn "PGF"
      pgf <- PGF.readPGF (dropExtension (fromJust binaryFile) <.> "pgf")
      time "linearise" (return $ length $ linPGF pgf trees)
      return ()

    when doPGF2 $ do
      putStrLn "PGF2"
      pgf <- PGF2.readPGF (dropExtension (fromJust binaryFile) <.> "pgf")
      time "linearise" (return $ length $ linPGF2 pgf trees2)
      return ()

    when doLPGF $ do
      putStrLn "LPGF"
      lpgf <- LPGF.readLPGF (dropExtension (fromJust binaryFile) <.> "lpgf")
      time "linearise" (return $ length $ linLPGF lpgf trees)
      return ()

  stats <- getRTSStats
  printf "Max live memory: %s\n" (convertSize (read (show (max_live_bytes stats))))
  printf "Max used memory: %s\n" (convertSize (read (show (max_mem_in_use_bytes stats))))

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

compilePGF :: [FilePath] -> IO (FilePath, PGF.PGF)
compilePGF mods = do
  pgf <- compileToPGF options mods
  files <- writePGF options pgf
  return (head files, pgf)

compileLPGF :: [FilePath] -> IO (FilePath, LPGF.LPGF)
compileLPGF mods = do
  lpgf <- compileToLPGF options mods
  file <- writeLPGF options lpgf
  return (file, lpgf)

linPGF :: PGF.PGF -> [PGF.Expr] -> [[String]]
linPGF pgf trees =
  [ map (PGF.linearize pgf lang) trees | lang <- PGF.languages pgf ]

linPGF2 :: PGF2.PGF -> [PGF2.Expr] -> [[String]]
linPGF2 pgf trees =
  [ map (PGF2.linearize concr) trees | (_, concr) <- Map.toList (PGF2.languages pgf) ]

linLPGF :: LPGF.LPGF -> [PGF.Expr] -> [[Text]]
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
