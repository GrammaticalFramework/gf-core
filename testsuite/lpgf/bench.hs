{-# LANGUAGE BangPatterns #-}
module Main where

import qualified LPGF
import qualified PGF
import qualified PGF2

import GF (compileToPGF, compileToLPGF, writePGF, writeLPGF)
import GF.Support (Options, Flags (..), Verbosity (..), noOptions, addOptions, modifyFlags)

import Control.Monad (when)
import qualified Data.List as L
import Data.Maybe (fromJust, isNothing)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Directory (listDirectory, getFileSize)
import System.Environment (lookupEnv)
import System.FilePath ((</>), (<.>), takeBaseName, takeExtension)
import Text.Printf (printf)

import GHC.Stats

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
  printf "Found %d modules\n" (length mods)

  -- Read trees
  lns <- lines <$> readFile (dir </> treesFile)
  let trees = map (fromJust . PGF.readExpr) lns
  let trees2 = map (fromJust . PGF2.readExpr) lns
  printf "Read %d trees\n" (length trees)

  only <- lookupEnv "ONLY"
  let doPGF = isNothing only || only == Just "PGF"
  let doPGF2 = isNothing only || only == Just "PGF2"
  let doLPGF = isNothing only || only == Just "LPGF"

  when doPGF $ do
    putStrLn "PGF"
    (path, pgf) <- time "compile" (compilePGF mods)
    size <- getFileSize path
    printf "- size: %s\n" (convertSize size)
    time "linearise" (return $ length $ linPGF pgf trees)
    return ()

  when doPGF2 $ do
    putStrLn "PGF2"
    pgf <- PGF2.readPGF (grammarName <.> "pgf") -- might fail!
    time "linearise" (return $ length $ linPGF2 pgf trees2)
    return ()

  when doLPGF $ do
    putStrLn "LPGF"
    (path, lpgf) <- time "compile" (compileLPGF mods)
    size <- getFileSize path
    printf "- size: %s\n" (convertSize size)
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
