import LPGF
import PGF (showLanguage, readExpr)
import GF (compileToLPGF, writeLPGF)
import GF.Support (noOptions)

import Control.Monad (forM_)
import qualified Data.List as L
import qualified Data.Map as Map
import Text.Printf (printf)
import System.Directory (listDirectory)
import System.FilePath ((</>), (<.>), takeBaseName, takeExtension, dropExtension)

dir :: FilePath
dir = "testsuite" </> "lpgf"

main :: IO ()
main = do
  doGrammar "Bind"
  doGrammar "Tables"
  doGrammar "Params"
  doGrammar "Walking"
  doGrammar "Foods"
  -- doGrammar' "Foods" ["Fre"]

doGrammar :: String -> IO ()
doGrammar gname = doGrammar' gname []

doGrammar' :: String -> [String] -> IO ()
doGrammar' gname cncs = do
  -- Collect concrete modules
  mods <- map (dir </>)
        . filter (\p -> gname `L.isPrefixOf` takeBaseName p
                        && takeExtension p == ".gf"
                        && null cncs || any (`L.isSuffixOf` dropExtension p) cncs
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
      langs =
        [ printf "%s: %s" (showLanguage lang) (linearizeConcr concr tree)
        | (lang,concr) <- Map.toList (concretes lpgf)
        ]
    mapM_ putStrLn langs
    if langs == tail grp
    then putStrLn "✅\n"
    else do
      putStrLn "❌ expected:"
      mapM_ putStrLn (tail grp)
      putStrLn ""
      error "Test failed"

-- | Group list of lines by blank lines
groups :: [String] -> [[String]]
groups [] = []
groups ss = let (a,b) = break (=="") ss in a : groups (drop 1 b)
