import Distribution.System(Platform(..),OS(..))
import Distribution.Simple(defaultMainWithHooks,UserHooks(..),simpleUserHooks)
import Distribution.Simple.LocalBuildInfo(LocalBuildInfo(..),absoluteInstallDirs,datadir)
import Distribution.Simple.Setup(BuildFlags(..),Flag(..),InstallFlags(..),CopyDest(..),CopyFlags(..),SDistFlags(..))
import Distribution.PackageDescription(PackageDescription(..),emptyHookedBuildInfo)
import Distribution.Simple.BuildPaths(exeExtension)
import System.Directory
import System.FilePath((</>),(<.>))
import System.Process
import Control.Monad(forM_,unless)
import Control.Exception(bracket_)
import Data.Char(isSpace)

import WebSetup

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { preConf   = gfPreConf
  , preBuild  = gfPreBuild
  , postBuild = gfPostBuild
  , preInst   = gfPreInst
  , postInst  = gfPostInst
  , postCopy  = gfPostCopy
  }
  where
    gfPreConf args flags = do
      pkgs <- fmap (map (dropWhile isSpace) . tail . lines)
                   (readProcess "ghc-pkg" ["list"] "")
      forM_ dependencies $ \pkg -> do
        let name = takeWhile (/='/') (drop 36 pkg)
        unless (name `elem` pkgs) $ do
          let fname = name <.> ".tar.gz"
          callProcess "wget" [pkg,"-O",fname]
          callProcess "tar"  ["-xzf",fname]
          removeFile fname
          bracket_ (setCurrentDirectory name) (setCurrentDirectory ".." >> removeDirectoryRecursive name) $ do
            exists <- doesFileExist "Setup.hs"
            unless exists $ do
              writeFile "Setup.hs" (unlines [
                  "import Distribution.Simple",
                  "main = defaultMain"
                ])
            let to_descr = reverse .
                           (++) (reverse ".cabal") . 
                           drop 1 . 
                           dropWhile (/='-') . 
                           reverse
            callProcess "wget"   [to_descr pkg, "-O", to_descr name]
            callProcess "runghc" ["Setup.hs","configure"]
            callProcess "runghc" ["Setup.hs","build"]
            callProcess "sudo" ["runghc","Setup.hs","install"]
          
      preConf simpleUserHooks args flags

    gfPreBuild args = gfPre args . buildDistPref
    gfPreInst  args = gfPre args . installDistPref

    gfPre args distFlag = do
      return emptyHookedBuildInfo

    gfPostBuild args flags pkg lbi = do
      let gf = default_gf lbi
      buildWeb gf flags (pkg,lbi)

    gfPostInst args flags pkg lbi = do
      installWeb (pkg,lbi)

    gfPostCopy args flags  pkg lbi = do
      copyWeb flags (pkg,lbi)

    -- `cabal sdist` will not make a proper dist archive, for that see `make sdist`
    -- However this function should exit quietly to allow building gf in sandbox
    gfSDist pkg lbi hooks flags = do
      return ()

dependencies = [
  "https://hackage.haskell.org/package/utf8-string-1.0.2/utf8-string-1.0.2.tar.gz",
  "https://hackage.haskell.org/package/json-0.10/json-0.10.tar.gz",
  "https://hackage.haskell.org/package/network-bsd-2.8.1.0/network-bsd-2.8.1.0.tar.gz",
  "https://hackage.haskell.org/package/httpd-shed-0.4.1.1/httpd-shed-0.4.1.1.tar.gz",
  "https://hackage.haskell.org/package/exceptions-0.10.5/exceptions-0.10.5.tar.gz",
  "https://hackage.haskell.org/package/stringsearch-0.3.6.6/stringsearch-0.3.6.6.tar.gz",
  "https://hackage.haskell.org/package/multipart-0.2.1/multipart-0.2.1.tar.gz",
  "https://hackage.haskell.org/package/cgi-3001.5.0.0/cgi-3001.5.0.0.tar.gz"
  ]

-- | Get path to locally-built gf
default_gf :: LocalBuildInfo -> FilePath
default_gf lbi = buildDir lbi </> exeName' </> exeNameReal
  where
    -- shadows Distribution.Simple.BuildPaths.exeExtension, which changed type signature in Cabal 2.4
    exeExtension = case hostPlatform lbi of
      Platform arch Windows -> "exe"
      _ -> ""
    exeName' = "gf"
    exeNameReal = exeName' <.> exeExtension
