import Distribution.System(Platform(..),OS(..))
import Distribution.Simple(defaultMainWithHooks,UserHooks(..),simpleUserHooks)
import Distribution.Simple.LocalBuildInfo(LocalBuildInfo(..),absoluteInstallDirs,datadir)
import Distribution.Simple.Setup(BuildFlags(..),Flag(..),InstallFlags(..),CopyDest(..),CopyFlags(..),SDistFlags(..))
import Distribution.PackageDescription(PackageDescription(..),emptyHookedBuildInfo)
import Distribution.Simple.BuildPaths(exeExtension)
import System.FilePath((</>),(<.>))

import WebSetup

-- | Notice about RGL not built anymore
noRGLmsg :: IO ()
noRGLmsg = putStrLn "Notice: the RGL is not built as part of GF anymore. See https://github.com/GrammaticalFramework/gf-rgl"

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { preBuild  = gfPreBuild
  , postBuild = gfPostBuild
  , preInst   = gfPreInst
  , postInst  = gfPostInst
  , postCopy  = gfPostCopy
  }
  where
    gfPreBuild args  = gfPre args . buildDistPref
    gfPreInst args = gfPre args . installDistPref

    gfPre args distFlag = do
      return emptyHookedBuildInfo

    gfPostBuild args flags pkg lbi = do
      -- noRGLmsg
      let gf = default_gf lbi
      buildWeb gf flags (pkg,lbi)

    gfPostInst args flags pkg lbi = do
      -- noRGLmsg
      saveInstallPath args flags (pkg,lbi)
      installWeb (pkg,lbi)

    gfPostCopy args flags  pkg lbi = do
      -- noRGLmsg
      saveCopyPath args flags (pkg,lbi)
      copyWeb flags (pkg,lbi)

    -- `cabal sdist` will not make a proper dist archive, for that see `make sdist`
    -- However this function should exit quietly to allow building gf in sandbox
    gfSDist pkg lbi hooks flags = do
      return ()

saveInstallPath :: [String] -> InstallFlags -> (PackageDescription, LocalBuildInfo) -> IO ()
saveInstallPath args flags bi = do
  let
    dest = NoCopyDest
    dir = datadir (uncurry absoluteInstallDirs bi dest)
  writeFile dataDirFile dir

saveCopyPath :: [String] -> CopyFlags -> (PackageDescription, LocalBuildInfo) -> IO ()
saveCopyPath args flags bi = do
  let
    dest = case copyDest flags of
      NoFlag -> NoCopyDest
      Flag d -> d
    dir = datadir (uncurry absoluteInstallDirs bi dest)
  writeFile dataDirFile dir

-- | Name of file where installation's data directory is recording
-- This is a last-resort way in which the seprate RGL build script
-- can determine where to put the compiled RGL files
dataDirFile :: String
dataDirFile = "DATA_DIR"

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
