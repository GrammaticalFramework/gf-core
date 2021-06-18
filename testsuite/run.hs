import Data.List(partition)
import System.IO
import Distribution.Simple.BuildPaths(exeExtension)
import Distribution.System ( buildPlatform, OS (Windows), Platform (Platform) )
import System.Process(readProcess)
import System.Directory(doesFileExist,getDirectoryContents)
import System.FilePath((</>),(<.>),takeExtension)
import System.Exit(exitSuccess,exitFailure)

main = 
   do res <- walk "testsuite"
      let cnt = length res
          (good,bad) = partition ((=="OK").fst.snd) res
          ok = length good + length (filter ((=="FAIL (expected)").fst.snd) bad)
          fail = ok<cnt
      putStrLn $ show ok++"/"++show cnt++ " passed/tests"
      let overview = "gf-tests.html"
      writeFile overview (toHTML bad)
      if ok<cnt 
        then do putStrLn $ overview++" contains an overview of the failed tests"
                exitFailure
        else exitSuccess
  where
    toHTML res =
        "<!DOCTYPE html>\n"
        ++ "<meta charset=\"UTF-8\">\n"
        ++ "<style>\n"
        ++ "pre { max-width: 600px; overflow: scroll; }\n"
        ++ "th,td { vertical-align: top; text-align: left; }\n"
        ++ "</style>\n"
        ++ "<table border=1>\n<tr><th>Result<th>Input<th>Gold<th>Output\n"
        ++ unlines (map testToHTML res)
        ++ "</table>\n"

    testToHTML (in_file,(res,(input,gold,output))) =
      "<tr>"++concatMap td [pre res,in_file++":\n"++pre input,pre gold,pre output]
    pre s = "<pre>"++s++"</pre>"
    td s = "<td>"++s

    walk path = fmap concat . mapM (walkFile . (path </>)) =<< ls path

    walkFile fpath = do
      exists <- doesFileExist fpath
      if exists
        then if takeExtension fpath == ".gfs"
               then do let in_file   = fpath
                           gold_file = fpath <.> ".gold"
                           out_file  = fpath <.> ".out"
                       putStr $ in_file++": "; hFlush stdout
                       res <- runTest in_file out_file gold_file
                       putStrLn $ fst res
                       return [(in_file,res)]
               else return []
        else walk fpath

    runTest in_file out_file gold_file = do
      input <- readFile in_file
      writeFile out_file =<< run_gf ["-run"] input
      exists <- doesFileExist gold_file
      if exists
        then do out <- compatReadFile out_file
                gold <- compatReadFile gold_file
                let info = (input,gold,out)
                if in_file `elem` expectedFailures
                  then return $! if out == gold then ("Unexpected success",info) else ("FAIL (expected)",info)
                  else return $! if out == gold then ("OK",info) else ("FAIL",info)
        else do out <- compatReadFile out_file
                return ("MISSING GOLD",(input,"",out))
    -- Avoid failures caused by Win32/Unix text file incompatibility
    compatReadFile path =
      do h <- openFile path ReadMode
         hSetNewlineMode h universalNewlineMode
         hGetContents h

expectedFailures :: [String]
expectedFailures =
  [ "testsuite/runtime/parser/parser.gfs" -- Only parses `z` as `zero` and not also as e.g. `succ zero` as expected
  , "testsuite/runtime/linearize/brackets.gfs" -- Missing "cannot linearize in the end"
  , "testsuite/compiler/typecheck/abstract/non-abstract-terms.gfs" -- Gives a different error than expected
  ]

-- Should consult the Cabal configuration!
run_gf = readProcess default_gf 
default_gf = "gf"<.>exeExtension
  where
    -- shadows Distribution.Simple.BuildPaths.exeExtension, which changed type signature in Cabal 2.4
    exeExtension = case buildPlatform of
      Platform arch Windows -> "exe"
      _ -> ""

-- | List files, excluding "." and ".."
ls path = filter (`notElem` [".",".."]) `fmap` getDirectoryContents path
