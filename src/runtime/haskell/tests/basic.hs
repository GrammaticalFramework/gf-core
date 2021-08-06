import Test.HUnit
import PGF2

main = do
  gr <- readPGF "tests/basic.pgf"
  runTestTTAndExit $
    TestList [TestCase (assertEqual "abstract names" "basic" (abstractName gr))
             ,TestCase (assertEqual "abstract categories" ["Float","Int","N","S","String"] (categories gr))
             ,TestCase (assertEqual "abstract functions" ["c","s","z"] (functions gr))
             ,TestCase (assertEqual "abstract functions by cat 1" ["s","z"] (functionsByCat gr "N"))
             ,TestCase (assertEqual "abstract functions by cat 2" ["c"] (functionsByCat gr "S"))
             ]
