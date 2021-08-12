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
             ,TestCase (assertBool  "type of z" (eqJust (readType "N")    (functionType gr "z")))
             ,TestCase (assertBool  "type of s" (eqJust (readType "N->N") (functionType gr "s")))
             ,TestCase (assertBool  "type of c" (eqJust (readType "N->S") (functionType gr "c")))
             ]

eqJust (Just x) (Just y) = x == y
eqJust _        _        = False
