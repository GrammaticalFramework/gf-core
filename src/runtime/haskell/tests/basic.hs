import Test.HUnit
import PGF2

main = do
  gr <- readPGF "tests/basic.pgf"
  runTestTTAndExit $
    TestList [TestCase (assertEqual "abstract names" "basic" (abstractName gr))
             ,TestCase (assertEqual "abstract categories" ["Float","Int","N","P","S","String"] (categories gr))
             ,TestCase (assertEqual "abstract functions" ["c","ind","s","z"] (functions gr))
             ,TestCase (assertEqual "abstract functions by cat 1" ["s","z"] (functionsByCat gr "N"))
             ,TestCase (assertEqual "abstract functions by cat 2" ["c"] (functionsByCat gr "S"))
             ,TestCase (assertEqual "abstract functions by cat 2" [] (functionsByCat gr "X")) -- no such category
             ,TestCase (assertBool  "type of z" (eqJust (readType "N")    (functionType gr "z")))
             ,TestCase (assertBool  "type of s" (eqJust (readType "N->N") (functionType gr "s")))
             ,TestCase (assertBool  "type of c" (eqJust (readType "N->S") (functionType gr "c")))
             ,TestCase (assertEqual "category context 1"  [] (categoryContext gr "N"))
             ,TestCase (assertEqual "category context 1"  [] (categoryContext gr "S"))
             ,TestCase (assertEqual "category context 1"  [(Explicit,"_",DTyp [] "N" [])] (categoryContext gr "P"))
             ,TestCase (assertEqual "category context 1"  [] (categoryContext gr "X")) -- no such category
             ]

eqJust (Just x) (Just y) = x == y
eqJust _        _        = False
