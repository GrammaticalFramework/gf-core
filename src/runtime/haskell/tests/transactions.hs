import Test.HUnit
import PGF2
import PGF2.Transactions

main = do
  gr1 <- readPGF "tests/basic.pgf"
  let Just ty = readType "(N -> N) -> P (s z)"
  gr2 <- modifyPGF gr1 (createFunction "foo" ty pi)

  runTestTTAndExit $
    TestList $
      [TestCase (assertEqual "original functions" ["c","ind","s","z"] (functions gr1))
      ,TestCase (assertEqual "extended functions" ["c","foo","ind","s","z"] (functions gr2))
      ,TestCase (assertEqual "old function type" Nothing   (functionType gr1 "foo"))
      ,TestCase (assertEqual "new function type" (Just ty) (functionType gr2 "foo"))
      ,TestCase (assertEqual "old function prob" (-log 0)  (functionProb gr1 "foo"))
      ,TestCase (assertEqual "new function prob" pi        (functionProb gr2 "foo"))
      ]
