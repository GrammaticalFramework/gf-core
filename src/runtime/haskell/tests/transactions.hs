import Test.HUnit
import PGF2
import PGF2.Transactions
import System.Mem
import System.Exit (exitSuccess, exitFailure)

main = do
  gr1 <- readPGF "tests/basic.pgf"
  let Just ty = readType "(N -> N) -> P (s z)"

  gr2 <- modifyPGF gr1              (createFunction "foo" ty 0 pi >>
                                     createCategory "Q" [(Explicit,"x",ty)] pi)
  gr3 <- branchPGF gr1 "bar_branch" (createFunction "bar" ty 0 pi >>
                                     createCategory "R" [(Explicit,"x",ty)] pi)

  Just gr4 <- checkoutPGF gr1 "master"
  Just gr5 <- checkoutPGF gr1 "bar_branch"

  gr6 <- modifyPGF gr1 (dropFunction "ind" >> dropCategory "S")

  c <- runTestTT $
    TestList $
      [TestCase (assertEqual "original functions" ["c","ind","s","z"] (functions gr1))
      ,TestCase (assertEqual "extended functions" ["c","foo","ind","s","z"] (functions gr2))
      ,TestCase (assertEqual "branched functions" ["bar","c","ind","s","z"] (functions gr3))
      ,TestCase (assertEqual "checked-out extended functions" ["c","foo","ind","s","z"] (functions gr4))
      ,TestCase (assertEqual "checked-out branched functions" ["bar","c","ind","s","z"] (functions gr5))
      ,TestCase (assertEqual "original categories" ["Float","Int","N","P","S","String"] (categories gr1))
      ,TestCase (assertEqual "extended categories" ["Float","Int","N","P","Q","S","String"] (categories gr2))
      ,TestCase (assertEqual "branched categories" ["Float","Int","N","P","R","S","String"] (categories gr3))
      ,TestCase (assertEqual "Q context" (Just [(Explicit,"x",ty)]) (categoryContext gr2 "Q"))
      ,TestCase (assertEqual "R context" (Just [(Explicit,"x",ty)]) (categoryContext gr3 "R"))
      ,TestCase (assertEqual "reduced functions" ["c","s","z"] (functions gr6))
      ,TestCase (assertEqual "reduced categories" ["Float","Int","N","P","String"] (categories gr6))
      ,TestCase (assertEqual "old function type" Nothing   (functionType gr1 "foo"))
      ,TestCase (assertEqual "new function type" (Just ty) (functionType gr2 "foo"))
      ,TestCase (assertEqual "old function prob" (-log 0)  (functionProbability gr1 "foo"))
      ,TestCase (assertEqual "new function prob" pi        (functionProbability gr2 "foo"))
      ,TestCase (assertEqual "old category prob" (-log 0)  (categoryProbability gr1 "Q"))
      ,TestCase (assertEqual "new category prob" pi        (categoryProbability gr2 "Q"))
      ]

  performMajorGC

  if (errors c == 0) && (failures c == 0)
    then exitSuccess
    else exitFailure
