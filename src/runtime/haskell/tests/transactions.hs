import Test.HUnit
import PGF2
import PGF2.Transactions
import System.Mem
import System.Exit (exitSuccess, exitFailure)
import qualified Data.Map as Map

main = do
  gr1 <- readPGF "tests/basic.pgf"
  let Just ty = readType "(N -> N) -> P (s z)"

  print 1
  gr2 <- modifyPGF gr1 (createFunction "foo" ty 0 [] pi >>
                        createCategory "Q" [(Explicit,"x",ty)] pi)
  print 2

  gr4 <- checkoutPGF gr1

  gr6 <- modifyPGF gr1 (dropFunction "ind" >> dropCategory "S")

  gr7 <- modifyPGF gr1 $
           createConcrete "basic_eng" $ do
             setConcreteFlag "test_flag" (LStr "test")
  let Just cnc = Map.lookup "basic_eng" (languages gr7)

  c <- runTestTT $
    TestList $
      [TestCase (assertEqual "original functions" ["c","floatLit","ind","intLit","nat","s","stringLit","z"] (functions gr1))
      ,TestCase (assertEqual "extended functions" ["c","floatLit","foo","ind","intLit","nat","s","stringLit","z"] (functions gr2))
      ,TestCase (assertEqual "checked-out extended functions" ["c","floatLit","foo","ind","intLit","nat","s","stringLit","z"] (functions gr4))
      ,TestCase (assertEqual "original categories" ["Float","Int","N","P","S","String"] (categories gr1))
      ,TestCase (assertEqual "extended categories" ["Float","Int","N","P","Q","S","String"] (categories gr2))
      ,TestCase (assertEqual "Q context" (Just [(Explicit,"x",ty)]) (categoryContext gr2 "Q"))
      ,TestCase (assertEqual "reduced functions" ["c","floatLit","intLit","nat","s","stringLit","z"] (functions gr6))
      ,TestCase (assertEqual "reduced categories" ["Float","Int","N","P","String"] (categories gr6))
      ,TestCase (assertEqual "old function type" Nothing   (functionType gr1 "foo"))
      ,TestCase (assertEqual "new function type" (Just ty) (functionType gr2 "foo"))
      ,TestCase (assertEqual "old function prob" (-log 0)  (functionProbability gr1 "foo"))
      ,TestCase (assertEqual "new function prob" pi        (functionProbability gr2 "foo"))
      ,TestCase (assertEqual "old category prob" (-log 0)  (categoryProbability gr1 "Q"))
      ,TestCase (assertEqual "new category prob" pi        (categoryProbability gr2 "Q"))
      ,TestCase (assertEqual "empty concretes" ["basic_cnc"] (Map.keys (languages gr1)))
      ,TestCase (assertEqual "extended concretes" ["basic_cnc","basic_eng"] (Map.keys (languages gr7)))
      ,TestCase (assertEqual "added concrete flag" (Just (LStr "test")) (concreteFlag cnc "test_flag"))
      ]

  performMajorGC

  if (errors c == 0) && (failures c == 0)
    then exitSuccess
    else exitFailure
