import Test.HUnit
import PGF2
import PGF2.Transactions
import System.Mem
import System.Exit (exitSuccess, exitFailure)
import qualified Data.Map as Map
import Control.Exception (try)

main = do
  gr1 <- readPGF "tests/basic.pgf"
  let Just ty = readType "(N -> N) -> P (s z)"

  excpt1 <- try (modifyPGF gr1 (createFunction "c" ty 0 [] pi) >> return ())
  excpt2 <- try (modifyPGF gr1 (createCategory "N" [] pi) >> return ())

  gr2 <- modifyPGF gr1 (createFunction "foo" ty 0 [] pi >>
                        createCategory "Q" [(Explicit,"x",ty)] pi)

  gr4 <- checkoutPGF gr1

  gr6 <- modifyPGF gr1 (dropFunction "ind" >> dropCategory "S")
  let Just cnc6 = Map.lookup "basic_cnc" (languages gr6)

  gr7 <- modifyPGF gr1 $
           createConcrete "basic_eng" $ do
             setConcreteFlag "test_flag" (LStr "test")
  let Just cnc7 = Map.lookup "basic_eng" (languages gr7)

  gr8 <- modifyPGF gr1 $
           alterConcrete "basic_cnc" $ do
             dropLin "z"
  let Just cnc8 = Map.lookup "basic_cnc" (languages gr8)

  gr9 <- modifyPGF gr1 $
           alterConcrete "basic_cnc" $ do
             dropLincat "N"
  let Just cnc9 = Map.lookup "basic_cnc" (languages gr9)

  excpt3 <- try (modifyPGF gr1 (alterConcrete "basic_foo" (return ())) >> return ())


  c <- runTestTT $
    TestList $
      [TestCase (assertEqual "original functions" ["c","floatLit","ind","intLit","nat","s","stringLit","z"] (functions gr1))
      ,TestCase (assertEqual "existing function" (Left (PGFError "modifyPGF" "A function with that name already exists")) excpt1)
      ,TestCase (assertEqual "existing category" (Left (PGFError "modifyPGF" "A category with that name already exists")) excpt2)
      ,TestCase (assertEqual "extended functions" ["c","floatLit","foo","ind","intLit","nat","s","stringLit","z"] (functions gr2))
      ,TestCase (assertEqual "checked-out extended functions" ["c","floatLit","foo","ind","intLit","nat","s","stringLit","z"] (functions gr4))
      ,TestCase (assertEqual "original categories" ["Float","Int","N","P","S","String"] (categories gr1))
      ,TestCase (assertEqual "extended categories" ["Float","Int","N","P","Q","S","String"] (categories gr2))
      ,TestCase (assertEqual "Q context" (Just [(Explicit,"x",ty)]) (categoryContext gr2 "Q"))
      ,TestCase (assertEqual "reduced functions" ["foo","nat","s","z"] (functions gr6))
      ,TestCase (assertEqual "reduced categories" ["Float","Int","N","P","Q","String"] (categories gr6))
      ,TestCase (assertEqual "reduced lins" [False,False,False,False,True,True,False,True] (map (hasLinearization cnc6) ["c","floatLit","foo","intLit","nat","s","stringLit","z"]))
      ,TestCase (assertEqual "old function type" Nothing   (functionType gr1 "foo"))
      ,TestCase (assertEqual "new function type" (Just ty) (functionType gr2 "foo"))
      ,TestCase (assertEqual "old function prob" (-log 0)  (functionProbability gr1 "foo"))
      ,TestCase (assertEqual "new function prob" pi        (functionProbability gr2 "foo"))
      ,TestCase (assertEqual "old category prob" (-log 0)  (categoryProbability gr1 "Q"))
      ,TestCase (assertEqual "new category prob" pi        (categoryProbability gr2 "Q"))
      ,TestCase (assertEqual "empty concretes" ["basic_cnc"] (Map.keys (languages gr1)))
      ,TestCase (assertEqual "extended concretes" ["basic_cnc","basic_eng"] (Map.keys (languages gr7)))
      ,TestCase (assertEqual "added concrete flag" (Just (LStr "test")) (concreteFlag cnc7 "test_flag"))
      ,TestCase (assertEqual "alter missing concrete" (Left (PGFError "modifyPGF" "Unknown concrete syntax")) excpt3)
      ,TestCase (assertEqual "drop lin" (True,False) (hasLinearization cnc8 "s",hasLinearization cnc8 "z"))
      ,TestCase (assertEqual "drop lincat" (False,False) (hasLinearization cnc9 "s",hasLinearization cnc9 "z"))
      ]

  performMajorGC

  if (errors c == 0) && (failures c == 0)
    then exitSuccess
    else exitFailure

