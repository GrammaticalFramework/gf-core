import Test.HUnit
import PGF2
import qualified Data.Map as Map

main = do
  gr <- readPGF "tests/basic.pgf"
  let Just cnc = Map.lookup "basic_cnc" (languages gr)
  runTestTTAndExit $
    TestList
      [TestCase (assertParseOk   gr cnc "zero" [("intLit 0",1.609438),("c z",2.3025851)] "0")
      ,TestCase (assertParseOk   gr cnc "one"  [("intLit 1",1.609438),("c (s ?)",2.3025851)] "1")
      ,TestCase (assertParseOk   gr cnc "two"  [("c (s (s ?))",2.9957323)] "1 + 1")
      ,TestCase (assertParseFail gr cnc "needs-bind" "1+1")
      ,TestCase (assertParseOk   gr cnc "int"  [("intLit 128",1.609438)] "128")
      ,TestCase (assertParseOk   gr cnc "neg-int" [("intLit -128",1.609438)] "-128")
      ,TestCase (assertParseOk   gr cnc "float"   [("floatLit 3.14",1.609438)] "3.14")
      ,TestCase (assertParseOk   gr cnc "neg-float" [("floatLit -3.14",1.609438)] "-3.14")
      ,TestCase (assertParseOk   gr cnc "string"  [("stringLit \"abc\"",1.609438)] "abc")
      ,TestCase (assertParseFail gr cnc "dwo-dots" "3.1.4")
      ]

assertParseOk gr cnc name expr_strs str =
  case parse cnc (startCat gr) str of
    ParseOk es -> let exp_es = [(e,prob) | (expr_str,prob) <- expr_strs, Just e <- [readExpr expr_str]]
                  in assertEqual name exp_es es
    _          -> assertFailure (name++": the expression is not readable")

assertParseFail gr cnc name str =
  case parse cnc (startCat gr) str of
    ParseOk es    -> assertFailure (name++": the string should not have been parsable")
    ParseFailed _ -> return ()
