import Test.HUnit
import Test.HUnit.Text
import PGF2

main = do
  gr <- readPGF "tests/basic.pgf"
  runTestTTAndExit $
    TestList $
      [TestCase (assertInference "infer fun" gr (Right "N") "z")
      ,TestCase (assertInference "infer app" gr (Right "N") "s z")
      ,TestCase (assertInference "infer n-args 1" gr (Left "Too many arguments") "z z")
      ,TestCase (assertInference "infer n-args 2" gr (Left "Too many arguments") "s z z")
      ,TestCase (assertInference "infer implarg 1" gr (Left "Unexpected implicit argument") "s {z}")
      ,TestCase (assertInference "infer implarg 2" gr (Right "(y : N) -> S") "imp {z}")
      ,TestCase (assertInference "infer implarg 3" gr (Right "S") "imp {z} z")
      ,TestCase (assertInference "infer implarg 4" gr (Right "({x},y : N) -> S") "imp")
      ,TestCase (assertInference' "infer implarg 4" gr (Right ("imp {?} z","S")) "imp z")
      ,TestCase (assertInference "infer typed 1" gr (Right "N->N") "<s : N->N>")
      ,TestCase (assertInference "infer typed 2" gr (Left "Types doesn't match") "<s : N>")
      ,TestCase (assertInference "infer typed 3" gr (Left "Too many arguments to category N - 0 expected but 1 given") "<s : N z>")
      ,TestCase (assertInference "infer hoas 1" gr (Left "Types doesn't match") "s s")
      ,TestCase (assertInference "infer literal 1" gr (Right "Int") "0")
      ,TestCase (assertInference "infer literal 2" gr (Right "Float") "3.14")
      ,TestCase (assertInference "infer literal 3" gr (Right "String") "\"abc\"")
      ,TestCase (assertInference "infer meta 1" gr (Left "Cannot infer the type of a meta variable") "?")
      ,TestCase (assertInference "infer meta 2" gr (Right "N->N") "<? : N->N>")
      ,TestCase (assertInference "infer lambda" gr (Left "Cannot infer the type of a lambda abstraction") "\\x->x")
      ,TestCase (assertChecking "check fun 1" gr (Right "s") "s" "N->N")
      ,TestCase (assertChecking "check fun 2" gr (Right "s z") "s z" "N")
      ,TestCase (assertChecking "check fun 3" gr (Left "Types doesn't match") "s z" "N->N")
      ,TestCase (assertChecking "check lambda 1" gr (Right "\\x->x") "\\x->x" "N->N")
      ,TestCase (assertChecking "check lambda 2" gr (Right "\\x->s x") "\\x->s x" "N->N")
      ,TestCase (assertType "check type 1" gr (Right "N -> N") "N -> N")
      ,TestCase (assertType "check type 2" gr (Left "Category s is not defined") "s")
      ,TestCase (assertType "check type 3" gr (Left "Too many arguments to category N - 0 expected but 1 given") "N z")
      ,TestCase (assertType "check type 4" gr (Left "Too few arguments to category P - 1 expected but 0 given") "P")
      ,TestCase (assertType "check type 5" gr (Right "P z") "P z")
      ,TestCase (assertType "check type 6" gr (Left "Types doesn't match") "P s")
      ,TestCase (assertType "check type 7" gr (Left "Unexpected implicit argument") "P {z}")
      ]

assertInference name gr (Left msg) e =
  case readExpr e of
    Just e -> assertEqual name (Left msg) (inferExpr gr e)
    _       -> error "Reading the expression failed"
assertInference name gr (Right ty) e =
  case (readType ty, readExpr e) of
    (Just ty,Just e) -> assertEqual name (Right (e,ty)) (inferExpr gr e)
    _                -> error "Reading the type/expression failed"

assertInference' name gr (Left msg) e =
  case readExpr e of
    Just e -> assertEqual name (Left msg) (inferExpr gr e)
    _       -> error "Reading the expression failed"
assertInference' name gr (Right (e1,ty)) e0 =
  case (readExpr e1, readType ty, readExpr e0) of
    (Just e1,Just ty,Just e0) -> assertEqual name (Right (e1,ty)) (inferExpr gr e0)
    _                         -> error "Reading the type/expression failed"

assertChecking name gr (Left msg) e ty =
  case (readExpr e, readType ty) of
    (Just e,Just ty) -> assertEqual name (Left msg) (checkExpr gr e ty)
    _                -> error "Reading the expression failed"
assertChecking name gr (Right e1) e ty =
  case (readExpr e1, readExpr e, readType ty) of
    (Just e1,Just e,Just ty) -> assertEqual name (Right e1) (checkExpr gr e ty)
    _                        -> error "Reading the type/expression failed"

assertType name gr (Left msg) ty =
  case readType ty of
    Just ty -> assertEqual name (Left msg) (checkType gr ty)
    _       -> error "Reading the type failed"
assertType name gr (Right ty) ty0 =
  case (readType ty, readType ty0) of
    (Just ty,Just ty0) -> assertEqual name (Right ty) (checkType gr ty0)
    _                  -> error "Reading the type failed"
