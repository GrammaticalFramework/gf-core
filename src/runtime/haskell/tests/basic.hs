import System.Random
import System.Directory
import Control.Exception
import Test.HUnit
import PGF2

main = do
  g <- newStdGen

  let
    limit = 10 ^ 100
    ns    = take 5000 (randomRs (-limit,limit) g)

    grammarTests :: PGF -> [Test]
    grammarTests gr =
      [TestCase (assertEqual "abstract names" "basic" (abstractName gr))
      ,TestCase (assertEqual "abstract categories" ["Float","Int","N","P","S","String"] (categories gr))
      ,TestCase (assertEqual "abstract functions" ["c","floatLit","ind","intLit","nat","s","stringLit","z"] (functions gr))
      ,TestCase (assertEqual "abstract functions by cat 1" ["s","z"] (functionsByCat gr "N"))
      ,TestCase (assertEqual "abstract functions by cat 2" ["c","floatLit","intLit","stringLit"] (functionsByCat gr "S"))
      ,TestCase (assertEqual "abstract functions by cat 2" [] (functionsByCat gr "X")) -- no such category
      ,TestCase (assertBool  "type of z" (eqJust (readType "N")    (functionType gr "z")))
      ,TestCase (assertBool  "type of s" (eqJust (readType "N->N") (functionType gr "s")))
      ,TestCase (assertBool  "type of c" (eqJust (readType "N->S") (functionType gr "c")))
      ,TestCase (assertEqual "category context 1"  (Just []) (categoryContext gr "N"))
      ,TestCase (assertEqual "category context 2"  (Just []) (categoryContext gr "S"))
      ,TestCase (assertEqual "category context 3"  (Just [(Explicit,"_",DTyp [] "N" [])]) (categoryContext gr "P"))
      ,TestCase (assertEqual "category context 4"  Nothing (categoryContext gr "X")) -- no such category
      ,TestCase (assertEqual "function is constructor 1"  True  (functionIsConstructor gr "s"))
      ,TestCase (assertEqual "function is constructor 2"  True  (functionIsConstructor gr "z"))
      ,TestCase (assertEqual "function is constructor 3"  True  (functionIsConstructor gr "c"))
      ,TestCase (assertEqual "function is constructor 4"  False (functionIsConstructor gr "ind"))
      ,TestCase (assertEqual "show expression 1" "f x y" (showExpr [] (EApp (EApp (EFun "f") (EFun "x")) (EFun "y"))))
      ,TestCase (assertEqual "show expression 2" "f (g x)" (showExpr [] (EApp (EFun "f") (EApp (EFun "g") (EFun "x")))))
      ,TestCase (assertEqual "show expression 3" "f {g x}" (showExpr [] (EApp (EFun "f") (EImplArg (EApp (EFun "g") (EFun "x"))))))
      ,TestCase (assertEqual "show expression 4" "x" (showExpr ["x"] (EVar 0)))
      ,TestCase (assertEqual "show expression 5" "#1" (showExpr ["x"] (EVar 1)))
      ,TestCase (assertEqual "show expression 6" "z" (showExpr ["z","y","x"] (EVar 0)))
      ,TestCase (assertEqual "show expression 7" "y" (showExpr ["z","y","x"] (EVar 1)))
      ,TestCase (assertEqual "show expression 8" "\\w->w" (showExpr ["z","y","x"] (EAbs Explicit "w" (EVar 0))))
      ,TestCase (assertEqual "show expression 9" "\\v,w->z" (showExpr ["z","y","x"] (EAbs Explicit "v" (EAbs Explicit "w" (EVar 2)))))
      ,TestCase (assertEqual "show expression 10" "\\v,{w}->z" (showExpr ["z","y","x"] (EAbs Explicit "v" (EAbs Implicit "w" (EVar 2)))))
      ,TestCase (assertEqual "show expression 11" "\\v,{w},z->z" (showExpr ["y","x"] (EAbs Explicit "v" (EAbs Implicit "w" (EAbs Explicit "z" (EVar 0))))))
      ,TestCase (assertEqual "show expression 12" "\\v,{w,z}->v" (showExpr ["y","x"] (EAbs Explicit "v" (EAbs Implicit "w" (EAbs Implicit "z" (EVar 2))))))
      ,TestCase (assertEqual "show expression 13" "\\v,{w,z},t->v" (showExpr ["y","x"] (EAbs Explicit "v" (EAbs Implicit "w" (EAbs Implicit "z" (EAbs Explicit "t" (EVar 3)))))))
      ,TestCase (assertEqual "show expression 14" "\\u,v,{w,z},t->v" (showExpr ["y","x"] (EAbs Explicit "u" (EAbs Explicit "v" (EAbs Implicit "w" (EAbs Implicit "z" (EAbs Explicit "t" (EVar 3))))))))
      ,TestCase (assertEqual "show expression 15" "f (\\x->x)" (showExpr [] (EApp (EFun "f") (EAbs Explicit "x" (EVar 0)))))
      ,TestCase (assertEqual "show expression 16" "?" (showExpr [] (EMeta 0)))
      ,TestCase (assertEqual "show expression 17" "?42" (showExpr [] (EMeta 42)))
      ,TestCase (assertEqual "show expression 18" "<z : N>" (showExpr [] (ETyped (EFun "z") (DTyp [] "N" []))))
      ,TestCase (assertEqual "show expression 19" "42" (showExpr [] (ELit (LInt 42))))
      ,TestCase (assertEqual "show expression 20" "3.14" (showExpr [] (ELit (LFlt 3.14))))
      ,TestCase (assertEqual "show expression 21" "\"abc\"" (showExpr [] (ELit (LStr "abc"))))
      ,TestCase (assertEqual "show expression 22" "\"ab\\0c\"" (showExpr [] (ELit (LStr "ab\0c"))))
      ,TestCase (assertEqual "show expression 23" "\"ab\\nc\"" (showExpr [] (ELit (LStr "ab\nc"))))
      ,TestCase (assertEqual "show type 1" "N" (showType [] (DTyp [] "N" [])))
      ,TestCase (assertEqual "show type 2" "N -> N" (showType [] (DTyp [(Explicit,"_",DTyp [] "N" [])] "N" [])))
      ,TestCase (assertEqual "show type 3" "(N -> N) -> N" (showType [] (DTyp [(Explicit,"_",DTyp [(Explicit,"_",DTyp [] "N" [])] "N" [])] "N" [])))
      ,TestCase (assertEqual "show type 4" "(x : N) -> P x" (showType [] (DTyp [(Explicit,"x",DTyp [] "N" [])] "P" [EVar 0])))
      ,TestCase (assertEqual "show type 5" "(f : N -> N) -> P (f z)" (showType [] (DTyp [(Explicit,"f",DTyp [(Explicit,"_",DTyp [] "N" [])] "N" [])] "P" [EApp (EVar 0) (EFun "z")])))
      ,TestCase (assertEqual "show type 6" "(f : N -> N) -> P (f n)" (showType ["n"] (DTyp [(Explicit,"f",DTyp [(Explicit,"_",DTyp [] "N" [])] "N" [])] "P" [EApp (EVar 0) (EVar 1)])))
      ,TestCase (assertEqual "show type 7" "({f} : N -> N) -> P (f n)" (showType ["n"] (DTyp [(Implicit,"f",DTyp [(Explicit,"_",DTyp [] "N" [])] "N" [])] "P" [EApp (EVar 0) (EVar 1)])))
      ,TestCase (assertEqual "show type 8" "(x : N) -> P x -> S" (showType [] (DTyp [(Explicit,"x",DTyp [] "N" []),(Explicit,"_",DTyp [] "P" [EVar 0])] "S" [])))
      ,TestCase (assertEqual "show type 9" "(x : N) -> (y : P x) -> S" (showType [] (DTyp [(Explicit,"x",DTyp [] "N" []),(Explicit,"y",DTyp [] "P" [EVar 0])] "S" [])))
      ,TestCase (assertEqual "show context" "(x : N) (P x)" (showContext [] [(Explicit,"x",DTyp [] "N" []),(Explicit,"_",DTyp [] "P" [EVar 0])]))
      ,TestCase (assertEqual "fresh variables 1" "\\v,v1->v1" (showExpr [] (EAbs Explicit "v" (EAbs Explicit "v" (EVar 0)))))
      ,TestCase (assertEqual "fresh variables 2" "\\v,v1->v" (showExpr [] (EAbs Explicit "v" (EAbs Explicit "v" (EVar 1)))))
      ,TestCase (assertEqual "fresh variables 3" "\\v,v1,v2->v1" (showExpr [] (EAbs Explicit "v" (EAbs Explicit "v" (EAbs Explicit "v" (EVar 1))))))
      ,TestCase (assertEqual "large integer 1" [] [n | n <- ns, showExpr [] (ELit (LInt n)) /= show n])
      ,TestCase (assertEqual "large integer 2" [] [n | n <- ns, readExpr (show n) /= Just (ELit (LInt n))])
      ,TestCase (assertEqual "unicode names 1" (Just "'абв'") (fmap (showExpr []) (readExpr "'абв'")))
      ,TestCase (assertEqual "unicode names 2" (Just "ab") (fmap (showExpr []) (readExpr "'ab'")))
      ,TestCase (assertEqual "unicode names 3" (Just "a'b") (fmap (showExpr []) (readExpr "'a\\'b'")))
      ,TestCase (assertEqual "unicode names 4" (Just "'а\\'б'") (fmap (showExpr []) (readExpr "'а\\'б'")))
      ]

  removeFile "tests/basic.ngf" `catch` \e -> do
    print (e :: SomeException)

  gr1 <- readPGF "tests/basic.pgf"
  gr2 <- bootNGF "tests/basic.pgf" "tests/basic.ngf"
  gr3 <- readNGF "tests/basic.ngf"
  pmcfg <- readFile "tests/basic.pmcfg"

  rp1 <- testLoadFailure (readPGF "non-existing.pgf")
  rp2 <- testLoadFailure (readPGF "tests/basic.gf")
  rp3 <- testLoadFailure (readPGF "tests/basic.ngf")

  bn1 <- testLoadFailure (bootNGF "non-existing.pgf" "non-existing.ngf")
  bn2 <- testLoadFailure (bootNGF "tests/basic.gf" "tests/basic.ngf")
  bn3 <- testLoadFailure (bootNGF "tests/basic.ngf" "tests/tmp.pgf")

  rn2 <- testLoadFailure (readNGF "tests/basic.gf")
  rn3 <- testLoadFailure (readNGF "tests/basic.pgf")

  runTestTTAndExit $
    TestList $
      [TestCase (assertBool "missing file" rp1)
      ,TestCase (assertBool "wrong file format (GF)" rp2)
      ,TestCase (assertBool "wrong file format (NGF)" rp3)

      ,TestCase (assertBool "missing file" bn1)
      ,TestCase (assertBool "wrong file format (GF)" bn2)
      ,TestCase (assertBool "wrong file format (NGF)" bn3)

      ,TestCase (assertBool "wrong file format (GF)" rn2)
      ,TestCase (assertBool "wrong file format (PGF)" rn3)

      ,TestCase (assertEqual "show pgf" (init pmcfg) (showPGF gr1))
      ]
      ++ grammarTests gr1
      ++ grammarTests gr2
      ++ grammarTests gr3


testLoadFailure io = do
  res <- try io
  case res :: Either SomeException PGF of
    Left  _ -> return True
    Right _ -> return False

eqJust (Just x) (Just y) = x == y
eqJust _        _        = False
