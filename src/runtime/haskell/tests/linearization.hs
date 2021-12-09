import Test.HUnit
import PGF2
import qualified Data.Map as Map

main = do
  gr <- readPGF "tests/basic.pgf"
  let Just cnc = Map.lookup "basic_cnc" (languages gr)
  runTestTTAndExit $
    TestList
      [TestCase (assertEqual "zero" "0" (linearize cnc (mkApp "z" [])))
      ,TestCase (assertEqual "one"  "1" (linearize cnc (mkApp "s" [mkApp "z" []])))
      ,TestCase (assertEqual "two"  "1 + 1" (linearize cnc (mkApp "s" [mkApp "s" [mkApp "z" []]])))
      ,TestCase (assertEqual "two'" "(S:1 (N:2 (N:3 1) + 1))" (showBracketedString (head (bracketedLinearize cnc (mkApp "c" [mkApp "s" [mkApp "s" [mkApp "z" []]]])))))
      ,TestCase (assertEqual "foo" "(S:1 (N:2 (N:3 [foo]) + 1))" (showBracketedString (head (bracketedLinearize cnc (mkApp "c" [mkApp "s" [mkApp "foo" []]])))))
      ,TestCase (assertEqual "meta" "(S:1 (N:2 (N:3 ?1) + 1))" (showBracketedString (head (bracketedLinearize cnc (mkApp "c" [mkApp "s" [mkMeta 1]])))))
      ,TestCase (assertEqual "ind" "nat(0) & λx,p . nat(x + 1)" (linearize cnc (mkApp "ind" [mkApp "nat" [mkApp "z" []], mkAbs Explicit "x" (mkAbs Explicit "p" (mkApp "nat" [mkApp "s" [mkVar 1]])),mkApp "s" [mkApp "z" []]])))
      ,TestCase (assertEqual "parse tree 1" graphviz_parse1 (graphvizParseTree cnc graphvizDefaults (mkApp "c" [mkApp "s" [mkMeta 1]])))
      ,TestCase (assertEqual "intLit" "666" (linearize cnc (mkApp "intLit" [mkInt 666])))
      ,TestCase (assertEqual "floatLit" "3.14" (linearize cnc (mkApp "floatLit" [mkFloat 3.14])))
      ,TestCase (assertEqual "stringLit" "abcd" (linearize cnc (mkApp "stringLit" [mkStr "abcd"])))
      ,TestCase (assertEqual "parse tree 2" graphviz_parse2 (graphvizParseTree cnc graphvizDefaults (mkApp "stringLit" [mkStr "abcd"])))
      ]

graphviz_parse1="graph {\n  node[shape=plaintext]\n\n  subgraph {\n    rank=same;\n    n1[label=\"c : S\"]\n  }\n\n  subgraph {\n    rank=same;\n    n2[label=\"s : N\"]\n  }\n  n1 -- n2\n\n  subgraph {\n    rank=same;\n    n3[label=\"_ : N\"]\n  }\n  n2 -- n3\n\n  subgraph {\n    rank=same;\n    edge[style=invis]\n    n100000[label=\"?1\"]\n    n100001[label=\"+\"]\n    n100002[label=\"1\"]\n    n100000 -- n100001 -- n100002\n  }\n  n3 -- n100000\n  n2 -- n100001\n  n2 -- n100002\n}"
graphviz_parse2="graph {\n  node[shape=plaintext]\n\n  subgraph {\n    rank=same;\n    n1[label=\"stringLit : S\"]\n  }\n\n  subgraph {\n    rank=same;\n    n2[label=\"_ : String\"]\n  }\n  n1 -- n2\n\n  subgraph {\n    rank=same;\n    n100000[label=\"abcd\"]\n  }\n  n2 -- n100000\n}"
