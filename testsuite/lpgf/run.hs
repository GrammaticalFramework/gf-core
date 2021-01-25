import LPGF
import PGF (Tree, mkCId, mkApp, readLanguage, showLanguage, showExpr)

import Control.Monad (forM_)
import qualified Data.Map as Map
import Text.Printf (printf)

main :: IO ()
main = do
  lpgf <- readLPGF "Zero.lpgf"
  forM_ [tree1, tree2, tree3] $ \tree -> do
    putStrLn (showExpr [] tree)
    forM_ (Map.toList (concretes lpgf)) $ \(lang,concr) ->
      printf "%s: %s\n" (showLanguage lang) (linearizeConcr concr tree)

-- Pred John Walk
tree1 :: Tree
tree1 = mkApp (mkCId "Pred") [mkApp (mkCId "John") [], mkApp (mkCId "Walk") []]

-- Pred We Walk
tree2 :: Tree
tree2 = mkApp (mkCId "Pred") [mkApp (mkCId "We") [], mkApp (mkCId "Walk") []]

-- And (Pred John Walk) (Pred We Walk)
tree3 :: Tree
tree3 = mkApp (mkCId "And") [tree1, tree2]

-- Initial LPGF, Figures 6 & 7
zero :: LPGF
zero = LPGF {
  absname = mkCId "Zero",
  abstract = Abstr {
    -- cats = Map.fromList [
    --   (mkCId "S", ()),
    --   (mkCId "NP", ()),
    --   (mkCId "VP", ())
    -- ],
    -- funs = Map.fromList [
    --   (mkCId "And", Type [mkCId "S", mkCId "S"] (mkCId "S")),
    --   (mkCId "Pred", Type [mkCId "NP", mkCId "VP"] (mkCId "S")),
    --   (mkCId "John", Type [] (mkCId "NP")),
    --   (mkCId "We", Type [] (mkCId "NP")),
    --   (mkCId "Walk", Type [] (mkCId "VP"))
    -- ]
  },
  concretes = Map.fromList [
    (mkCId "ZeroEng", Concr {
      -- lincats = Map.fromList [
      --   (mkCId "S", LTStr),
      --   (mkCId "NP", LTProduct [LTStr, LTInt 2]),
      --   (mkCId "VP", LTProduct [LTStr, LTStr])
      -- ],
      lins = Map.fromList [
        (mkCId "And", mkConcat [LFArgument 1, LFToken "and", LFArgument 2]),
        (mkCId "Pred", mkConcat [LFProjection (LFArgument 1) (LFInt 1), LFProjection (LFArgument 2) (LFProjection (LFArgument 1) (LFInt 2))]),
        (mkCId "John", LFTuple [LFToken "John", LFInt 1]),
        (mkCId "We", LFTuple [LFToken "we", LFInt 2]),
        (mkCId "Walk", LFTuple [LFToken "walks", LFToken "walk"])
      ]
    }),
    (mkCId "ZeroGer", Concr {
      -- lincats = Map.fromList [
      --   (mkCId "S", LTStr),
      --   (mkCId "NP", LTProduct [LTStr, LTInt 2, LTInt 3]),
      --   (mkCId "VP", LTProduct [LTProduct [LTStr, LTStr, LTStr], LTProduct [LTStr, LTStr, LTStr]])
      -- ],
      lins = Map.fromList [
        (mkCId "And", mkConcat [LFArgument 1, LFToken "und", LFArgument 2]),
        (mkCId "Pred", mkConcat [LFProjection (LFArgument 1) (LFInt 1), LFProjection (LFProjection (LFArgument 2) (LFProjection (LFArgument 1) (LFInt 2))) (LFProjection (LFArgument 1) (LFInt 3))]),
        (mkCId "John", LFTuple [LFToken "John", LFInt 1, LFInt 3]),
        (mkCId "We", LFTuple [LFToken "wir", LFInt 2, LFInt 1]),
        (mkCId "Walk", LFTuple [LFTuple [LFToken "gehe", LFToken "gehst", LFToken "geht"], LFTuple [LFToken "gehen", LFToken "geht", LFToken "gehen"]])
      ]
    })
  ]
}
