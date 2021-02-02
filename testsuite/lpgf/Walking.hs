import LPGF
import PGF (Tree, mkCId, mkApp)

import qualified Data.Map as Map

main = return ()

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
walking :: LPGF
walking = LPGF {
  absname = mkCId "Walking",
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
    (mkCId "WalkingEng", Concr {
      -- lincats = Map.fromList [
      --   (mkCId "S", LTStr),
      --   (mkCId "NP", LTProduct [LTStr, LTInt 2]),
      --   (mkCId "VP", LTProduct [LTStr, LTStr])
      -- ],
      lins = Map.fromList [
        (mkCId "And", mkConcat [LFArgument 1, LFToken "and", LFArgument 2]),
        -- (mkCId "Pred", mkConcat [LFProjection (LFArgument 1) (LFInt 1), LFProjection (LFArgument 2) (LFProjection (LFArgument 1) (LFInt 2))]),
        (mkCId "Pred", mkConcat [LFProjection (LFArgument 1) (LFInt 1), LFProjection (LFProjection (LFArgument 2) (LFInt 1)) (LFProjection (LFArgument 1) (LFInt 2))]),
        (mkCId "John", LFTuple [LFToken "John", LFInt 1]),
        (mkCId "We", LFTuple [LFToken "we", LFInt 2]),
        -- (mkCId "Walk", LFTuple [LFToken "walks", LFToken "walk"])
        (mkCId "Walk", LFTuple [LFTuple [LFToken "walks", LFToken "walk"]])
      ]
    }),
    (mkCId "WalkingGer", Concr {
      -- lincats = Map.fromList [
      --   (mkCId "S", LTStr),
      --   (mkCId "NP", LTProduct [LTStr, LTInt 2, LTInt 3]),
      --   (mkCId "VP", LTProduct [LTProduct [LTStr, LTStr, LTStr], LTProduct [LTStr, LTStr, LTStr]])
      -- ],
      lins = Map.fromList [
        (mkCId "And", mkConcat [LFArgument 1, LFToken "und", LFArgument 2]),
        -- (mkCId "Pred", mkConcat [LFProjection (LFArgument 1) (LFInt 1), LFProjection (LFProjection (LFArgument 2) (LFProjection (LFArgument 1) (LFInt 2))) (LFProjection (LFArgument 1) (LFInt 3))]),
        (mkCId "Pred", mkConcat [LFProjection (LFArgument 1) (LFInt 1), LFProjection (LFProjection (LFProjection (LFArgument 2) (LFInt 1)) (LFProjection (LFArgument 1) (LFInt 2))) (LFProjection (LFArgument 1) (LFInt 3))]),
        (mkCId "John", LFTuple [LFToken "John", LFInt 1, LFInt 3]),
        (mkCId "We", LFTuple [LFToken "wir", LFInt 2, LFInt 1]),
        -- (mkCId "Walk", LFTuple [LFTuple [LFToken "gehe", LFToken "gehst", LFToken "geht"], LFTuple [LFToken "gehen", LFToken "geht", LFToken "gehen"]])
        (mkCId "Walk", LFTuple [LFTuple [LFTuple [LFToken "gehe", LFToken "gehst", LFToken "geht"], LFTuple [LFToken "gehen", LFToken "geht", LFToken "gehen"]]])
      ]
    })
  ]
}
