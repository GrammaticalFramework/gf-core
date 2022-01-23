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
      --   (mkCId "S", StrType),
      --   (mkCId "NP", ProductType [StrType, IxType 2]),
      --   (mkCId "VP", ProductType [StrType, StrType])
      -- ],
      lins = Map.fromList [
        (mkCId "And", mkConcat [Argument 1, Token "and", Argument 2]),
        -- (mkCId "Pred", mkConcat [Projection (Argument 1) (Ix 1), Projection (Argument 2) (Projection (Argument 1) (Ix 2))]),
        (mkCId "Pred", mkConcat [Projection (Argument 1) (Ix 1), Projection (Projection (Argument 2) (Ix 1)) (Projection (Argument 1) (Ix 2))]),
        (mkCId "John", Tuple [Token "John", Ix 1]),
        (mkCId "We", Tuple [Token "we", Ix 2]),
        -- (mkCId "Walk", Tuple [Token "walks", Token "walk"])
        (mkCId "Walk", Tuple [Tuple [Token "walks", Token "walk"]])
      ]
    }),
    (mkCId "WalkingGer", Concr {
      -- lincats = Map.fromList [
      --   (mkCId "S", StrType),
      --   (mkCId "NP", ProductType [StrType, IxType 2, IxType 3]),
      --   (mkCId "VP", ProductType [ProductType [StrType, StrType, StrType], ProductType [StrType, StrType, StrType]])
      -- ],
      lins = Map.fromList [
        (mkCId "And", mkConcat [Argument 1, Token "und", Argument 2]),
        -- (mkCId "Pred", mkConcat [Projection (Argument 1) (Ix 1), Projection (Projection (Argument 2) (Projection (Argument 1) (Ix 2))) (Projection (Argument 1) (Ix 3))]),
        (mkCId "Pred", mkConcat [Projection (Argument 1) (Ix 1), Projection (Projection (Projection (Argument 2) (Ix 1)) (Projection (Argument 1) (Ix 2))) (Projection (Argument 1) (Ix 3))]),
        (mkCId "John", Tuple [Token "John", Ix 1, Ix 3]),
        (mkCId "We", Tuple [Token "wir", Ix 2, Ix 1]),
        -- (mkCId "Walk", Tuple [Tuple [Token "gehe", Token "gehst", Token "geht"], Tuple [Token "gehen", Token "geht", Token "gehen"]])
        (mkCId "Walk", Tuple [Tuple [Tuple [Token "gehe", Token "gehst", Token "geht"], Tuple [Token "gehen", Token "geht", Token "gehen"]]])
      ]
    })
  ]
}

-- | Helper for building concat trees
mkConcat :: [LinFun] -> LinFun
mkConcat [] = Empty
mkConcat [x] = x
mkConcat xs = foldl1 Concat xs
