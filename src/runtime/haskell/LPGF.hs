-- | Linearisation-only PGF format
-- Closely follows description in Section 2 of Angelov, Bringert, Ranta (2009)
-- "PGF: A Portable Run-Time Format for Type-Theoretical Grammars"
module LPGF (
  LPGF,
  abstractName,
  linearize, linearizeConcr,
  readLPGF,

  -- internal/testing
  encodeFile,
  zero
  ) where

import PGF (Language, readLanguage, showLanguage)
import PGF.CId
import PGF.Tree

import Control.Monad (forM_)
import qualified Data.Map as Map
import Text.Printf (printf)

-- | Linearisation-only PGF
data LPGF = LPGF {
  absname   :: CId,
  abstract  :: Abstr,
  concretes :: Map.Map CId Concr
} deriving (Read, Show)

-- | Abstract syntax
data Abstr = Abstr {
  cats :: Map.Map CId (),
  funs :: Map.Map CId Type
} deriving (Read, Show)

-- | Concrete syntax
data Concr = Concr {
  lincats :: Map.Map CId LinType, -- ^ assigning a linearization type to each category
  lins    :: Map.Map CId LinFun  -- ^ assigning a linearization function to each function
} deriving (Read, Show)

-- | Abstract function type
data Type = Type [CId] CId
  deriving (Read, Show)

-- | Linearisation type
data LinType =
    LTStr
  | LTInt Int
  | LTProduct [LinType]
  deriving (Read, Show)

-- | Linearisation function
data LinFun =
    LFEmpty
  | LFToken String
  | LFConcat LinFun LinFun
  | LFInt Int
  | LFTuple [LinFun]
  | LFProjection LinFun LinFun -- ^ In order for the projection to be well-formed, t1 must be a tuple and t2 an integer within the bounds of the size of the tuple
  | LFArgument Int
  deriving (Read, Show)

abstractName :: LPGF -> CId
abstractName = absname

encodeFile :: FilePath -> LPGF -> IO ()
encodeFile path lpgf = writeFile path (show lpgf)

readLPGF :: FilePath -> IO LPGF
readLPGF path = read <$> readFile path

-- | Helper for building concat trees
mkConcat :: [LinFun] -> LinFun
mkConcat [] = LFEmpty
mkConcat [x] = x
mkConcat xs = foldl1 LFConcat xs

-- | Main linearize function
linearize :: LPGF -> Language -> Tree -> String
linearize lpgf lang =
  case Map.lookup lang (concretes lpgf) of
    Just concr -> linearizeConcr concr
    Nothing -> error $ printf "Unknown language: %s" (showCId lang)

-- | Language-specific linearize function
-- Section 2.5
linearizeConcr :: Concr -> Tree -> String
linearizeConcr concr tree = lin2string $ lin tree
  where
    lin :: Tree -> LinFun
    lin tree = case tree of
      Fun f as -> v
        where
          Just t = Map.lookup f (lins concr)
          ts = map lin as
          v = eval ts t
      x -> error $ printf "Cannot lin %s" (prTree x)

-- | Evaluation context is a sequence of terms
type Context = [LinFun]

-- | Operational semantics, Table 2
eval :: Context -> LinFun -> LinFun
eval cxt t = case t of
  LFEmpty -> LFEmpty
  LFToken tok -> LFToken tok
  LFConcat s t -> LFConcat v w
    where
      v = eval cxt s
      w = eval cxt t
  LFInt i -> LFInt i
  LFTuple ts -> LFTuple vs
    where vs = map (eval cxt) ts
  LFProjection t u -> vs !! (i-1)
    where
      LFTuple vs = eval cxt t
      LFInt i = eval cxt u
  LFArgument i -> cxt !! (i-1)

-- | Turn concrete syntax terms into an actual string
lin2string :: LinFun -> String
lin2string l = case l of
  LFEmpty -> ""
  LFToken tok -> tok
  LFConcat l1 l2 -> unwords [lin2string l1, lin2string l2]
  x -> printf "[%s]" (show x)

---

main :: IO ()
main =
  forM_ [tree1, tree2, tree3] $ \tree -> do
    putStrLn (prTree tree)
    forM_ (Map.toList (concretes zero)) $ \(lang,concr) ->
      printf "%s: %s\n" (showLanguage lang) (linearizeConcr concr tree)
    putStrLn ""

-- Pred John Walk
tree1 :: Tree
tree1 = Fun (mkCId "Pred") [Fun (mkCId "John") [], Fun (mkCId "Walk") []]

-- Pred We Walk
tree2 :: Tree
tree2 = Fun (mkCId "Pred") [Fun (mkCId "We") [], Fun (mkCId "Walk") []]

-- And (Pred John Walk) (Pred We Walk)
tree3 :: Tree
tree3 = Fun (mkCId "And") [tree1, tree2]

-- Initial LPGF, Figures 6 & 7
zero :: LPGF
zero = LPGF {
  absname = mkCId "Zero",
  abstract = Abstr {
    cats = Map.fromList [
      (mkCId "S", ()),
      (mkCId "NP", ()),
      (mkCId "VP", ())
    ],
    funs = Map.fromList [
      (mkCId "And", Type [mkCId "S", mkCId "S"] (mkCId "S")),
      (mkCId "Pred", Type [mkCId "NP", mkCId "VP"] (mkCId "S")),
      (mkCId "John", Type [] (mkCId "NP")),
      (mkCId "We", Type [] (mkCId "NP")),
      (mkCId "Walk", Type [] (mkCId "VP"))
    ]
  },
  concretes = Map.fromList [
    (mkCId "ZeroEng", Concr {
      lincats = Map.fromList [
        (mkCId "S", LTStr),
        (mkCId "NP", LTProduct [LTStr, LTInt 2]),
        (mkCId "VP", LTProduct [LTStr, LTStr])
      ],
      lins = Map.fromList [
        (mkCId "And", mkConcat [LFArgument 1, LFToken "and", LFArgument 2]),
        (mkCId "Pred", mkConcat [LFProjection (LFArgument 1) (LFInt 1), LFProjection (LFArgument 2) (LFProjection (LFArgument 1) (LFInt 2))]),
        (mkCId "John", LFTuple [LFToken "John", LFInt 1]),
        (mkCId "We", LFTuple [LFToken "we", LFInt 2]),
        (mkCId "Walk", LFTuple [LFToken "walks", LFToken "walk"])
      ]
    }),
    (mkCId "ZeroGer", Concr {
      lincats = Map.fromList [
        (mkCId "S", LTStr),
        (mkCId "NP", LTProduct [LTStr, LTInt 2, LTInt 3]),
        (mkCId "VP", LTProduct [LTProduct [LTStr, LTStr, LTStr], LTProduct [LTStr, LTStr, LTStr]])
      ],
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
