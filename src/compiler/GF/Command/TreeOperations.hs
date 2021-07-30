module GF.Command.TreeOperations (
  treeOp,
  allTreeOps,
  ) where

import PGF2(Expr,PGF,Fun,compute,mkApp,unApp,unMeta,exprSize,exprFunctions)
import Data.List

type TreeOp = [Expr] -> [Expr]

treeOp :: PGF -> String -> Maybe (Either TreeOp (Fun -> TreeOp))
treeOp pgf f = fmap snd $ lookup f $ allTreeOps pgf

allTreeOps :: PGF -> [(String,(String,Either TreeOp (Fun -> TreeOp)))]
allTreeOps pgf = [
   ("compute",("compute by using semantic definitions (def)",
      Left  $ map (compute pgf))),
   ("largest",("sort trees from largest to smallest, in number of nodes",
      Left  $ largest)),
   ("nub",("remove duplicate trees",
      Left  $ nub)),
   ("smallest",("sort trees from smallest to largest, in number of nodes",
      Left  $ smallest)),
   ("subtrees",("return all fully applied subtrees (stopping at abstractions), by default sorted from the largest",
      Left  $ concatMap subtrees)),
   ("funs",("return all fun functions appearing in the tree, with duplications",
      Left  $ \es -> [mkApp f [] | e <- es, f <- exprFunctions e]))
  ]

largest :: [Expr] -> [Expr]
largest = reverse . smallest

smallest :: [Expr] -> [Expr]
smallest = sortBy (\t u -> compare (exprSize t) (exprSize u))

subtrees :: Expr -> [Expr]
subtrees t = t : case unApp t of
  Just (f,ts) -> concatMap subtrees ts
  _ -> []  -- don't go under abstractions
