module GF.Command.TreeOperations (
  treeOp,
  allTreeOps,
  treeChunks
  ) where

import PGF(Expr,PGF,CId,compute,mkApp,unApp,unapply,unMeta,exprSize,exprFunctions)
import PGF.Data(Expr(EApp,EFun))
import PGF.TypeCheck(inferExpr)
import Data.List

type TreeOp = [Expr] -> [Expr]

treeOp :: PGF -> String -> Maybe (Either TreeOp (CId -> TreeOp))
treeOp pgf f = fmap snd $ lookup f $ allTreeOps pgf

allTreeOps :: PGF -> [(String,(String,Either TreeOp (CId -> TreeOp)))]
allTreeOps pgf = [
   ("compute",("compute by using semantic definitions (def)",
      Left  $ map (compute pgf))),
   ("transfer",("apply this transfer function to all maximal subtrees of suitable type",
      Right $ \f -> map (transfer pgf f))), -- HL 12/24, modified from gf-3.3
   ("largest",("sort trees from largest to smallest, in number of nodes",
      Left  $ largest)),
   ("nub\t",("remove duplicate trees",
      Left  $ nub)),
   ("smallest",("sort trees from smallest to largest, in number of nodes",
      Left  $ smallest)),
   ("subtrees",("return all fully applied subtrees (stopping at abstractions), by default sorted from the largest",
      Left  $ concatMap subtrees)),
   ("funs\t",("return all fun functions appearing in the tree, with duplications",
      Left  $ \es -> [mkApp f [] | e <- es, f <- exprFunctions e]))
  ]

largest :: [Expr] -> [Expr]
largest = reverse . smallest

smallest :: [Expr] -> [Expr]
smallest = sortBy (\t u -> compare (exprSize t) (exprSize u))

treeChunks :: Expr -> [Expr]
treeChunks = snd . cks where
  cks t = 
    case unapply t of
      (t, ts) -> case unMeta t of
                   Just _  -> (False,concatMap (snd . cks) ts)
                   Nothing -> case unzip (map cks ts) of
                                (bs,_) | and bs      -> (True, [t])
                                (_,cts)              -> (False,concat cts)

subtrees :: Expr -> [Expr]
subtrees t = t : case unApp t of
  Just (f,ts) -> concatMap subtrees ts
  _ -> []  -- don't go under abstractions

-- Apply transfer function f:C -> D to all maximal subtrees s:C of tree e and replace
-- these s by the values of f(s). This modifies the 'simple-minded transfer' of gf-3.3.
-- If applied to strict subtrees s of e, better use with f:C -> C only.  HL 12/2024

transfer :: PGF -> CId -> Expr -> Expr
transfer pgf f e = case inferExpr pgf (appf e) of
     Left _err -> case e of
                    EApp g a -> EApp (transfer pgf f g) (transfer pgf f a)
                    _ -> e
     Right _ty -> case (compute pgf (appf e)) of
                    v | v /= (appf e) -> v
                    _ -> e          -- default case of f, or f has no computation rule
 where
  appf = EApp (EFun f)
