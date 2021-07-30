module Fold where

import PGF2
import Data.Map as M (lookup, fromList)

--import Debug.Trace


foldable = fromList [(c, "bin_" ++ c) | c <- ops]
  where ops = words "plus times and or xor cartesian_product intersect union"

fold :: Expr -> Expr
fold t =
  case unApp t of
    Just (i,[x])  ->
        case M.lookup i foldable of
          Just j -> appFold j x
          _      -> mkApp i [fold x]
    Just (i,xs)   -> mkApp i $ map fold xs
    _             -> t

<<<<<<< HEAD
appFold :: Fun -> Expr -> Expr
appFold j t = 
=======
appFold :: CId -> Tree -> Tree
appFold j t =
>>>>>>> master
  case unApp t of
    Just (i,[t,ts]) | isPre i "Cons"  -> mkApp j [fold t, appFold j ts]
    Just (i,[t,s])  | isPre i "Base"  -> mkApp j [fold t, fold s]
  where isPre i s = take 4 (show i) == s
