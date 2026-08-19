{-# LANGUAGE BangPatterns #-}
module GF.Compile.TerminationCheck where

import GF.Grammar
import Debug.Trace

callGraph m c (ps,t) =
  let (_,xs) = foldl (\(i,xs) p -> (i+1,patts i EQ xs p)) (0,[]) ps
      cs     = calls m 0 xs t [] []
  in trace (show (c,cs)) $ return ()

patts i ord xs (PP _ ps)    = foldl (patts i LT) xs ps
patts i ord xs (PV x)
  | x /= identW             = (x,(i,ord)):xs
patts i ord xs (PR as)      = foldl (\xs (_,p) -> patts i ord xs p) xs as
patts i ord xs (PT ty p)    = patts i ord xs p
patts i ord xs (PAs x p)    = patts i ord ((x,(i,ord)):xs) p
patts i ord xs (PImplArg p) = patts i ord xs p
patts i ord xs (PSeq _ _ p1 _ _ p2) = patts i LT (patts i LT xs p1) p2
patts i ord xs _            = xs


calls m i xs (App t1 t2) args cs =
  let args' = case t2 of
                Vr x -> case lookup x xs of
                          Just (j,ord) -> (i,j,ord):args
                          Nothing -> args
                _    -> args
  in calls m (i+1) xs t1 args' (calls m 0 xs t2 [] cs)
calls m i xs (Q (m',q)) args cs
  | m == m' =
      let args' = [(i-i'-1,j,ord) | (i',j,ord) <- args]
      in (q,args') : cs
calls m i xs _ args cs = cs


matmul a b =
  sum [(i,k,mul ord1 ord2) | (i ,j,ord1) <- a
                           , (j',k,ord2) <- b
                           , j==j'
                           ]
      []
  where
    sum []                 ys = ys
    sum (x@(i,k,ord) : xs) ys = sum xs (accumulate ys)
      where
        accumulate []      = [x]
        accumulate (y@(i',k',ord') : ys)
          | i==i' && k==k' = let !sum = add ord ord'
                             in (i',k',sum):ys
          | otherwise      = y : accumulate ys

    add LT LT = LT
    add LT EQ = LT
    add LT GT = LT
    add EQ LT = LT
    add EQ EQ = EQ
    add EQ GT = EQ
    add GT LT = LT
    add GT EQ = EQ
    add GT GT = GT

    mul LT LT = LT
    mul LT EQ = LT
    mul LT GT = GT
    mul EQ LT = LT
    mul EQ EQ = EQ
    mul EQ GT = GT
    mul GT LT = GT
    mul GT EQ = GT
    mul GT GT = GT
