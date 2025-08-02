----------------------------------------------------------------------
-- |
-- Module      : BacktrackM
-- Maintainer  : PL
-- Stability   : (stable)
-- Portability : (portable)
--
-- > CVS $Date: 2005/04/21 16:22:00 $
-- > CVS $Author: bringert $
-- > CVS $Revision: 1.4 $
--
-- Backtracking state monad, with r\/o environment
-----------------------------------------------------------------------------

{-# LANGUAGE Rank2Types, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE CPP #-}
module GF.Data.BacktrackM (
                    -- * the backtracking state monad
                    BacktrackM,
                    -- * monad specific utilities
                    member,
                    cut,
                    -- * running the monad
                    foldBM,          runBM,
                    foldSolutions,   solutions,
                    foldFinalStates, finalStates,

                    -- * reexport the 'MonadState' class
                    module Control.Monad.State.Class,
                  ) where

import Data.List
import Control.Applicative
import Control.Monad
import Control.Monad.State.Class
import qualified Control.Monad.Fail as Fail

----------------------------------------------------------------------
-- Combining endomorphisms and continuations
-- a la Ralf Hinze

-- BacktrackM = state monad transformer over the backtracking monad

newtype BacktrackM s a = BM (forall b . (a -> s -> b -> b) -> s -> b -> b)

-- * running the monad

runBM :: BacktrackM s a -> s -> [(s,a)]
runBM (BM m) s = m (\x s xs -> (s,x) : xs) s []

foldBM :: (a -> s -> b -> b) -> b -> BacktrackM s a -> s -> b
foldBM f b (BM m) s = m f s b

foldSolutions   :: (a -> b -> b) -> b -> BacktrackM s a -> s -> b
foldSolutions f b (BM m) s = m (\x s b -> f x b) s b

solutions   :: BacktrackM s a  -> s -> [a]
solutions = foldSolutions (:) []

foldFinalStates :: (s -> b -> b) -> b -> BacktrackM s () -> s -> b
foldFinalStates f b (BM m) s = m (\x s b -> f s b) s b

finalStates :: BacktrackM s () -> s -> [s]
finalStates bm = map fst . runBM bm

instance Applicative (BacktrackM s) where
    pure a = BM (\c s b -> c a s b)
    (<*>) = ap

instance Monad (BacktrackM s) where
    return     = pure
    BM m >>= k = BM (\c s b -> m (\a s b -> unBM (k a) c s b) s b)
        where unBM (BM m) = m

#if !(MIN_VERSION_base(4,13,0))
    fail = Fail.fail
#endif

instance Fail.MonadFail (BacktrackM s) where
    fail _ = mzero

instance Functor (BacktrackM s) where
    fmap f (BM m) = BM (\c s b -> m (\a s b -> c (f a) s b) s b)

instance Alternative (BacktrackM s) where
   empty = mzero
   (<|>) = mplus

instance MonadPlus (BacktrackM s) where
    mzero                 = BM (\c s b -> b)
    (BM f) `mplus` (BM g) = BM (\c s b -> g c s $! f c s b)

instance MonadState s (BacktrackM s) where
  get = BM (\c s b -> c s s b)
  put s = BM (\c _ b -> c () s b)

-- * specific functions on the backtracking monad

member :: [a] -> BacktrackM s a
member xs = BM (\c s b -> foldl' (\b x -> c x s b) b xs)

cut :: BacktrackM s a -> BacktrackM s [(s,a)]
cut f = BM (\c s b -> c (runBM f s) s b)
