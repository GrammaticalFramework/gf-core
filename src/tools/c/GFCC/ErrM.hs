-- BNF Converter: Error Monad
-- Copyright (C) 2004  Author:  Aarne Ranta

-- This file comes with NO WARRANTY and may be used FOR ANY PURPOSE.
module GFCC.ErrM where

-- Control.Monad.Fail import will become redundant in GHC 8.8+
import qualified Control.Monad.Fail as Fail


-- the Error monad: like Maybe type with error msgs

data Err a = Ok a | Bad String
  deriving (Read, Show, Eq)

instance Monad Err where
  return      = Ok
  Ok a  >>= f = f a
  Bad s >>= f = Bad s

#if !(MIN_VERSION_base(4,13,0))
  fail        = Bad
#endif

instance Fail.MonadFail Err where
  fail        = Bad

