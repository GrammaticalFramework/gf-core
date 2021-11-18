module PGF2.ByteCode(-- * Byte code
                     CodeLabel, Instr(..), IVal(..), TailInfo(..),
                    ) where

import PGF2.Expr

type CodeLabel = Int

data Instr
  = CHECK_ARGS {-# UNPACK #-} !Int
  | CASE Fun  {-# UNPACK #-} !CodeLabel
  | CASE_LIT Literal  {-# UNPACK #-} !CodeLabel
  | SAVE {-# UNPACK #-} !Int
  | ALLOC  {-# UNPACK #-} !Int
  | PUT_CONSTR Fun
  | PUT_CLOSURE   {-# UNPACK #-} !CodeLabel
  | PUT_LIT Literal
  | SET IVal
  | SET_PAD
  | PUSH_FRAME
  | PUSH IVal
  | TUCK IVal {-# UNPACK #-} !Int
  | EVAL IVal TailInfo
  | DROP {-# UNPACK #-} !Int
  | JUMP {-# UNPACK #-} !CodeLabel
  | FAIL
  | PUSH_ACCUM Literal
  | POP_ACCUM
  | ADD

data IVal
  = HEAP     {-# UNPACK #-} !Int
  | ARG_VAR  {-# UNPACK #-} !Int
  | FREE_VAR {-# UNPACK #-} !Int
  | GLOBAL   Fun
  deriving Eq

data TailInfo
  = RecCall
  | TailCall {-# UNPACK #-} !Int
  | UpdateCall
