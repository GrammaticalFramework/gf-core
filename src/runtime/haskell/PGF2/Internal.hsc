{-# LANGUAGE ImplicitParams, RankNTypes #-}

module PGF2.Internal(-- * Access the internal structures
                     FId,isPredefFId,
                     fidString,fidInt,fidFloat,fidVar,fidStart,

                     -- * Byte code
                     CodeLabel, Instr(..), IVal(..), TailInfo(..),

                     unionPGF, writeConcr
                    ) where

import PGF2.FFI
import PGF2.Expr

type FId = Int

fidString, fidInt, fidFloat, fidVar, fidStart :: FId
fidString = (-1)
fidInt    = (-2)
fidFloat  = (-3)
fidVar    = (-4)
fidStart  = (-5)

isPredefFId :: FId -> Bool
isPredefFId = (`elem` [fidString, fidInt, fidFloat, fidVar])

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

unionPGF :: PGF -> PGF -> Maybe PGF
unionPGF = error "TODO: unionPGF"

writeConcr :: FilePath -> Concr -> IO ()
writeConcr = error "TODO: writeConcr"
