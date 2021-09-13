{-# LANGUAGE ImplicitParams, RankNTypes #-}

module PGF2.Internal(-- * Access the internal structures
                     FId,isPredefFId,
                     fidString,fidInt,fidFloat,fidVar,fidStart,

                     -- * Byte code
                     CodeLabel, Instr(..), IVal(..), TailInfo(..),

                     SeqId,LIndex,
                     FunId,Token,Production(..),PArg(..),Symbol(..),

                     unionPGF, writeConcr
                    ) where

import PGF2.FFI
import PGF2.Expr

type Token  = String
type LIndex = Int
data Symbol
  = SymCat {-# UNPACK #-} !Int {-# UNPACK #-} !LIndex
  | SymLit {-# UNPACK #-} !Int {-# UNPACK #-} !LIndex
  | SymVar {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | SymKS Token
  | SymKP [Symbol] [([Symbol],[String])]
  | SymBIND                         -- the special BIND token
  | SymNE                           -- non exist
  | SymSOFT_BIND                    -- the special SOFT_BIND token
  | SymSOFT_SPACE                   -- the special SOFT_SPACE token
  | SymCAPIT                        -- the special CAPIT token
  | SymALL_CAPIT                    -- the special ALL_CAPIT token
  deriving (Eq,Ord,Show)
data Production
  = PApply  {-# UNPACK #-} !FunId [PArg]
  | PCoerce {-# UNPACK #-} !FId
  deriving (Eq,Ord,Show)
type FunId = Int
type SeqId = Int
type FId = Int
data PArg = PArg [FId] {-# UNPACK #-} !FId deriving (Eq,Ord,Show)

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
