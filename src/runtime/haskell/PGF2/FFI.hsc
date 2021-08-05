{-# LANGUAGE ForeignFunctionInterface, MagicHash, BangPatterns #-}

module PGF2.FFI where

import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import qualified Data.Map as Map

-- | An abstract data type representing multilingual grammar
-- in Portable Grammar Format.
data PGF = PGF {a_pgf :: ForeignPtr PgfPGF, langs :: Map.Map String Concr}
data Concr = Concr {c_pgf :: ForeignPtr PgfPGF, concr :: Ptr PgfConcr}

------------------------------------------------------------------
-- libpgf API

data PgfExn
data PgfPGF
data PgfConcr

foreign import ccall "pgf.h pgf_read"
  pgf_read :: CString -> Ptr PgfExn -> IO (Ptr PgfPGF)

foreign import ccall "&pgf_free"
  pgf_free_fptr :: FinalizerPtr PgfPGF

