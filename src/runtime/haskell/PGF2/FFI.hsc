{-# LANGUAGE ForeignFunctionInterface, MagicHash, BangPatterns #-}

module PGF2.FFI where

import Data.Word
import Foreign ( alloca, peek, poke, peekByteOff )
import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import qualified Data.Map as Map

#include <pgf.h>

-- | An abstract data type representing multilingual grammar
-- in Portable Grammar Format.
data PGF = PGF {a_pgf :: ForeignPtr PgfPGF, langs :: Map.Map String Concr}
data Concr = Concr {c_pgf :: ForeignPtr PgfPGF, concr :: Ptr PgfConcr}

------------------------------------------------------------------
-- libpgf API

data PgfExn
data PgfText
data PgfPGF
data PgfConcr

foreign import ccall unsafe "pgf_utf8_decode"
  pgf_utf8_decode :: Ptr CString -> IO Word32

foreign import ccall "pgf.h pgf_read"
  pgf_read :: CString -> Ptr PgfExn -> IO (Ptr PgfPGF)

foreign import ccall "&pgf_free"
  pgf_free_fptr :: FinalizerPtr PgfPGF

foreign import ccall "pgf/pgf.h pgf_abstract_name"
  pgf_abstract_name :: Ptr PgfPGF -> IO (Ptr PgfText)

peekText :: Ptr PgfText -> IO String
peekText ptr =
  alloca $ \pptr -> do
    size <- ((#peek PgfText, size) ptr) :: IO CSize
    let c_text = castPtr ptr `plusPtr` (#offset PgfText, text)
    poke pptr c_text
    decode pptr (c_text `plusPtr` fromIntegral size)
  where
    decode pptr end = do
      ptr <- peek pptr
      if ptr >= end
        then return []
        else do x <- pgf_utf8_decode pptr
                cs <- decode pptr end
                return (((toEnum . fromEnum) x) : cs)
