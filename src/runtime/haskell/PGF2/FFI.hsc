{-# LANGUAGE ForeignFunctionInterface, MagicHash, BangPatterns #-}

module PGF2.FFI where

import Data.Word
import Foreign
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
data PgfItor
data PgfPGF
data PgfConcr

foreign import ccall unsafe "pgf_utf8_decode"
  pgf_utf8_decode :: Ptr CString -> IO Word32

foreign import ccall unsafe "pgf_utf8_encode"
  pgf_utf8_encode :: Word32 -> Ptr CString -> IO ()

foreign import ccall "pgf_read_pgf"
  pgf_read_pgf :: CString -> Ptr PgfExn -> IO (Ptr PgfPGF)

foreign import ccall "pgf_boot_ngf"
  pgf_boot_ngf :: CString -> CString -> Ptr PgfExn -> IO (Ptr PgfPGF)

foreign import ccall "pgf_read_ngf"
  pgf_read_ngf :: CString -> Ptr PgfExn -> IO (Ptr PgfPGF)

foreign import ccall "&pgf_free"
  pgf_free_fptr :: FinalizerPtr PgfPGF

foreign import ccall "pgf_abstract_name"
  pgf_abstract_name :: Ptr PgfPGF -> IO (Ptr PgfText)

type ItorCallback = Ptr PgfItor -> Ptr PgfText -> IO ()

foreign import ccall "wrapper"
  wrapItorCallback :: ItorCallback -> IO (FunPtr ItorCallback)

foreign import ccall "pgf_iter_categories"
  pgf_iter_categories :: Ptr PgfPGF -> Ptr PgfItor -> IO ()

foreign import ccall "pgf_iter_functions"
  pgf_iter_functions :: Ptr PgfPGF -> Ptr PgfItor -> IO ()

foreign import ccall "pgf_iter_functions_by_cat"
  pgf_iter_functions_by_cat :: Ptr PgfPGF -> Ptr PgfText -> Ptr PgfItor -> IO ()

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

withText :: String -> (Ptr PgfText -> IO a) -> IO a
withText s fn =
  allocaBytes ((#size PgfText) + size + 1) $ \ptr -> do
    (#poke PgfText, size) ptr (fromIntegral size :: CSize)
    pokeUtf8CString s (ptr `plusPtr` (#const offsetof(PgfText, text)))
    fn ptr
  where
    size = utf8Length s

    pokeUtf8CString s ptr =
      alloca $ \pptr ->
        poke pptr ptr >> encode s pptr
      where
        encode []     pptr = do
          pgf_utf8_encode 0 pptr
        encode (c:cs) pptr = do
          pgf_utf8_encode ((toEnum . fromEnum) c) pptr
          encode cs pptr

    utf8Length s = count 0 s
      where
        count !c []         = c
        count !c (x:xs)
          | ucs < 0x80      = count (c+1) xs
          | ucs < 0x800     = count (c+2) xs
          | ucs < 0x10000   = count (c+3) xs
          | ucs < 0x200000  = count (c+4) xs
          | ucs < 0x4000000 = count (c+5) xs
          | otherwise       = count (c+6) xs
          where
            ucs = fromEnum x
