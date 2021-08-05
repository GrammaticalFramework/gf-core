{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable, ScopedTypeVariables #-}
-------------------------------------------------
-- |
-- Module      : PGF2
-- Maintainer  : Krasimir Angelov
-- Stability   : stable
-- Portability : portable
--
-- This module is an Application Programming Interface to
-- load and interpret grammars compiled in the Portable Grammar Format (PGF).
-- The PGF format is produced as the final output from the GF compiler.
-- The API is meant to be used for embedding GF grammars in Haskell
-- programs
-------------------------------------------------

module PGF2 (-- * PGF
             PGF,readPGF
            ) where

import Control.Exception(Exception,throwIO)
import PGF2.FFI

import Foreign
import Foreign.C
import Data.Typeable
import qualified Data.Map as Map

#include <pgf.h>

readPGF :: FilePath -> IO PGF
readPGF fpath =
  withCString fpath $ \c_fpath ->
  allocaBytes (#size PgfExn) $ \c_exn -> do
    c_pgf <- pgf_read c_fpath c_exn
    ex_type <- (#peek PgfExn, type) c_exn :: IO (#type PgfExnType)
    if ex_type == (#const PGF_EXN_NONE)
      then do fptr <- newForeignPtr pgf_free_fptr c_pgf
              return (PGF fptr Map.empty)
      else if ex_type == (#const PGF_EXN_SYSTEM_ERROR)
             then do errno <- (#peek PgfExn, code) c_exn
                     ioError (errnoToIOError "readPGF" (Errno errno) Nothing (Just fpath))
             else do c_msg <- (#peek PgfExn, msg) c_exn
                     msg <- peekCString c_msg
                     throwIO (PGFError msg)

-----------------------------------------------------------------------
-- Exceptions

newtype PGFError = PGFError String
     deriving (Show, Typeable)

instance Exception PGFError

