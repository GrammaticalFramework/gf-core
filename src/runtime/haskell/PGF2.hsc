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
             PGF,readPGF,bootNGF,readNGF,

             -- * Abstract syntax
             AbsName,abstractName,
             -- ** Categories
             Cat,categories,
             -- ** Functions
             Fun, functions, functionsByCat,

             -- * Concrete syntax
             ConcName
            ) where

import Control.Exception(Exception,throwIO,mask_,bracket)
import System.IO.Unsafe(unsafePerformIO)
import PGF2.Expr
import PGF2.FFI

import Foreign
import Foreign.C
import Data.Typeable
import qualified Data.Map as Map
import Data.IORef

#include <pgf.h>

type AbsName  = String -- ^ Name of abstract syntax
type ConcName = String -- ^ Name of concrete syntax

-- | Reads a PGF file and keeps it in memory.
readPGF :: FilePath -> IO PGF
readPGF fpath =
  withCString fpath $ \c_fpath ->
  allocaBytes (#size PgfExn) $ \c_exn ->
  mask_ $ do
    c_pgf <- pgf_read_pgf c_fpath c_exn
    ex_type <- (#peek PgfExn, type) c_exn :: IO (#type PgfExnType)
    if ex_type == (#const PGF_EXN_NONE)
      then do fptr <- newForeignPtr pgf_free_fptr c_pgf
              return (PGF fptr Map.empty)
      else if ex_type == (#const PGF_EXN_SYSTEM_ERROR)
             then do errno <- (#peek PgfExn, code) c_exn
                     ioError (errnoToIOError "readPGF" (Errno errno) Nothing (Just fpath))
             else do c_msg <- (#peek PgfExn, msg) c_exn
                     msg <- peekCString c_msg
                     free c_msg
                     throwIO (PGFError msg)

-- | Reads a PGF file and stores the unpacked data in an NGF file
-- ready to be shared with other process, or used for quick startup.
-- The NGF file is platform dependent and should not be copied
-- between machines.
bootNGF :: FilePath -> FilePath -> IO PGF
bootNGF pgf_path ngf_path =
  withCString pgf_path $ \c_pgf_path ->
  withCString ngf_path $ \c_ngf_path ->
  allocaBytes (#size PgfExn) $ \c_exn ->
  mask_ $ do
    c_pgf <- pgf_boot_ngf c_pgf_path c_ngf_path c_exn
    ex_type <- (#peek PgfExn, type) c_exn :: IO (#type PgfExnType)
    if ex_type == (#const PGF_EXN_NONE)
      then do fptr <- newForeignPtr pgf_free_fptr c_pgf
              return (PGF fptr Map.empty)
      else if ex_type == (#const PGF_EXN_SYSTEM_ERROR)
             then do errno <- (#peek PgfExn, code) c_exn
                     ioError (errnoToIOError "bootNGF" (Errno errno) Nothing (Just pgf_path))
             else do c_msg <- (#peek PgfExn, msg) c_exn
                     msg <- peekCString c_msg
                     free c_msg
                     throwIO (PGFError msg)

-- | Tries to read the grammar from an already booted NGF file.
-- If the file does not exist then a new one is created, and the
-- grammar is set to be empty. It can later be populated with
-- rules dynamically.
readNGF :: FilePath -> IO PGF
readNGF fpath =
  withCString fpath $ \c_fpath ->
  allocaBytes (#size PgfExn) $ \c_exn ->
  mask_ $ do
    c_pgf <- pgf_read_ngf c_fpath c_exn
    ex_type <- (#peek PgfExn, type) c_exn :: IO (#type PgfExnType)
    if ex_type == (#const PGF_EXN_NONE)
      then do fptr <- newForeignPtr pgf_free_fptr c_pgf
              return (PGF fptr Map.empty)
      else if ex_type == (#const PGF_EXN_SYSTEM_ERROR)
             then do errno <- (#peek PgfExn, code) c_exn
                     ioError (errnoToIOError "readPGF" (Errno errno) Nothing (Just fpath))
             else do c_msg <- (#peek PgfExn, msg) c_exn
                     msg <- peekCString c_msg
                     free c_msg
                     throwIO (PGFError msg)

-- | The abstract language name is the name of the top-level
-- abstract module
abstractName :: PGF -> AbsName
abstractName p =
  unsafePerformIO $
  withForeignPtr (a_pgf p) $ \p_pgf ->
  bracket (pgf_abstract_name p_pgf) free $ \c_text ->
    peekText c_text

-- | List of all functions defined in the abstract syntax
categories :: PGF -> [Fun]
categories p =
  unsafePerformIO $ do
    ref <- newIORef []
    (allocaBytes (#size PgfItor) $ \itor ->
     bracket (wrapItorCallback (getCategories ref)) freeHaskellFunPtr $ \fptr ->
     withForeignPtr (a_pgf p) $ \p_pgf -> do
      (#poke PgfItor, fn) itor fptr
      pgf_iter_categories p_pgf itor
      cs <- readIORef ref
      return (reverse cs))
  where
    getCategories :: IORef [String] -> ItorCallback
    getCategories ref itor key = do
      names <- readIORef ref
      name  <- peekText key
      writeIORef ref $ (name : names)

-- | List of all functions defined in the abstract syntax
functions :: PGF -> [Fun]
functions p =
  unsafePerformIO $ do
    ref <- newIORef []
    (allocaBytes (#size PgfItor) $ \itor ->
     bracket (wrapItorCallback (getFunctions ref)) freeHaskellFunPtr $ \fptr ->
     withForeignPtr (a_pgf p) $ \p_pgf -> do
      (#poke PgfItor, fn) itor fptr
      pgf_iter_functions p_pgf itor
      fs <- readIORef ref
      return (reverse fs))
  where
    getFunctions :: IORef [String] -> ItorCallback
    getFunctions ref itor key = do
      names <- readIORef ref
      name  <- peekText key
      writeIORef ref $ (name : names)

-- | List of all functions defined in the abstract syntax
functionsByCat :: PGF -> Cat -> [Fun]
functionsByCat p cat =
  unsafePerformIO $ do
    ref <- newIORef []
    (withText cat $ \c_cat ->
     allocaBytes (#size PgfItor) $ \itor ->
     bracket (wrapItorCallback (getFunctions ref)) freeHaskellFunPtr $ \fptr ->
     withForeignPtr (a_pgf p) $ \p_pgf -> do
      (#poke PgfItor, fn) itor fptr
      pgf_iter_functions_by_cat p_pgf c_cat itor
      fs <- readIORef ref
      return (reverse fs))
  where
    getFunctions :: IORef [String] -> ItorCallback
    getFunctions ref itor key = do
      names <- readIORef ref
      name  <- peekText key
      writeIORef ref $ (name : names)

-----------------------------------------------------------------------
-- Exceptions

newtype PGFError = PGFError String
     deriving (Show, Typeable)

instance Exception PGFError

