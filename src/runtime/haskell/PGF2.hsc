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
             Cat,categories,categoryContext,categoryProb,
             -- ** Functions
             Fun, functions, functionsByCat,
             functionType, functionIsConstructor, functionProb,
             -- ** Expressions
             Expr(..), Literal(..), showExpr, readExpr,
             mkAbs,    unAbs,
             mkApp,    unApp, unapply,
             mkStr,    unStr,
             mkInt,    unInt,
             mkDouble, unDouble,
             mkFloat,  unFloat,
             mkMeta,   unMeta,
             -- extra
             exprSize, exprFunctions,
             -- ** Types
             Type(..), Hypo, BindType(..), startCat,
             readType, showType,
             mkType, unType,
             mkHypo, mkDepHypo, mkImplHypo,

             createFunction,

             -- * Concrete syntax
             ConcName,

             -- * Exceptions
             PGFError(..)
             ) where

import Control.Exception(mask_,bracket)
import System.IO.Unsafe(unsafePerformIO)
import PGF2.Expr
import PGF2.FFI

import Foreign
import Foreign.C
import qualified Foreign.Concurrent as C
import qualified Data.Map as Map
import Data.IORef

#include <pgf/pgf.h>

type AbsName  = String -- ^ Name of abstract syntax
type ConcName = String -- ^ Name of concrete syntax

-- | Reads a PGF file and keeps it in memory.
readPGF :: FilePath -> IO PGF
readPGF fpath =
  withCString fpath $ \c_fpath ->
  alloca $ \p_revision ->
  mask_ $ do
    c_pgf <- withPgfExn fpath (pgf_read_pgf c_fpath p_revision)
    c_revision <- peek p_revision
    fptr1 <- newForeignPtr pgf_free_fptr c_pgf
    fptr2 <- C.newForeignPtr c_revision (withForeignPtr fptr1 (\c_db -> pgf_free_revision c_db c_revision))
    return (PGF fptr1 fptr2 Map.empty)

-- | Reads a PGF file and stores the unpacked data in an NGF file
-- ready to be shared with other process, or used for quick startup.
-- The NGF file is platform dependent and should not be copied
-- between machines.
bootNGF :: FilePath -> FilePath -> IO PGF
bootNGF pgf_path ngf_path =
  withCString pgf_path $ \c_pgf_path ->
  withCString ngf_path $ \c_ngf_path ->
  alloca $ \p_revision ->
  mask_ $ do
    c_pgf <- withPgfExn pgf_path (pgf_boot_ngf c_pgf_path c_ngf_path p_revision)
    c_revision <- peek p_revision
    fptr1 <- newForeignPtr pgf_free_fptr c_pgf
    fptr2 <- C.newForeignPtr c_revision (withForeignPtr fptr1 (\c_db -> pgf_free_revision c_db c_revision))
    return (PGF fptr1 fptr2 Map.empty)

-- | Tries to read the grammar from an already booted NGF file.
-- If the file does not exist then a new one is created, and the
-- grammar is set to be empty. It can later be populated with
-- rules dynamically.
readNGF :: FilePath -> IO PGF
readNGF fpath =
  withCString fpath $ \c_fpath ->
  alloca $ \p_revision ->
  mask_ $ do
    c_db <- withPgfExn fpath (pgf_read_ngf c_fpath p_revision)
    c_revision <- peek p_revision
    fptr1 <- newForeignPtr pgf_free_fptr c_db
    fptr2 <- C.newForeignPtr c_revision (withForeignPtr fptr1 (\c_db -> pgf_free_revision c_db c_revision))
    return (PGF fptr1 fptr2 Map.empty)

-- | The abstract language name is the name of the top-level
-- abstract module
abstractName :: PGF -> AbsName
abstractName p =
  unsafePerformIO $
  withForeignPtr (a_db p) $ \c_db ->
  withForeignPtr (revision p) $ \c_revision ->
  bracket (pgf_abstract_name c_db c_revision) free $ \c_text ->
    peekText c_text

-- | The start category is defined in the grammar with
-- the \'startcat\' flag. This is usually the sentence category
-- but it is not necessary. Despite that there is a start category
-- defined you can parse with any category. The start category
-- definition is just for convenience.
startCat :: PGF -> Type
startCat p =
  unsafePerformIO $
  withForeignPtr unmarshaller $ \u ->
  withForeignPtr (a_db p) $ \c_db ->
  withForeignPtr (revision p) $ \c_revision -> do
    c_typ <- pgf_start_cat c_db c_revision u
    typ <- deRefStablePtr c_typ
    freeStablePtr c_typ
    return typ

-- | The type of a function
functionType :: PGF -> Fun -> Maybe Type
functionType p fn =
  unsafePerformIO $
  withForeignPtr unmarshaller $ \u ->
  withForeignPtr (a_db p) $ \c_db ->
  withForeignPtr (revision p) $ \c_revision ->
  withText fn $ \c_fn -> do
    c_typ <- pgf_function_type c_db c_revision c_fn u
    if c_typ == castPtrToStablePtr nullPtr
      then return Nothing
      else do typ <- deRefStablePtr c_typ
              freeStablePtr c_typ
              return (Just typ)

functionIsConstructor :: PGF -> Fun -> Bool
functionIsConstructor p fun =
  unsafePerformIO $
  withText fun $ \c_fun ->
  withForeignPtr (a_db p) $ \c_db ->
  withForeignPtr (revision p) $ \c_revision ->
      do res <- pgf_function_is_constructor c_db c_revision c_fun
         return (res /= 0)

functionProb :: PGF -> Fun -> Float
functionProb p fun =
  unsafePerformIO $
  withText fun $ \c_fun ->
  withForeignPtr (a_db p) $ \c_db ->
  withForeignPtr (revision p) $ \c_revision ->
      do c_prob <- pgf_function_prob c_db c_revision c_fun
         return (realToFrac c_prob)

-- | List of all functions defined in the abstract syntax
categories :: PGF -> [Cat]
categories p =
  unsafePerformIO $ do
    ref <- newIORef []
    (allocaBytes (#size PgfItor) $ \itor ->
     bracket (wrapItorCallback (getCategories ref)) freeHaskellFunPtr $ \fptr ->
     withForeignPtr (a_db p) $ \c_db ->
     withForeignPtr (revision p) $ \c_revision -> do
      (#poke PgfItor, fn) itor fptr
      withPgfExn "" (pgf_iter_categories c_db c_revision itor)
      cs <- readIORef ref
      return (reverse cs))
  where
    getCategories :: IORef [String] -> ItorCallback
    getCategories ref itor key exn = do
      names <- readIORef ref
      name  <- peekText key
      writeIORef ref $ (name : names)

categoryContext :: PGF -> Cat -> [Hypo]
categoryContext p cat =
  unsafePerformIO $
  withText cat $ \c_cat ->
  alloca $ \p_n_hypos ->
  withForeignPtr unmarshaller $ \u ->
  withForeignPtr (a_db p) $ \c_db ->
  withForeignPtr (revision p) $ \c_revision ->
  mask_ $ do
    c_hypos <- pgf_category_context c_db c_revision c_cat p_n_hypos u
    if c_hypos == nullPtr
      then return []
      else do n_hypos <- peek p_n_hypos
              hypos <- peekHypos c_hypos 0 n_hypos
              free c_hypos
              return hypos
  where
    peekHypos :: Ptr PgfTypeHypo -> CSize -> CSize -> IO [Hypo]
    peekHypos c_hypo i n
      | i < n     = do c_cat <- (#peek PgfTypeHypo, cid) c_hypo
                       cat <- peekText c_cat
                       free c_cat
                       c_ty <- (#peek PgfTypeHypo, type) c_hypo
                       ty <- deRefStablePtr c_ty
                       freeStablePtr c_ty
                       bt  <- fmap unmarshalBindType ((#peek PgfTypeHypo, bind_type) c_hypo)
                       hs <- peekHypos (plusPtr c_hypo (#size PgfTypeHypo)) (i+1) n
                       return ((bt,cat,ty) : hs)
      | otherwise = return []

categoryProb :: PGF -> Cat -> Float
categoryProb p cat =
  unsafePerformIO $
  withText cat $ \c_cat ->
  withForeignPtr (a_db p) $ \c_db ->
  withForeignPtr (revision p) $ \c_revision ->
      do c_prob <- pgf_category_prob c_db c_revision c_cat
         return (realToFrac c_prob)

-- | List of all functions defined in the abstract syntax
functions :: PGF -> [Fun]
functions p =
  unsafePerformIO $ do
    ref <- newIORef []
    (allocaBytes (#size PgfItor) $ \itor ->
     bracket (wrapItorCallback (getFunctions ref)) freeHaskellFunPtr $ \fptr ->
     withForeignPtr (a_db p) $ \c_db ->
     withForeignPtr (revision p) $ \c_revision -> do
      (#poke PgfItor, fn) itor fptr
      withPgfExn "" (pgf_iter_functions c_db c_revision itor)
      fs <- readIORef ref
      return (reverse fs))
  where
    getFunctions :: IORef [String] -> ItorCallback
    getFunctions ref itor key exn = do
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
     withForeignPtr (a_db p) $ \c_db ->
     withForeignPtr (revision p) $ \c_revision -> do
      (#poke PgfItor, fn) itor fptr
      withPgfExn "" (pgf_iter_functions_by_cat c_db c_revision c_cat itor)
      fs <- readIORef ref
      return (reverse fs))
  where
    getFunctions :: IORef [String] -> ItorCallback
    getFunctions ref itor key exn = do
      names <- readIORef ref
      name  <- peekText key
      writeIORef ref $ (name : names)

-----------------------------------------------------------------------
-- Expressions & types

-- | renders an expression as a 'String'. The list
-- of identifiers is the list of all free variables
-- in the expression in order reverse to the order
-- of binding.
showExpr :: [Var] -> Expr -> String
showExpr scope e =
  unsafePerformIO $
  withForeignPtr marshaller $ \m ->
  bracket (newPrintCtxt scope) freePrintCtxt $ \pctxt ->
  bracket (newStablePtr e) freeStablePtr $ \c_e ->
  bracket (pgf_print_expr c_e pctxt 1 m) free $ \c_text ->
    peekText c_text

newPrintCtxt :: [Var] -> IO (Ptr PgfPrintContext)
newPrintCtxt []     = return nullPtr
newPrintCtxt (x:xs) = do
  pctxt <- newTextEx (#offset PgfPrintContext, name) x
  newPrintCtxt xs >>= (#poke PgfPrintContext, next) pctxt
  return pctxt

freePrintCtxt :: Ptr PgfPrintContext -> IO ()
freePrintCtxt pctxt
  | pctxt == nullPtr = return ()
  | otherwise        = do
      (#peek PgfPrintContext, next) pctxt >>= freePrintCtxt
      free pctxt

-- | parses a 'String' as an expression
readExpr :: String -> Maybe Expr
readExpr str =
  unsafePerformIO $
  withText str $ \c_str ->
  withForeignPtr unmarshaller $ \u ->
  mask_ $ do
    c_expr <- pgf_read_expr c_str u
    if c_expr == castPtrToStablePtr nullPtr
      then return Nothing
      else do expr <- deRefStablePtr c_expr
              freeStablePtr c_expr
              return (Just expr)

-- | renders a type as a 'String'. The list
-- of identifiers is the list of all free variables
-- in the type in order reverse to the order
-- of binding.
showType :: [Var] -> Type -> String
showType scope ty =
  unsafePerformIO $
  withForeignPtr marshaller $ \m ->
  bracket (newPrintCtxt scope) freePrintCtxt $ \pctxt ->
  bracket (newStablePtr ty) freeStablePtr $ \c_ty ->
  bracket (pgf_print_type c_ty pctxt 0 m) free $ \c_text ->
    peekText c_text

-- | parses a 'String' as a type
readType :: String -> Maybe Type
readType str =
  unsafePerformIO $
  withText str $ \c_str ->
  withForeignPtr unmarshaller $ \u -> do
    c_ty <- pgf_read_type c_str u
    if c_ty == castPtrToStablePtr nullPtr
      then return Nothing
      else do ty <- deRefStablePtr c_ty
              freeStablePtr c_ty
              return (Just ty)

createFunction :: PGF -> Fun -> Type -> Float -> IO ()
createFunction p name ty prob =
  withForeignPtr (a_db p) $ \c_db ->
  withForeignPtr (revision p) $ \c_revision ->
  withText name $ \c_name ->
  bracket (newStablePtr ty) freeStablePtr $ \c_ty ->
  withForeignPtr marshaller $ \m -> do
    pgf_create_function c_db c_revision c_name c_ty prob m
