module PGF2.Transactions
          ( Transaction

            -- abstract syntax
          , modifyPGF
          , branchPGF
          , checkoutPGF
          , createFunction
          , dropFunction
          , createCategory
          , dropCategory
          , setGlobalFlag
          , setAbstractFlag

            -- concrete syntax
          , Token, LIndex, LVar, LParam(..)
          , PArg(..), Symbol(..), Production(..)

          , createConcrete
          , alterConcrete
          , dropConcrete
          , setConcreteFlag
          , createLin
          ) where

import PGF2.FFI
import PGF2.Expr

import Foreign
import Foreign.C
import Control.Exception

#include <pgf/pgf.h>

newtype Transaction k a =
  Transaction (Ptr PgfDB -> Ptr (PgfRevision k) -> Ptr PgfExn -> IO a)

instance Functor (Transaction k) where
  fmap f (Transaction g) = Transaction $ \c_db c_revision c_exn -> do
    res <- g c_db c_revision c_exn
    return (f res)

instance Applicative (Transaction k) where
  pure x = Transaction $ \c_db c_revision c_exn -> return x
  f <*> g = do
    f <- f
    g <- g
    return (f g)

instance Monad (Transaction k) where
  (Transaction f) >>= g = Transaction $ \c_db c_revision c_exn -> do
    res <- f c_db c_revision c_exn
    ex_type <- (#peek PgfExn, type) c_exn
    if (ex_type :: (#type PgfExnType)) == (#const PGF_EXN_NONE)
      then case g res of
             Transaction g -> g c_db c_revision c_exn
      else return undefined

{- | @modifyPGF gr t@ updates the grammar @gr@ by performing the
   transaction @t@. The changes are applied to the new grammar
   returned by the function, while any further operations with @gr@
   will still work with the old grammar. The newly created grammar
   also replaces the corresponding branch. In the example:
   
   > do gr <- readPGF "my_grammar.pgf"
   >    Just ty = readType "S"
   >    gr1 <- modifyPGF gr (createFunction "foo" ty)
   >    gr2 <- checkoutPGF gr "master"
   >    print (functionType gr2 "foo")

   both @gr1@ and @gr2@ will refer to the new grammar which contains
   the new function @foo@.
-}
modifyPGF :: PGF -> Transaction PGF a -> IO PGF
modifyPGF = branchPGF_ nullPtr

{- | @branchPGF gr branch_name t@ is similar to @modifyPGF gr t@,
   except that it stores the result as a branch with the given name.
-}
branchPGF :: PGF -> String -> Transaction PGF a -> IO PGF
branchPGF p name t =
  withText name $ \c_name ->
    branchPGF_ c_name p t

branchPGF_ :: Ptr PgfText -> PGF -> Transaction PGF a -> IO PGF
branchPGF_ c_name p (Transaction f) =
  withForeignPtr (a_revision p) $ \c_revision ->
  withPgfExn "branchPGF" $ \c_exn ->
  mask $ \restore -> do
    c_revision <- pgf_clone_revision (a_db p) c_revision c_name c_exn
    ex_type <- (#peek PgfExn, type) c_exn
    if (ex_type :: (#type PgfExnType)) == (#const PGF_EXN_NONE)
      then do ((restore (f (a_db p) c_revision c_exn))
               `catch`
               (\e -> do
                    pgf_free_revision_ (a_db p) c_revision
                    throwIO (e :: SomeException)))
              ex_type <- (#peek PgfExn, type) c_exn
              if (ex_type :: (#type PgfExnType)) == (#const PGF_EXN_NONE)
                then do pgf_commit_revision (a_db p) c_revision c_exn
                        ex_type <- (#peek PgfExn, type) c_exn
                        if (ex_type :: (#type PgfExnType)) == (#const PGF_EXN_NONE)
                          then do fptr <- newForeignPtrEnv pgf_free_revision (a_db p) c_revision
                                  langs <- getConcretes (a_db p) fptr
                                  return (PGF (a_db p) fptr langs)
                          else do pgf_free_revision_ (a_db p) c_revision
                                  return p
                else do pgf_free_revision_ (a_db p) c_revision
                        return p
      else return p

{- | Retrieves the branch with the given name -}
checkoutPGF :: PGF -> String -> IO (Maybe PGF)
checkoutPGF p name =
  withText name $ \c_name -> do
    c_revision <- withPgfExn "checkoutPGF" (pgf_checkout_revision (a_db p) c_name)
    if c_revision == nullPtr
      then return Nothing
      else do fptr <- newForeignPtrEnv pgf_free_revision (a_db p) c_revision
              langs <- getConcretes (a_db p) fptr
              return (Just (PGF (a_db p) fptr langs))

createFunction :: Fun -> Type -> Int -> Float -> Transaction PGF ()
createFunction name ty arity prob = Transaction $ \c_db c_revision c_exn ->
  withText name $ \c_name ->
  bracket (newStablePtr ty) freeStablePtr $ \c_ty ->
  withForeignPtr marshaller $ \m -> do
    pgf_create_function c_db c_revision c_name c_ty (fromIntegral arity) prob m c_exn

dropFunction :: Fun -> Transaction PGF ()
dropFunction name = Transaction $ \c_db c_revision c_exn ->
  withText name $ \c_name -> do
    pgf_drop_function c_db c_revision c_name c_exn

createCategory :: Fun -> [Hypo] -> Float -> Transaction PGF ()
createCategory name hypos prob = Transaction $ \c_db c_revision c_exn ->
  withText name $ \c_name ->
  withHypos hypos $ \n_hypos c_hypos ->
  withForeignPtr marshaller $ \m -> do
    pgf_create_category c_db c_revision c_name n_hypos c_hypos prob m c_exn

dropCategory :: Cat -> Transaction PGF ()
dropCategory name = Transaction $ \c_db c_revision c_exn ->
  withText name $ \c_name -> do
    pgf_drop_category c_db c_revision c_name c_exn

createConcrete :: ConcName -> Transaction Concr () -> Transaction PGF ()
createConcrete name (Transaction f) = Transaction $ \c_db c_revision c_exn ->
  withText name $ \c_name -> do
  bracket (pgf_create_concrete c_db c_revision c_name c_exn)
          (pgf_free_concr_revision_ c_db) $ \c_concr_revision ->
    f c_db c_concr_revision c_exn

alterConcrete :: ConcName -> Transaction Concr () -> Transaction PGF ()
alterConcrete name (Transaction f) = Transaction $ \c_db c_revision c_exn ->
  withText name $ \c_name -> do
    c_concr_revision <- pgf_clone_concrete c_db c_revision c_name c_exn
    f c_db c_concr_revision c_exn

dropConcrete :: ConcName -> Transaction PGF ()
dropConcrete name = Transaction $ \c_db c_revision c_exn ->
  withText name $ \c_name -> do
    pgf_drop_concrete c_db c_revision c_name c_exn

setGlobalFlag :: String -> Literal -> Transaction PGF ()
setGlobalFlag name value = Transaction $ \c_db c_revision c_exn ->
  withText name $ \c_name ->
  bracket (newStablePtr value) freeStablePtr $ \c_value ->
  withForeignPtr marshaller $ \m ->
    pgf_set_global_flag c_db c_revision c_name c_value m c_exn

setAbstractFlag :: String -> Literal -> Transaction PGF ()
setAbstractFlag name value = Transaction $ \c_db c_revision c_exn ->
  withText name $ \c_name ->
  bracket (newStablePtr value) freeStablePtr $ \c_value ->
  withForeignPtr marshaller $ \m ->
    pgf_set_abstract_flag c_db c_revision c_name c_value m c_exn

setConcreteFlag :: String -> Literal -> Transaction Concr ()
setConcreteFlag name value = Transaction $ \c_db c_revision c_exn ->
  withText name $ \c_name ->
  bracket (newStablePtr value) freeStablePtr $ \c_value ->
  withForeignPtr marshaller $ \m ->
    pgf_set_concrete_flag c_db c_revision c_name c_value m c_exn

type Token  = String

type LIndex = Int
type LVar   = Int
data LParam  = LParam {-# UNPACK #-} !LIndex [(LIndex,LVar)]
               deriving (Eq,Show)

data Symbol
  = SymCat {-# UNPACK #-} !Int {-# UNPACK #-} !LParam
  | SymLit {-# UNPACK #-} !Int {-# UNPACK #-} !LParam
  | SymVar {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | SymKS Token
  | SymKP [Symbol] [([Symbol],[String])]
  | SymBIND                         -- the special BIND token
  | SymNE                           -- non exist
  | SymSOFT_BIND                    -- the special SOFT_BIND token
  | SymSOFT_SPACE                   -- the special SOFT_SPACE token
  | SymCAPIT                        -- the special CAPIT token
  | SymALL_CAPIT                    -- the special ALL_CAPIT token
  deriving (Eq,Show)

data PArg = PArg [(LIndex,LIndex)] {-# UNPACK #-} !LParam
            deriving (Eq,Show)

data Production = Production [PArg] LParam [[Symbol]]
                 deriving (Eq,Show)

createLin :: Fun -> [Production] -> Transaction Concr ()
createLin name rules = Transaction $ \c_db c_revision c_exn ->
  withText name $ \c_name ->
    pgf_create_lin c_db c_revision c_name (fromIntegral (length rules)) c_exn
