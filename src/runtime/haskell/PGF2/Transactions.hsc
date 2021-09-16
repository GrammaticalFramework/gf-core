module PGF2.Transactions
          ( Transaction
          , modifyPGF
          , branchPGF
          , checkoutPGF
          , createFunction
          , dropFunction
          , createCategory
          , dropCategory
          , setGlobalFlag
          , setAbstractFlag
          ) where

import PGF2.FFI
import PGF2.Expr

import Foreign
import Foreign.C
import qualified Foreign.Concurrent as C
import Control.Exception

#include <pgf/pgf.h>

newtype Transaction a =
  Transaction (Ptr PgfDB -> Ptr PgfRevision -> Ptr PgfExn -> IO a)

instance Functor Transaction where
  fmap f (Transaction g) = Transaction $ \c_db c_revision c_exn -> do
    res <- g c_db c_revision c_exn
    return (f res)

instance Applicative Transaction where
  pure x = Transaction $ \c_db c_revision c_exn -> return x
  f <*> g = do
    f <- f
    g <- g
    return (f g)

instance Monad Transaction where
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
modifyPGF :: PGF -> Transaction a -> IO PGF
modifyPGF = branchPGF_ nullPtr

{- | @branchPGF gr branch_name t@ is similar to @modifyPGF gr t@,
   except that it stores the result as a branch with the given name.
-}
branchPGF :: PGF -> String -> Transaction a -> IO PGF
branchPGF p name t =
  withText name $ \c_name ->
    branchPGF_ c_name p t

branchPGF_ :: Ptr PgfText -> PGF -> Transaction a -> IO PGF
branchPGF_ c_name p (Transaction f) =
  withForeignPtr (a_db p) $ \c_db ->
  withForeignPtr (revision p) $ \c_revision ->
  withPgfExn $ \c_exn ->
  mask $ \restore -> do
    c_revision <- pgf_clone_revision c_db c_revision c_name c_exn
    ex_type <- (#peek PgfExn, type) c_exn
    if (ex_type :: (#type PgfExnType)) == (#const PGF_EXN_NONE)
      then do ((restore (f c_db c_revision c_exn))
               `catch`
               (\e -> do
                    pgf_free_revision c_db c_revision
                    throwIO (e :: SomeException)))
              ex_type <- (#peek PgfExn, type) c_exn
              if (ex_type :: (#type PgfExnType)) == (#const PGF_EXN_NONE)
                then do pgf_commit_revision c_db c_revision c_exn
                        ex_type <- (#peek PgfExn, type) c_exn
                        if (ex_type :: (#type PgfExnType)) == (#const PGF_EXN_NONE)
                          then do fptr2 <- C.newForeignPtr c_revision (withForeignPtr (a_db p) (\c_db -> pgf_free_revision c_db c_revision))
                                  return (PGF (a_db p) fptr2 (languages p))
                          else do pgf_free_revision c_db c_revision
                                  return p
                else do pgf_free_revision c_db c_revision
                        return p
      else return p

{- | Retrieves the branch with the given name -}
checkoutPGF :: PGF -> String -> IO (Maybe PGF)
checkoutPGF p name =
  withForeignPtr (a_db p) $ \c_db ->
  withText name $ \c_name -> do
    c_revision <- withPgfExn (pgf_checkout_revision c_db c_name)
    if c_revision == nullPtr
      then return Nothing
      else do fptr2 <- C.newForeignPtr c_revision (withForeignPtr (a_db p) (\c_db -> pgf_free_revision c_db c_revision))
              return (Just (PGF (a_db p) fptr2 (languages p)))

createFunction :: Fun -> Type -> Int -> Float -> Transaction ()
createFunction name ty arity prob = Transaction $ \c_db c_revision c_exn ->
  withText name $ \c_name ->
  bracket (newStablePtr ty) freeStablePtr $ \c_ty ->
  withForeignPtr marshaller $ \m -> do
    pgf_create_function c_db c_revision c_name c_ty (fromIntegral arity) prob m c_exn

dropFunction :: Fun -> Transaction ()
dropFunction name = Transaction $ \c_db c_revision c_exn ->
  withText name $ \c_name -> do
    pgf_drop_function c_db c_revision c_name c_exn

createCategory :: Fun -> [Hypo] -> Float -> Transaction ()
createCategory name hypos prob = Transaction $ \c_db c_revision c_exn ->
  withText name $ \c_name ->
  withHypos hypos $ \n_hypos c_hypos ->
  withForeignPtr marshaller $ \m -> do
    pgf_create_category c_db c_revision c_name n_hypos c_hypos prob m c_exn

dropCategory :: Cat -> Transaction ()
dropCategory name = Transaction $ \c_db c_revision c_exn ->
  withText name $ \c_name -> do
    pgf_drop_category c_db c_revision c_name c_exn

setGlobalFlag :: String -> Literal -> Transaction ()
setGlobalFlag name value = Transaction $ \c_db c_revision c_exn ->
  withText name $ \c_name ->
  bracket (newStablePtr value) freeStablePtr $ \c_value ->
  withForeignPtr marshaller $ \m ->
    pgf_set_global_flag c_db c_revision c_name c_value m c_exn

setAbstractFlag :: String -> Literal -> Transaction ()
setAbstractFlag name value = Transaction $ \c_db c_revision c_exn ->
  withText name $ \c_name ->
  bracket (newStablePtr value) freeStablePtr $ \c_value ->
  withForeignPtr marshaller $ \m ->
    pgf_set_abstract_flag c_db c_revision c_name c_value m c_exn
