module PGF2.Transactions
          ( Transaction
          , modifyPGF
          , createFunction
          ) where

import PGF2.FFI
import PGF2.Expr

import Foreign
import Foreign.C
import qualified Foreign.Concurrent as C
import Control.Exception(bracket)

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

modifyPGF :: PGF -> Transaction a -> IO PGF
modifyPGF p (Transaction f) =
  withForeignPtr (a_db p) $ \c_db ->
  withForeignPtr (revision p) $ \c_revision ->
  withPgfExn "" $ \c_exn -> do
    c_revision <- pgf_clone_revision c_db c_revision c_exn
    ex_type <- (#peek PgfExn, type) c_exn
    if (ex_type :: (#type PgfExnType)) == (#const PGF_EXN_NONE)
      then do f c_db c_revision c_exn
              fptr2 <- C.newForeignPtr c_revision (withForeignPtr (a_db p) (\c_db -> pgf_free_revision c_db c_revision))
              return (PGF (a_db p) fptr2 (langs p))
      else return p

createFunction :: Fun -> Type -> Float -> Transaction ()
createFunction name ty prob = Transaction $ \c_db c_revision c_exn ->
  withText name $ \c_name ->
  bracket (newStablePtr ty) freeStablePtr $ \c_ty ->
  withForeignPtr marshaller $ \m -> do
    pgf_create_function c_db c_revision c_name c_ty prob m c_exn
