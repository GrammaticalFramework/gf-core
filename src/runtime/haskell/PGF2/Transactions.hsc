module PGF2.Transactions
          ( -- transactions
            TxnID
          , Transaction
          , startTransaction
          , commitTransaction
          , rollbackTransaction
          , inTransaction

            -- abstract syntax
          , modifyPGF
          , checkoutPGF
          , createFunction
          , dropFunction
          , createCategory
          , dropCategory
          , setGlobalFlag
          , setAbstractFlag

            -- concrete syntax
          , Token, SeqId, LIndex, LVar, LParam(..)
          , PArg(..), Symbol(..), Production(..)

          , createConcrete
          , alterConcrete
          , dropConcrete
          , mergePGF
          , setConcreteFlag
          , SeqTable
          , createLincat
          , dropLincat
          , createLin, alterLin
          , dropLin
          , setPrintName
          , getFunctionType
          , getCategoryFields
          ) where

import PGF2.FFI
import PGF2.Expr
import PGF2.ByteCode

import Foreign
import Foreign.C
import Control.Exception
import qualified Control.Monad.Fail as Fail
import qualified Data.Sequence as Seq
import Data.IORef

#include <pgf/pgf.h>

newtype Transaction k a =
  Transaction (Ptr PgfDB -> Ptr PGF -> Ptr k -> Ptr PgfExn -> IO a)

instance Functor (Transaction k) where
  fmap f (Transaction g) = Transaction $ \c_db c_abstr c_revision c_exn -> do
    res <- g c_db c_abstr c_revision c_exn
    return (f res)

instance Applicative (Transaction k) where
  pure x = Transaction $ \c_db _ c_revision c_exn -> return x
  f <*> g = do
    f <- f
    g <- g
    return (f g)

instance Monad (Transaction k) where
  (Transaction f) >>= g = Transaction $ \c_db c_abstr c_revision c_exn -> do
    res <- f c_db c_abstr c_revision c_exn
    ex_type <- (#peek PgfExn, type) c_exn
    if (ex_type :: (#type PgfExnType)) == (#const PGF_EXN_NONE)
      then case g res of
             Transaction g -> g c_db c_abstr c_revision c_exn
      else return undefined

#if !(MIN_VERSION_base(4,13,0))
  -- Monad(fail) will be removed in GHC 8.8+
  fail = Fail.fail
#endif

instance Fail.MonadFail (Transaction k) where
  fail msg = Transaction $ \c_db c_abstr c_revision c_exn -> fail msg

data TxnID = TxnID (Ptr PgfDB) (ForeignPtr PGF)

startTransaction :: PGF -> IO TxnID
startTransaction p = do
  c_revision <- withPgfExn "startTransaction" (pgf_start_transaction (a_db p))
  fptr <- newForeignPtrEnv pgf_free_revision (a_db p) c_revision
  return (TxnID (a_db p) fptr)

commitTransaction :: TxnID -> IO PGF
commitTransaction (TxnID db fptr) = do
  withForeignPtr fptr $ \c_revision ->
    withPgfExn "commitTransaction" (pgf_commit_transaction db c_revision)
  return (PGF db fptr)

rollbackTransaction :: TxnID -> IO ()
rollbackTransaction (TxnID db fptr) =
  finalizeForeignPtr fptr

inTransaction :: TxnID -> Transaction PGF a -> IO a
inTransaction (TxnID db fptr) (Transaction f) =
  withForeignPtr fptr $ \c_revision -> do
  withPgfExn "inTransaction" $ \c_exn ->
    f db c_revision c_revision c_exn

{- | @modifyPGF gr t@ updates the grammar @gr@ by performing the
   transaction @t@. The changes are applied to the new grammar
   returned by the function, while any further operations with @gr@
   will still access the old grammar.
-}
modifyPGF :: PGF -> Transaction PGF a -> IO PGF
modifyPGF p (Transaction f) =
  withPgfExn "modifyPGF" $ \c_exn ->
  mask $ \restore -> do
    c_revision <- pgf_start_transaction (a_db p) c_exn
    ex_type <- (#peek PgfExn, type) c_exn
    if (ex_type :: (#type PgfExnType)) == (#const PGF_EXN_NONE)
      then do ((restore (f (a_db p) c_revision c_revision c_exn))
               `catch`
               (\e -> do
                    pgf_free_revision_ (a_db p) c_revision
                    throwIO (e :: SomeException)))
              ex_type <- (#peek PgfExn, type) c_exn
              if (ex_type :: (#type PgfExnType)) == (#const PGF_EXN_NONE)
                then do pgf_commit_transaction (a_db p) c_revision c_exn
                        ex_type <- (#peek PgfExn, type) c_exn
                        if (ex_type :: (#type PgfExnType)) == (#const PGF_EXN_NONE)
                          then do fptr <- newForeignPtrEnv pgf_free_revision (a_db p) c_revision
                                  return (PGF (a_db p) fptr)
                          else do pgf_free_revision_ (a_db p) c_revision
                                  return p
                else do pgf_free_revision_ (a_db p) c_revision
                        return p
      else return p

{- | Retrieves the branch with the given name -}
checkoutPGF :: PGF -> IO PGF
checkoutPGF p = do
  c_revision <- withPgfExn "checkoutPGF" (pgf_checkout_revision (a_db p))
  fptr <- newForeignPtrEnv pgf_free_revision (a_db p) c_revision
  return (PGF (a_db p) fptr)

{- | 'createFunction name ty arity bytecode prob' creates a new abstract
     syntax function with the given name, type, arity, etc. If the name
     contains %d, %x or %a then the pattern is replaced with a random
     number in base 10, 16, or 36, which guarantees that the name is
     unique. The returned name is the final name after the substitution.
     If there is no substitution pattern in the name, and there is
     already a function with the same name then an exception is thrown.
-}
createFunction :: Fun -> Type -> Int -> [[Instr]] -> Float -> Transaction PGF Fun
createFunction name ty arity bytecode prob = Transaction $ \c_db _ c_revision c_exn ->
  withText name $ \c_name ->
  bracket (newStablePtr ty) freeStablePtr $ \c_ty ->
  (if null bytecode then (\f -> f nullPtr) else (allocaBytes 0)) $ \c_bytecode -> do
    c_name <- pgf_create_function c_db c_revision c_name c_ty (fromIntegral arity) c_bytecode prob marshaller c_exn
    if c_name == nullPtr
      then return ""
      else do name  <- peekText c_name
              free c_name
              return name

dropFunction :: Fun -> Transaction PGF ()
dropFunction name = Transaction $ \c_db _ c_revision c_exn ->
  withText name $ \c_name -> do
    pgf_drop_function c_db c_revision c_name c_exn

createCategory :: Cat -> [Hypo] -> Float -> Transaction PGF ()
createCategory name hypos prob = Transaction $ \c_db _ c_revision c_exn ->
  withText name $ \c_name ->
  withHypos hypos $ \n_hypos c_hypos -> do
    pgf_create_category c_db c_revision c_name n_hypos c_hypos prob marshaller c_exn

dropCategory :: Cat -> Transaction PGF ()
dropCategory name = Transaction $ \c_db _ c_revision c_exn ->
  withText name $ \c_name -> do
    pgf_drop_category c_db c_revision c_name c_exn

createConcrete :: ConcName -> Transaction Concr () -> Transaction PGF ()
createConcrete name (Transaction f) = Transaction $ \c_db c_abstr c_revision c_exn ->
  withText name $ \c_name -> do
  bracketPtr (pgf_create_concrete c_db c_revision c_name c_exn)
             (pgf_free_concr_revision_ c_db) $ \c_concr_revision ->
    f c_db c_abstr c_concr_revision c_exn

alterConcrete :: ConcName -> Transaction Concr a -> Transaction PGF a
alterConcrete name (Transaction f) = Transaction $ \c_db c_abstr c_revision c_exn ->
  withText name $ \c_name -> do
  bracketPtr (pgf_clone_concrete c_db c_revision c_name c_exn)
             (pgf_free_concr_revision_ c_db) $ \c_concr_revision ->
    f c_db c_abstr c_concr_revision c_exn

bracketPtr before after thing =
  mask $ \restore -> do
    a <- before
    if a == nullPtr
      then return undefined
      else do r <- restore (thing a) `onException` after a
              _ <- after a
              return r

dropConcrete :: ConcName -> Transaction PGF ()
dropConcrete name = Transaction $ \c_db _ c_revision c_exn ->
  withText name $ \c_name -> do
    pgf_drop_concrete c_db c_revision c_name c_exn

mergePGF :: FilePath -> Transaction PGF ()
mergePGF fpath = Transaction $ \c_db _ c_revision c_exn ->
  withCString fpath $ \c_fpath ->
    pgf_merge_pgf c_db c_revision c_fpath c_exn

setGlobalFlag :: String -> Literal -> Transaction PGF ()
setGlobalFlag name value = Transaction $ \c_db _ c_revision c_exn ->
  withText name $ \c_name ->
  bracket (newStablePtr value) freeStablePtr $ \c_value ->
    pgf_set_global_flag c_db c_revision c_name c_value marshaller c_exn

setAbstractFlag :: String -> Literal -> Transaction PGF ()
setAbstractFlag name value = Transaction $ \c_db _ c_revision c_exn ->
  withText name $ \c_name ->
  bracket (newStablePtr value) freeStablePtr $ \c_value ->
    pgf_set_abstract_flag c_db c_revision c_name c_value marshaller c_exn

setConcreteFlag :: String -> Literal -> Transaction Concr ()
setConcreteFlag name value = Transaction $ \c_db _ c_revision c_exn ->
  withText name $ \c_name ->
  bracket (newStablePtr value) freeStablePtr $ \c_value ->
    pgf_set_concrete_flag c_db c_revision c_name c_value marshaller c_exn

type Token  = String

type SeqId  = Int
type LIndex = Int
type LVar   = Int
data LParam  = LParam {-# UNPACK #-} !LIndex [(LIndex,LVar)]
               deriving (Eq,Ord,Show)

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
  deriving (Eq,Ord,Show)

data PArg = PArg [(LIndex,LIndex)] {-# UNPACK #-} !LParam
            deriving (Eq,Show)

data Production = Production [(LVar,LIndex)] [PArg] LParam [SeqId]
                 deriving (Eq,Show)

type SeqTable = Seq.Seq (Either [Symbol] SeqId)

createLincat :: Cat -> [String] -> [Production] -> [Production] -> SeqTable -> Transaction Concr SeqTable
createLincat name fields lindefs linrefs seqtbl = Transaction $ \c_db c_abstr c_revision c_exn ->
  let n_fields = length fields
  in withText name $ \c_name ->
     allocaBytes (n_fields*(#size PgfText*)) $ \c_fields ->
     withTexts c_fields 0 fields $
     withBuildLinIface (lindefs++linrefs) seqtbl $ \c_build ->
       pgf_create_lincat c_db c_abstr c_revision c_name
                         (fromIntegral n_fields) c_fields
                         (fromIntegral (length lindefs)) (fromIntegral (length linrefs))
                         c_build c_exn
  where
    withTexts p i []     f = f
    withTexts p i (s:ss) f =
      withText s $ \c_s -> do
        pokeElemOff p i c_s
        withTexts p (i+1) ss f

dropLincat :: Cat -> Transaction Concr ()
dropLincat name = Transaction $ \c_db  c_abstr c_revision c_exn ->
  withText name $ \c_name ->
    pgf_drop_lincat c_db  c_abstr c_revision c_name c_exn

createLin :: Fun -> [Production] -> SeqTable -> Transaction Concr SeqTable
createLin name prods seqtbl = Transaction $ \c_db c_abstr c_revision c_exn ->
  withText name $ \c_name ->
  withBuildLinIface prods seqtbl $ \c_build ->
    pgf_create_lin c_db c_abstr c_revision c_name (fromIntegral (length prods)) c_build c_exn

alterLin :: Fun -> [Production] -> SeqTable -> Transaction Concr SeqTable
alterLin name prods seqtbl = Transaction $ \c_db c_abstr c_revision c_exn ->
  withText name $ \c_name ->
  withBuildLinIface prods seqtbl $ \c_build ->
    pgf_alter_lin c_db c_abstr c_revision c_name (fromIntegral (length prods)) c_build c_exn

withBuildLinIface prods seqtbl f = do
  ref <- newIORef seqtbl
  (allocaBytes (#size PgfBuildLinIface) $ \c_build ->
   allocaBytes (#size PgfBuildLinIfaceVtbl) $ \vtbl ->
   bracket (wrapLinBuild (build ref)) freeHaskellFunPtr $ \c_callback -> do
     (#poke PgfBuildLinIface, vtbl) c_build vtbl
     (#poke PgfBuildLinIfaceVtbl, build) vtbl c_callback
     f c_build)
  readIORef ref
  where
    forM_ []     c_exn f = return ()
    forM_ (x:xs) c_exn f = do
      ex_type <- (#peek PgfExn, type) c_exn
      if (ex_type :: (#type PgfExnType)) == (#const PGF_EXN_NONE)
        then f x >> forM_ xs c_exn f
        else return ()

    build ref _ c_builder c_exn = do
      vtbl <- (#peek PgfLinBuilderIface, vtbl) c_builder
      forM_ prods c_exn $ \(Production vars args res seqids) -> do
        fun <- (#peek PgfLinBuilderIfaceVtbl, start_production) vtbl
        callLinBuilder0 fun c_builder c_exn
        fun <- (#peek PgfLinBuilderIfaceVtbl, add_argument) vtbl
        forM_ args c_exn $ \(PArg hypos param) ->
          callLParam (callLinBuilder3 fun c_builder (fromIntegral (length hypos))) param c_exn
        fun <- (#peek PgfLinBuilderIfaceVtbl, set_result) vtbl  
        callLParam (callLinBuilder3 fun c_builder (fromIntegral (length vars))) res c_exn
        fun <- (#peek PgfLinBuilderIfaceVtbl, add_variable) vtbl
        forM_ vars c_exn $ \(v,r) ->
          callLinBuilder2 fun c_builder (fromIntegral v) (fromIntegral r) c_exn
        fun <- (#peek PgfLinBuilderIfaceVtbl, add_sequence_id) vtbl
        seqtbl <- readIORef ref
        forM_ seqids c_exn $ \seqid ->
          case Seq.index seqtbl seqid of
            Left syms   -> do fun    <- (#peek PgfLinBuilderIfaceVtbl, start_sequence) vtbl
                              callLinBuilder1 fun c_builder (fromIntegral (length syms)) c_exn
                              forM_ syms c_exn (addSymbol c_builder vtbl c_exn)
                              fun <- (#peek PgfLinBuilderIfaceVtbl, end_sequence) vtbl
                              seqid' <- callLinBuilder7 fun c_builder c_exn
                              writeIORef ref $! Seq.update seqid (Right (fromIntegral seqid')) seqtbl
            Right seqid -> do callLinBuilder1 fun c_builder (fromIntegral seqid) c_exn
        fun <- (#peek PgfLinBuilderIfaceVtbl, end_production) vtbl
        callLinBuilder0 fun c_builder c_exn

    addSymbol c_builder vtbl c_exn (SymCat d r) = do
      fun <- (#peek PgfLinBuilderIfaceVtbl, add_symcat) vtbl
      callLParam (callLinBuilder4 fun c_builder (fromIntegral d)) r c_exn
    addSymbol c_builder vtbl c_exn (SymLit d r) = do
      fun <- (#peek PgfLinBuilderIfaceVtbl, add_symlit) vtbl
      callLParam (callLinBuilder4 fun c_builder (fromIntegral d)) r c_exn
    addSymbol c_builder vtbl c_exn (SymVar d r) = do
      fun <- (#peek PgfLinBuilderIfaceVtbl, add_symvar) vtbl
      callLinBuilder2 fun c_builder (fromIntegral d) (fromIntegral r) c_exn
    addSymbol c_builder vtbl c_exn (SymKS tok) = do
      fun <- (#peek PgfLinBuilderIfaceVtbl, add_symks) vtbl
      withText tok $ \c_tok ->
        callLinBuilder5 fun c_builder c_tok c_exn
    addSymbol c_builder vtbl c_exn (SymKP def alts) = do
      fun <- (#peek PgfLinBuilderIfaceVtbl, start_symkp) vtbl
      callLinBuilder2 fun c_builder (fromIntegral (length def)) (fromIntegral (length alts)) c_exn
      forM_ def c_exn (addSymbol c_builder vtbl c_exn)
      forM_ alts c_exn $ \(form,ps) -> do
        let n_ps = length ps
        (allocaBytes (n_ps*(#size PgfText*)) $ \c_ps ->
         withTexts c_ps 0 ps $ do
           fun <- (#peek PgfLinBuilderIfaceVtbl, start_symkp_alt) vtbl
           callLinBuilder6 fun c_builder (fromIntegral (length form)) (fromIntegral n_ps) c_ps c_exn)
        forM_ form c_exn (addSymbol c_builder vtbl c_exn)
        fun <- (#peek PgfLinBuilderIfaceVtbl, end_symkp_alt) vtbl
        callLinBuilder0 fun c_builder c_exn
      fun <- (#peek PgfLinBuilderIfaceVtbl, end_symkp) vtbl
      callLinBuilder0 fun c_builder c_exn
      where
        withTexts p i []     f = f
        withTexts p i (s:ss) f =
          withText s $ \c_s -> do
            pokeElemOff p i c_s
            withTexts p (i+1) ss f

    addSymbol c_builder vtbl c_exn SymBIND = do
      fun <- (#peek PgfLinBuilderIfaceVtbl, add_symbind) vtbl
      callLinBuilder0 fun c_builder c_exn
    addSymbol c_builder vtbl c_exn SymSOFT_BIND = do
      fun <- (#peek PgfLinBuilderIfaceVtbl, add_symsoftbind) vtbl
      callLinBuilder0 fun c_builder c_exn
    addSymbol c_builder vtbl c_exn SymNE = do
      fun <- (#peek PgfLinBuilderIfaceVtbl, add_symne) vtbl
      callLinBuilder0 fun c_builder c_exn
    addSymbol c_builder vtbl c_exn SymSOFT_SPACE = do
      fun <- (#peek PgfLinBuilderIfaceVtbl, add_symsoftspace) vtbl
      callLinBuilder0 fun c_builder c_exn
    addSymbol c_builder vtbl c_exn SymCAPIT = do
      fun <- (#peek PgfLinBuilderIfaceVtbl, add_symcapit) vtbl
      callLinBuilder0 fun c_builder c_exn
    addSymbol c_builder vtbl c_exn SymALL_CAPIT = do
      fun <- (#peek PgfLinBuilderIfaceVtbl, add_symallcapit) vtbl
      callLinBuilder0 fun c_builder c_exn

    callLParam f (LParam i0 terms) c_exn =
      allocaBytes (n_terms*2*(#size size_t)) $ \c_terms -> do
        pokeTerms c_terms terms
        f (fromIntegral i0) (fromIntegral n_terms) c_terms c_exn
      where
        n_terms = length terms

        pokeTerms c_terms []                  = return ()
        pokeTerms c_terms ((factor,var):terms) = do
          pokeElemOff c_terms 0 (fromIntegral factor)
          pokeElemOff c_terms 1 (fromIntegral var)
          pokeTerms (c_terms `plusPtr` (2*(#size size_t))) terms

dropLin :: Fun -> Transaction Concr ()
dropLin name = Transaction $ \c_db c_abstr c_revision c_exn ->
  withText name $ \c_name ->
    pgf_drop_lin c_db c_abstr c_revision c_name c_exn

setPrintName :: Fun -> String -> Transaction Concr ()
setPrintName fun name = Transaction $ \c_db _ c_revision c_exn ->
  withText fun $ \c_fun ->
  withText name $ \c_name -> do
    pgf_set_printname c_db c_revision c_fun c_name c_exn

-- | A monadic version of 'functionType' which returns the type of
-- the function in the current transaction.
getFunctionType :: Fun -> Transaction PGF (Maybe Type)
getFunctionType fun = Transaction $ \c_db c_revision _ c_exn -> do
  c_typ <- withText fun $ \c_fun ->
             pgf_function_type c_db c_revision c_fun unmarshaller c_exn
  ex_type <- (#peek PgfExn, type) c_exn
  if (ex_type :: (#type PgfExnType)) == (#const PGF_EXN_NONE)
    then if c_typ == castPtrToStablePtr nullPtr
           then return Nothing
           else do typ <- deRefStablePtr c_typ
                   freeStablePtr c_typ
                   return (Just typ)
    else return undefined

-- | A monadic version of 'categoryFields' which returns the fields of
-- a category from grammar in the current transaction.
getCategoryFields :: Cat -> Transaction Concr (Maybe [String])
getCategoryFields cat = Transaction $ \c_db _ c_revision c_exn ->
  withText cat $ \c_cat ->
  alloca $ \p_n_fields -> do
    c_fields <- pgf_category_fields c_db c_revision c_cat p_n_fields c_exn
    ex_type <- (#peek PgfExn, type) c_exn
    if (ex_type :: (#type PgfExnType)) == (#const PGF_EXN_NONE)
      then if c_fields == nullPtr
             then return Nothing
             else do n_fields <- peek p_n_fields
                     fs <- peekFields n_fields c_fields
                     free c_fields
                     return (Just fs)
      else return undefined
  where
    peekFields n_fields c_fields
      | n_fields == 0 = return []
      | otherwise     = do c_text <- peek c_fields
                           f  <- peekText c_text
                           free c_text
                           fs <- peekFields (n_fields-1) (c_fields `plusPtr` (#size PgfText*))
                           return (f:fs)
