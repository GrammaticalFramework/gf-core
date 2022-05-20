{-# LANGUAGE ForeignFunctionInterface, MagicHash, BangPatterns #-}

module PGF2.FFI where

import GHC.Exts
import GHC.Prim
import GHC.Integer.Logarithms
import Data.Word
import Data.IORef
import Data.Typeable
import Foreign
import Foreign.C
import Foreign.Ptr
import qualified Data.Map as Map
import Control.Exception(Exception,bracket,mask_,throwIO)
import System.IO.Unsafe(unsafePerformIO)

import PGF2.Expr

#include <pgf/pgf.h>

type AbsName  = String -- ^ Name of abstract syntax
type ConcName = String -- ^ Name of concrete syntax

-- | An abstract data type representing multilingual grammar
-- in Portable Grammar Format.
data PGF = PGF { a_db       :: Ptr PgfDB
               , a_revision :: ForeignPtr PGF
               , languages  :: Map.Map ConcName Concr
               }
data Concr = Concr {c_db :: Ptr PgfDB, c_revision :: ForeignPtr Concr}

------------------------------------------------------------------
-- libpgf API

data PgfExn
data PgfText
data PgfItor
data PgfDB
data PgfPrintContext
data PgfTypeHypo
data PgfMarshaller
data PgfUnmarshaller
data PgfBuildLinIface
data PgfLinBuilderIface
data PgfLinearizationOutputIface
data PgfGraphvizOptions
data PgfSequenceItor
data PgfMorphoCallback
data PgfPhrasetableIds

type Wrapper a = a -> IO (FunPtr a)
type Dynamic a = FunPtr a -> a

foreign import ccall unsafe "pgf_utf8_decode"
  pgf_utf8_decode :: Ptr CString -> IO Word32

foreign import ccall unsafe "pgf_utf8_encode"
  pgf_utf8_encode :: Word32 -> Ptr CString -> IO ()

foreign import ccall "pgf_read_pgf"
  pgf_read_pgf :: CString -> Ptr (Ptr PGF) -> Ptr PgfExn -> IO (Ptr PgfDB)

foreign import ccall "pgf_boot_ngf"
  pgf_boot_ngf :: CString -> CString -> Ptr (Ptr PGF) -> Ptr PgfExn -> IO (Ptr PgfDB)

foreign import ccall "pgf_read_ngf"
  pgf_read_ngf :: CString -> Ptr (Ptr PGF) -> Ptr PgfExn -> IO (Ptr PgfDB)

foreign import ccall pgf_new_ngf :: Ptr PgfText -> CString -> Ptr (Ptr PGF) -> Ptr PgfExn -> IO (Ptr PgfDB)

foreign import ccall pgf_merge_pgf :: Ptr PgfDB -> Ptr PGF -> CString -> Ptr PgfExn -> IO ()

foreign import ccall pgf_write_pgf :: CString -> Ptr PgfDB -> Ptr PGF -> Ptr PgfExn -> IO ()

foreign import ccall "&pgf_free_revision" pgf_free_revision :: FinalizerEnvPtr PgfDB PGF

foreign import ccall "pgf_free_concr_revision" pgf_free_concr_revision_ :: Ptr PgfDB -> Ptr Concr -> IO ()

foreign import ccall "&pgf_free_concr_revision" pgf_free_concr_revision :: FinalizerEnvPtr PgfDB Concr

foreign import ccall "pgf_abstract_name"
  pgf_abstract_name :: Ptr PgfDB -> Ptr PGF -> Ptr PgfExn -> IO (Ptr PgfText)

foreign import ccall "pgf_print_expr"
  pgf_print_expr :: StablePtr Expr -> Ptr PgfPrintContext -> CInt -> Ptr PgfMarshaller -> IO (Ptr PgfText)

foreign import ccall pgf_print_ident :: Ptr PgfText -> IO (Ptr PgfText)

foreign import ccall "pgf_read_expr"
  pgf_read_expr :: Ptr PgfText -> Ptr PgfUnmarshaller -> IO (StablePtr Expr)

foreign import ccall pgf_read_expr_ex :: Ptr PgfText -> Ptr CString -> Ptr PgfUnmarshaller -> IO (StablePtr Expr)

foreign import ccall "pgf_print_type"
  pgf_print_type :: StablePtr Type -> Ptr PgfPrintContext -> CInt -> Ptr PgfMarshaller -> IO (Ptr PgfText)

foreign import ccall pgf_print_context :: CSize -> Ptr PgfTypeHypo -> Ptr PgfPrintContext -> CInt -> Ptr PgfMarshaller -> IO (Ptr PgfText)

foreign import ccall pgf_read_type :: Ptr PgfText -> Ptr PgfUnmarshaller -> IO (StablePtr Type)

foreign import ccall pgf_read_context :: Ptr PgfText -> Ptr PgfUnmarshaller -> Ptr CSize -> IO (Ptr PgfTypeHypo)

foreign import ccall pgf_print_start_cat_internal :: Ptr PgfDB -> Ptr PGF -> Ptr PgfExn -> IO (Ptr PgfText)

foreign import ccall pgf_print_category_internal :: Ptr () -> IO (Ptr PgfText)

foreign import ccall pgf_print_function_internal :: Ptr () -> IO (Ptr PgfText)

foreign import ccall pgf_iter_lincats :: Ptr PgfDB -> Ptr Concr -> Ptr PgfItor -> Ptr PgfExn -> IO ()

foreign import ccall pgf_iter_lins :: Ptr PgfDB -> Ptr Concr -> Ptr PgfItor -> Ptr PgfExn -> IO ()

type SequenceItorCallback = Ptr PgfSequenceItor -> CSize -> Ptr () -> Ptr PgfExn -> IO CInt

foreign import ccall "wrapper" wrapSequenceItorCallback :: Wrapper SequenceItorCallback

type MorphoCallback = Ptr PgfMorphoCallback -> Ptr PgfText -> Ptr PgfText -> (#type prob_t) -> Ptr PgfExn -> IO ()

foreign import ccall "wrapper" wrapMorphoCallback :: Wrapper MorphoCallback

foreign import ccall pgf_iter_sequences :: Ptr PgfDB -> Ptr Concr -> Ptr PgfSequenceItor -> Ptr PgfMorphoCallback -> Ptr PgfExn -> IO (Ptr PgfPhrasetableIds)

foreign import ccall pgf_get_lincat_counts_internal :: Ptr () -> Ptr CSize -> IO ()

foreign import ccall pgf_get_lincat_field_internal :: Ptr () -> CSize -> IO (Ptr PgfText)

foreign import ccall pgf_print_lindef_internal :: Ptr PgfPhrasetableIds -> Ptr () -> CSize -> IO (Ptr PgfText)

foreign import ccall pgf_print_linref_internal :: Ptr PgfPhrasetableIds -> Ptr () -> CSize -> IO (Ptr PgfText)

foreign import ccall pgf_get_lin_get_prod_count :: Ptr () -> IO CSize

foreign import ccall pgf_print_lin_internal :: Ptr PgfPhrasetableIds -> Ptr () -> CSize -> IO (Ptr PgfText)

foreign import ccall pgf_print_sequence_internal :: CSize -> Ptr () -> IO (Ptr PgfText)

foreign import ccall pgf_sequence_get_text_internal :: Ptr () -> IO (Ptr PgfText)

foreign import ccall pgf_release_phrasetable_ids :: Ptr PgfPhrasetableIds -> IO ()

type ItorCallback = Ptr PgfItor -> Ptr PgfText -> Ptr () -> Ptr PgfExn -> IO ()

foreign import ccall "wrapper" wrapItorCallback :: Wrapper ItorCallback

foreign import ccall pgf_iter_categories :: Ptr PgfDB -> Ptr PGF -> Ptr PgfItor -> Ptr PgfExn -> IO ()

foreign import ccall pgf_iter_concretes :: Ptr PgfDB -> Ptr PGF -> Ptr PgfItor -> Ptr PgfExn -> IO ()

foreign import ccall pgf_start_cat :: Ptr PgfDB -> Ptr PGF -> Ptr PgfUnmarshaller -> Ptr PgfExn -> IO (StablePtr Type)

foreign import ccall pgf_category_context :: Ptr PgfDB -> Ptr PGF -> Ptr PgfText -> Ptr CSize -> Ptr PgfUnmarshaller -> Ptr PgfExn -> IO (Ptr PgfTypeHypo)

foreign import ccall pgf_category_prob :: Ptr PgfDB -> Ptr PGF -> Ptr PgfText -> Ptr PgfExn -> IO (#type prob_t)

foreign import ccall pgf_iter_functions :: Ptr PgfDB -> Ptr PGF -> Ptr PgfItor -> Ptr PgfExn -> IO ()

foreign import ccall pgf_iter_functions_by_cat :: Ptr PgfDB -> Ptr PGF -> Ptr PgfText -> Ptr PgfItor -> Ptr PgfExn -> IO ()

foreign import ccall pgf_function_type :: Ptr PgfDB -> Ptr PGF -> Ptr PgfText -> Ptr PgfUnmarshaller -> Ptr PgfExn -> IO (StablePtr Type)

foreign import ccall pgf_function_is_constructor :: Ptr PgfDB -> Ptr PGF -> Ptr PgfText -> Ptr PgfExn -> IO (#type int)

foreign import ccall pgf_function_prob :: Ptr PgfDB -> Ptr PGF -> Ptr PgfText -> Ptr PgfExn -> IO (#type prob_t)

foreign import ccall pgf_concrete_name :: Ptr PgfDB -> Ptr Concr -> Ptr PgfExn -> IO (Ptr PgfText)

foreign import ccall pgf_concrete_language_code :: Ptr PgfDB -> Ptr Concr -> Ptr PgfExn -> IO (Ptr PgfText)

foreign import ccall pgf_expr_prob :: Ptr PgfDB -> Ptr PGF -> StablePtr Expr -> Ptr PgfMarshaller -> Ptr PgfExn -> IO (#type prob_t)

foreign import ccall pgf_check_expr :: Ptr PgfDB -> Ptr PGF -> Ptr (StablePtr Expr) -> StablePtr Type -> Ptr PgfMarshaller -> Ptr PgfUnmarshaller -> Ptr PgfExn -> IO ()

foreign import ccall pgf_infer_expr :: Ptr PgfDB -> Ptr PGF -> Ptr (StablePtr Expr) -> Ptr PgfMarshaller -> Ptr PgfUnmarshaller -> Ptr PgfExn -> IO (StablePtr Type)

foreign import ccall pgf_check_type :: Ptr PgfDB -> Ptr PGF -> Ptr (StablePtr Type) -> Ptr PgfMarshaller -> Ptr PgfUnmarshaller -> Ptr PgfExn -> IO ()

foreign import ccall pgf_start_transaction :: Ptr PgfDB -> Ptr PGF -> Ptr PgfExn -> IO (Ptr PGF)

foreign import ccall pgf_commit_transaction :: Ptr PgfDB -> Ptr PGF -> Ptr PgfExn -> IO ()

foreign import ccall pgf_rollback_transaction :: Ptr PgfDB -> Ptr PGF -> IO ()

foreign import ccall pgf_checkout_revision :: Ptr PgfDB -> Ptr PgfExn -> IO (Ptr PGF)

foreign import ccall pgf_create_function :: Ptr PgfDB -> Ptr PGF -> Ptr PgfText -> StablePtr Type -> CSize -> Ptr CChar -> (#type prob_t) -> Ptr PgfMarshaller -> Ptr PgfExn -> IO ()

foreign import ccall pgf_drop_function :: Ptr PgfDB -> Ptr PGF -> Ptr PgfText -> Ptr PgfExn -> IO ()

foreign import ccall pgf_create_category :: Ptr PgfDB -> Ptr PGF -> Ptr PgfText -> CSize -> Ptr PgfTypeHypo -> (#type prob_t) -> Ptr PgfMarshaller -> Ptr PgfExn -> IO ()

foreign import ccall pgf_drop_category :: Ptr PgfDB -> Ptr PGF -> Ptr PgfText -> Ptr PgfExn -> IO ()

foreign import ccall pgf_create_concrete :: Ptr PgfDB -> Ptr PGF -> Ptr PgfText -> Ptr PgfExn -> IO (Ptr Concr)

foreign import ccall pgf_clone_concrete :: Ptr PgfDB -> Ptr PGF -> Ptr PgfText -> Ptr PgfExn -> IO (Ptr Concr)

foreign import ccall pgf_drop_concrete :: Ptr PgfDB -> Ptr PGF -> Ptr PgfText -> Ptr PgfExn -> IO ()

foreign import ccall "wrapper" wrapLinBuild :: Wrapper (Ptr PgfBuildLinIface -> Ptr PgfLinBuilderIface -> Ptr PgfExn -> IO ())

foreign import ccall "dynamic" callLinBuilder0 :: Dynamic (Ptr PgfLinBuilderIface -> Ptr PgfExn -> IO ())

foreign import ccall "dynamic" callLinBuilder1 :: Dynamic (Ptr PgfLinBuilderIface -> CSize -> Ptr PgfExn -> IO ())

foreign import ccall "dynamic" callLinBuilder2 :: Dynamic (Ptr PgfLinBuilderIface -> CSize -> CSize -> Ptr PgfExn -> IO ())

foreign import ccall "dynamic" callLinBuilder3 :: Dynamic (Ptr PgfLinBuilderIface -> CSize -> CSize -> CSize -> Ptr CSize -> Ptr PgfExn -> IO ())

foreign import ccall "dynamic" callLinBuilder4 :: Dynamic (Ptr PgfLinBuilderIface -> CSize -> CSize -> CSize -> Ptr CSize -> Ptr PgfExn -> IO ())

foreign import ccall "dynamic" callLinBuilder5 :: Dynamic (Ptr PgfLinBuilderIface -> Ptr PgfText -> Ptr PgfExn -> IO ())

foreign import ccall "dynamic" callLinBuilder6 :: Dynamic (Ptr PgfLinBuilderIface -> CSize -> CSize -> Ptr (Ptr PgfText) -> Ptr PgfExn -> IO ())

foreign import ccall "dynamic" callLinBuilder7 :: Dynamic (Ptr PgfLinBuilderIface -> Ptr PgfExn -> IO CSize)

foreign import ccall pgf_create_lincat :: Ptr PgfDB -> Ptr PGF -> Ptr Concr -> Ptr PgfText -> CSize -> Ptr (Ptr PgfText) -> CSize -> CSize -> Ptr PgfBuildLinIface -> Ptr PgfExn -> IO ()

foreign import ccall pgf_drop_lincat :: Ptr PgfDB -> Ptr Concr -> Ptr PgfText -> Ptr PgfExn -> IO ()

foreign import ccall pgf_create_lin :: Ptr PgfDB -> Ptr PGF -> Ptr Concr -> Ptr PgfText -> CSize -> Ptr PgfBuildLinIface -> Ptr PgfExn -> IO ()

foreign import ccall pgf_drop_lin :: Ptr PgfDB -> Ptr Concr -> Ptr PgfText -> Ptr PgfExn -> IO ()

foreign import ccall pgf_has_linearization :: Ptr PgfDB -> Ptr Concr -> Ptr PgfText -> Ptr PgfExn -> IO CInt

foreign import ccall pgf_category_fields :: Ptr PgfDB -> Ptr Concr -> Ptr PgfText -> Ptr CSize -> Ptr PgfExn -> IO (Ptr (Ptr PgfText))

foreign import ccall pgf_linearize :: Ptr PgfDB -> Ptr Concr -> StablePtr Expr -> Ptr PgfPrintContext -> Ptr PgfMarshaller -> Ptr PgfExn -> IO (Ptr PgfText)

foreign import ccall pgf_linearize_all :: Ptr PgfDB -> Ptr Concr -> StablePtr Expr -> Ptr PgfPrintContext -> Ptr PgfMarshaller -> Ptr CSize -> Ptr PgfExn -> IO (Ptr (Ptr PgfText))

foreign import ccall pgf_tabular_linearize :: Ptr PgfDB -> Ptr Concr -> StablePtr Expr -> Ptr PgfPrintContext -> Ptr PgfMarshaller -> Ptr PgfExn -> IO (Ptr (Ptr PgfText))

foreign import ccall pgf_tabular_linearize_all :: Ptr PgfDB -> Ptr Concr -> StablePtr Expr -> Ptr PgfPrintContext -> Ptr PgfMarshaller -> Ptr PgfExn -> IO (Ptr (Ptr PgfText))

foreign import ccall pgf_bracketed_linearize :: Ptr PgfDB -> Ptr Concr -> StablePtr Expr -> Ptr PgfPrintContext -> Ptr PgfMarshaller -> Ptr PgfLinearizationOutputIface -> Ptr PgfExn -> IO ()

foreign import ccall pgf_bracketed_linearize_all :: Ptr PgfDB -> Ptr Concr -> StablePtr Expr -> Ptr PgfPrintContext -> Ptr PgfMarshaller -> Ptr PgfLinearizationOutputIface -> Ptr PgfExn -> IO ()

foreign import ccall "wrapper" wrapSymbol0 :: Wrapper (Ptr PgfLinearizationOutputIface -> IO ())

foreign import ccall "wrapper" wrapSymbol1 :: Wrapper (Ptr PgfLinearizationOutputIface -> Ptr PgfText -> IO ())

foreign import ccall "wrapper" wrapSymbol2 :: Wrapper (Ptr PgfLinearizationOutputIface -> Ptr PgfText -> CInt -> Ptr PgfText -> Ptr PgfText -> IO ())

foreign import ccall "wrapper" wrapSymbol3 :: Wrapper (Ptr PgfLinearizationOutputIface -> CInt -> IO ())

foreign import ccall pgf_get_printname :: Ptr PgfDB -> Ptr Concr -> Ptr PgfText -> Ptr PgfExn -> IO (Ptr PgfText)

foreign import ccall pgf_set_printname :: Ptr PgfDB -> Ptr Concr -> Ptr PgfText -> Ptr PgfText -> Ptr PgfExn -> IO ()

foreign import ccall pgf_get_global_flag :: Ptr PgfDB -> Ptr PGF -> Ptr PgfText -> Ptr PgfUnmarshaller -> Ptr PgfExn -> IO (StablePtr Literal)

foreign import ccall pgf_set_global_flag :: Ptr PgfDB -> Ptr PGF -> Ptr PgfText -> StablePtr Literal -> Ptr PgfMarshaller -> Ptr PgfExn -> IO ()

foreign import ccall pgf_get_abstract_flag :: Ptr PgfDB -> Ptr PGF -> Ptr PgfText -> Ptr PgfUnmarshaller -> Ptr PgfExn -> IO (StablePtr Literal)

foreign import ccall pgf_set_abstract_flag :: Ptr PgfDB -> Ptr PGF -> Ptr PgfText -> StablePtr Literal -> Ptr PgfMarshaller -> Ptr PgfExn -> IO ()

foreign import ccall pgf_get_concrete_flag :: Ptr PgfDB -> Ptr Concr -> Ptr PgfText -> Ptr PgfUnmarshaller -> Ptr PgfExn -> IO (StablePtr Literal)

foreign import ccall pgf_set_concrete_flag :: Ptr PgfDB -> Ptr Concr -> Ptr PgfText -> StablePtr Literal -> Ptr PgfMarshaller -> Ptr PgfExn -> IO ()

foreign import ccall pgf_graphviz_abstract_tree :: Ptr PgfDB -> Ptr PGF -> StablePtr Expr -> Ptr PgfMarshaller -> Ptr PgfGraphvizOptions -> Ptr PgfExn -> IO (Ptr PgfText)

foreign import ccall pgf_graphviz_parse_tree :: Ptr PgfDB -> Ptr Concr -> StablePtr Expr -> Ptr PgfPrintContext -> Ptr PgfMarshaller -> Ptr PgfGraphvizOptions -> Ptr PgfExn -> IO (Ptr PgfText)


-----------------------------------------------------------------------
-- Texts

peekText :: Ptr PgfText -> IO String
peekText ptr = do
  size <- ((#peek PgfText, size) ptr) :: IO CSize
  let c_text = castPtr ptr `plusPtr` (#offset PgfText, text)
  peekUtf8CString c_text (c_text `plusPtr` fromIntegral size)

newTextEx :: Int -> String -> IO (Ptr a)
newTextEx offs s = do
  ptr <- mallocBytes (offs + (#size PgfText) + size + 1)
  let ptext = ptr `plusPtr` offs
  (#poke PgfText, size) ptext (fromIntegral size :: CSize)
  pokeUtf8CString s (ptext `plusPtr` (#const offsetof(PgfText, text)))
  return ptr
  where
    size = utf8Length s

newText :: String -> IO (Ptr PgfText)
newText s = do
  ptr <- mallocBytes ((#size PgfText) + size + 1)
  (#poke PgfText, size) ptr (fromIntegral size :: CSize)
  pokeUtf8CString s (ptr `plusPtr` (#const offsetof(PgfText, text)))
  return ptr
  where
    size = utf8Length s

withText :: String -> (Ptr PgfText -> IO a) -> IO a
withText s fn =
  allocaBytes ((#size PgfText) + size + 1) $ \ptr -> do
    (#poke PgfText, size) ptr (fromIntegral size :: CSize)
    pokeUtf8CString s (ptr `plusPtr` (#const offsetof(PgfText, text)))
    fn ptr
  where
    size = utf8Length s

peekUtf8CString c_start c_end =
  alloca $ \pptr -> do
    poke pptr c_start
    decode pptr c_end
  where
    decode pptr end = do
      ptr <- peek pptr
      if ptr >= end
        then return []
        else do x <- pgf_utf8_decode pptr
                cs <- decode pptr end
                return (((toEnum . fromEnum) x) : cs)

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

-----------------------------------------------------------------------
-- Exceptions

data PGFError = PGFError String String
     deriving Typeable

instance Show PGFError where
  show (PGFError loc msg) = loc++": "++msg

instance Exception PGFError

withPgfExn loc f =
  allocaBytes (#size PgfExn) $ \c_exn -> do
    res <- f c_exn
    ex_type <- (#peek PgfExn, type) c_exn :: IO (#type PgfExnType)
    case ex_type of
      (#const PGF_EXN_NONE) -> return res
      (#const PGF_EXN_SYSTEM_ERROR) -> do
         errno <- (#peek PgfExn, code) c_exn
         c_msg <- (#peek PgfExn, msg) c_exn
         mb_fpath <- if c_msg == nullPtr
                       then return Nothing
                       else fmap Just (peekCString c_msg)
         ioError (errnoToIOError loc (Errno errno) Nothing mb_fpath)
      (#const PGF_EXN_PGF_ERROR) -> do
         c_msg <- (#peek PgfExn, msg) c_exn
         msg <- peekCString c_msg
         free c_msg
         throwIO (PGFError loc msg)
      _ -> throwIO (PGFError loc "An unidentified error occurred")

-----------------------------------------------------------------------
-- Marshalling

type CBindType = (#type PgfBindType)

type EAbsFun = Ptr PgfUnmarshaller -> (#type PgfBindType) -> Ptr PgfText -> StablePtr Expr -> IO (StablePtr Expr)

foreign import ccall "dynamic" callEAbsFun :: Dynamic EAbsFun

foreign import ccall "wrapper" wrapEAbsFun :: Wrapper EAbsFun

type EAppFun = Ptr PgfUnmarshaller -> StablePtr Expr -> StablePtr Expr -> IO (StablePtr Expr)

foreign import ccall "dynamic" callEAppFun :: Dynamic EAppFun

foreign import ccall "wrapper" wrapEAppFun :: Wrapper EAppFun

type ELitFun = Ptr PgfUnmarshaller -> StablePtr Literal -> IO (StablePtr Expr)

foreign import ccall "dynamic" callELitFun :: Dynamic ELitFun

foreign import ccall "wrapper" wrapELitFun :: Wrapper ELitFun

type EMetaFun = Ptr PgfUnmarshaller -> (#type PgfMetaId) -> IO (StablePtr Expr)

foreign import ccall "dynamic" callEMetaFun :: Dynamic EMetaFun

foreign import ccall "wrapper" wrapEMetaFun :: Wrapper EMetaFun

type EFunFun = Ptr PgfUnmarshaller -> Ptr PgfText -> IO (StablePtr Expr)

foreign import ccall "dynamic" callEFunFun :: Dynamic EFunFun

foreign import ccall "wrapper" wrapEFunFun :: Wrapper EFunFun

type EVarFun = Ptr PgfUnmarshaller -> CInt -> IO (StablePtr Expr)

foreign import ccall "dynamic" callEVarFun :: Dynamic EVarFun

foreign import ccall "wrapper" wrapEVarFun :: Wrapper EVarFun

type ETypedFun = Ptr PgfUnmarshaller -> StablePtr Expr -> StablePtr Type -> IO (StablePtr Expr)

foreign import ccall "dynamic" callETypedFun :: Dynamic ETypedFun

foreign import ccall "wrapper" wrapETypedFun :: Wrapper ETypedFun

type EImplArgFun = Ptr PgfUnmarshaller -> StablePtr Expr -> IO (StablePtr Expr)

foreign import ccall "dynamic" callEImplArgFun :: Dynamic EImplArgFun

foreign import ccall "wrapper" wrapEImplArgFun :: Wrapper EImplArgFun

type LIntFun = Ptr PgfUnmarshaller -> (#type size_t) -> Ptr (#type uintmax_t) -> IO (StablePtr Literal)

foreign import ccall "dynamic" callLIntFun :: Dynamic LIntFun

foreign import ccall "wrapper" wrapLIntFun :: Wrapper LIntFun

type LFltFun = Ptr PgfUnmarshaller -> CDouble -> IO (StablePtr Literal)

foreign import ccall "dynamic" callLFltFun :: Dynamic LFltFun

foreign import ccall "wrapper" wrapLFltFun :: Wrapper LFltFun

type LStrFun = Ptr PgfUnmarshaller -> Ptr PgfText -> IO (StablePtr Literal)

foreign import ccall "dynamic" callLStrFun :: Dynamic LStrFun

foreign import ccall "wrapper" wrapLStrFun :: Wrapper LStrFun

type DTypFun = Ptr PgfUnmarshaller -> CSize -> Ptr PgfTypeHypo -> Ptr PgfText -> CSize -> Ptr (StablePtr Expr) -> IO (StablePtr Type)

foreign import ccall "dynamic" callDTypFun :: Dynamic DTypFun

foreign import ccall "wrapper" wrapDTypFun :: Wrapper DTypFun

foreign import ccall "&hs_free_reference" hs_free_reference :: FunPtr (Ptr a -> StablePtr a -> IO ())

foreign import ccall "&hs_free_marshaller" hs_free_marshaller :: FinalizerPtr PgfMarshaller

foreign import ccall "&hs_free_unmarshaller" hs_free_unmarshaller :: FinalizerPtr PgfUnmarshaller

foreign import ccall "wrapper" wrapMatchFun :: Wrapper (Ptr PgfMarshaller -> Ptr PgfUnmarshaller -> StablePtr a -> IO (StablePtr a))

{-# NOINLINE marshaller #-}
marshaller = unsafePerformIO $ do
  vtbl <- mallocBytes (#size PgfMarshallerVtbl)
  wrapMatchFun matchLit  >>= (#poke PgfMarshallerVtbl, match_lit)  vtbl
  wrapMatchFun matchExpr >>= (#poke PgfMarshallerVtbl, match_expr) vtbl
  wrapMatchFun matchType >>= (#poke PgfMarshallerVtbl, match_type) vtbl
  ptr <- mallocBytes (#size PgfMarshaller)
  (#poke PgfMarshaller, vtbl) ptr vtbl
  newForeignPtr hs_free_marshaller ptr
  where
    matchLit this u c_lit = do
      vtbl <- (#peek PgfUnmarshaller, vtbl) u
      lit <- deRefStablePtr c_lit
      case lit of
        LStr s -> withText s $ \c_s -> do
                     fun <- (#peek PgfUnmarshallerVtbl, lstr) vtbl
                     callLStrFun fun u c_s
        LInt n -> let abs_n = abs n
                      size  = I## (integerLogBase## (#const LINT_BASE) abs_n +## 1##)
                  in allocaArray size $ \c_v -> do
                       pokeValue c_v (c_v `plusPtr` ((#size uintmax_t) * (size - 1)))
                                 (fromIntegral (signum n)) abs_n
                       fun <- (#peek PgfUnmarshallerVtbl, lint) vtbl
                       callLIntFun fun u (fromIntegral size) c_v
        LFlt d -> do fun <- (#peek PgfUnmarshallerVtbl, lflt) vtbl
                     callLFltFun fun u (realToFrac d)
      where
        pokeValue c_v p sign abs_n
          | c_v == p  = poke p (sign * fromIntegral abs_n)
          | otherwise = do let (q,r) = quotRem abs_n (#const LINT_BASE)
                           poke p (fromIntegral r)
                           pokeValue c_v (p `plusPtr` (- #size uintmax_t)) sign q

    matchExpr this u c_expr = do
      vtbl <- (#peek PgfUnmarshaller, vtbl) u
      expr <- deRefStablePtr c_expr
      case expr of
        EAbs bt var e-> withText var $ \c_var ->
                        bracket (newStablePtr e) freeStablePtr $ \c_e -> do
                           fun <- (#peek PgfUnmarshallerVtbl, eabs) vtbl
                           callEAbsFun fun u (marshalBindType bt) c_var c_e
        EApp fun arg -> bracket (newStablePtr fun) freeStablePtr $ \c_fun ->
                        bracket (newStablePtr arg) freeStablePtr $ \c_arg -> do
                           fun <- (#peek PgfUnmarshallerVtbl, eapp) vtbl
                           callEAppFun fun u c_fun c_arg
        ELit lit     -> bracket (newStablePtr lit) freeStablePtr $ \c_lit -> do
                           fun <- (#peek PgfUnmarshallerVtbl, elit) vtbl
                           callELitFun fun u c_lit
        EMeta id     -> do fun <- (#peek PgfUnmarshallerVtbl, emeta) vtbl
                           callEMetaFun fun u (fromIntegral id)
        EFun name    -> withText name $ \c_name -> do
                           fun <- (#peek PgfUnmarshallerVtbl, efun) vtbl
                           callEFunFun fun u c_name
        EVar index   -> do fun <- (#peek PgfUnmarshallerVtbl, evar) vtbl
                           callEVarFun fun u (fromIntegral index)
        ETyped e ty  -> bracket (newStablePtr e)  freeStablePtr $ \c_e  ->
                        bracket (newStablePtr ty) freeStablePtr $ \c_ty -> do
                           fun <- (#peek PgfUnmarshallerVtbl, etyped) vtbl
                           callETypedFun fun u c_e c_ty
        EImplArg arg -> bracket (newStablePtr arg) freeStablePtr $ \c_arg -> do
                           fun <- (#peek PgfUnmarshallerVtbl, eimplarg) vtbl
                           callEImplArgFun fun u c_arg

    matchType this u c_ty = do
      vtbl <- (#peek PgfUnmarshaller, vtbl) u
      ty   <- deRefStablePtr c_ty
      case ty of
        DTyp hypos cat es -> let n_hypos = length hypos
                             in withHypos hypos $ \n_hypos c_hypos ->
                                withText cat $ \c_cat ->
                                mask_ $ do
                                  c_es <- mapM newStablePtr es
                                  res <- withArray c_es $ \c_exprs -> do
                                           fun <- (#peek PgfUnmarshallerVtbl, dtyp) vtbl
                                           callDTypFun fun u
                                                       n_hypos
                                                       c_hypos
                                                       c_cat
                                                       (fromIntegral (length es))
                                                       c_exprs
                                  mapM_ freeStablePtr c_es
                                  return res
      where
        marshalHypos _   []               = return ()
        marshalHypos ptr ((bt,var,ty):hs) = do
          (#poke PgfTypeHypo, bind_type) ptr (marshalBindType bt)
          newText var     >>= (#poke PgfTypeHypo, cid)  ptr
          newStablePtr ty >>= (#poke PgfTypeHypo, type) ptr
          marshalHypos (ptr `plusPtr` (#size PgfTypeHypo)) hs

        freeHypos ptr 0 = return ()
        freeHypos ptr n = do
          (#peek PgfTypeHypo, cid)  ptr >>= free
          (#peek PgfTypeHypo, type) ptr >>= freeStablePtr
          freeHypos (ptr `plusPtr` (#size PgfTypeHypo)) (n-1)

{-# NOINLINE unmarshaller #-}
unmarshaller = unsafePerformIO $ do
  vtbl <- mallocBytes (#size PgfUnmarshallerVtbl)
  wrapEAbsFun     unmarshalEAbs     >>= (#poke PgfUnmarshallerVtbl, eabs)     vtbl
  wrapEAppFun     unmarshalEApp     >>= (#poke PgfUnmarshallerVtbl, eapp)     vtbl
  wrapELitFun     unmarshalELit     >>= (#poke PgfUnmarshallerVtbl, elit)     vtbl
  wrapEMetaFun    unmarshalEMeta    >>= (#poke PgfUnmarshallerVtbl, emeta)    vtbl
  wrapEFunFun     unmarshalEFun     >>= (#poke PgfUnmarshallerVtbl, efun)     vtbl
  wrapEVarFun     unmarshalEVar     >>= (#poke PgfUnmarshallerVtbl, evar)     vtbl
  wrapETypedFun   unmarshalETyped   >>= (#poke PgfUnmarshallerVtbl, etyped)   vtbl
  wrapEImplArgFun unmarshalEImplArg >>= (#poke PgfUnmarshallerVtbl, eimplarg) vtbl
  wrapLIntFun     unmarshalLInt     >>= (#poke PgfUnmarshallerVtbl, lint)     vtbl
  wrapLFltFun     unmarshalLFlt     >>= (#poke PgfUnmarshallerVtbl, lflt)     vtbl
  wrapLStrFun     unmarshalLStr     >>= (#poke PgfUnmarshallerVtbl, lstr)     vtbl
  wrapDTypFun     unmarshalDTyp     >>= (#poke PgfUnmarshallerVtbl, dtyp)     vtbl
  (#poke PgfUnmarshallerVtbl, free_ref) vtbl hs_free_reference
  ptr <- mallocBytes (#size PgfUnmarshaller)
  (#poke PgfUnmarshaller, vtbl) ptr vtbl
  newForeignPtr hs_free_unmarshaller ptr
  where
    unmarshalEAbs this c_btype c_var c_body = do
      let btype = unmarshalBindType c_btype
      var <- peekText c_var
      body <- deRefStablePtr c_body
      newStablePtr (EAbs btype var body)

    unmarshalEApp this c_fun c_arg = do
      fun <- deRefStablePtr c_fun
      arg <- deRefStablePtr c_arg
      newStablePtr (EApp fun arg)

    unmarshalELit this c_lit = do
      lit <- deRefStablePtr c_lit
      newStablePtr (ELit lit)

    unmarshalEMeta this c_metaid = do
      newStablePtr (EMeta (fromIntegral c_metaid))

    unmarshalEFun this c_name = do
      name <- peekText c_name
      newStablePtr (EFun name)

    unmarshalEVar this c_var = do
      newStablePtr (EVar (fromIntegral c_var))

    unmarshalETyped this c_expr c_typ = do
      expr <- deRefStablePtr c_expr
      typ  <- deRefStablePtr c_typ
      newStablePtr (ETyped expr typ)

    unmarshalEImplArg this c_expr = do
      expr <- deRefStablePtr c_expr
      newStablePtr (EImplArg expr)

    unmarshalLInt this c_size c_v = do
      n <- if c_size == 0
             then return 0
             else do v     <- peek (castPtr c_v :: Ptr (#type intmax_t))
                     abs_n <- peekValue (c_size-1)
                                        (c_v `plusPtr` (#size uintmax_t))
                                        (fromIntegral (abs v))
                     return (fromIntegral (signum v) * abs_n)
      newStablePtr (LInt n)
      where
        peekValue 0      c_v value = return value
        peekValue c_size c_v value = do
          v <- peek (castPtr c_v :: Ptr (#type uintmax_t))
          peekValue (c_size-1)
                    (c_v `plusPtr` (#size uintmax_t))
                    (value*(#const LINT_BASE)+fromIntegral v)

    unmarshalLFlt this c_v = do
      newStablePtr (LFlt (realToFrac c_v))

    unmarshalLStr this c_v = do
      s <- peekText c_v
      newStablePtr (LStr s)

    unmarshalDTyp this n_hypos hypos c_cat n_exprs exprs = do
      hypos <- peekHypos n_hypos hypos
      cat <- peekText c_cat
      exprs <- peekExprs n_exprs exprs
      newStablePtr (DTyp hypos cat exprs)
      where
        peekHypos 0       p_hypo = return []
        peekHypos n_hypos p_hypo = do
          bt  <- fmap unmarshalBindType ((#peek PgfTypeHypo, bind_type) p_hypo)
          cid <- (#peek PgfTypeHypo, cid) p_hypo >>= peekText
          ty  <- (#peek PgfTypeHypo, type) p_hypo >>= deRefStablePtr
          hs  <- peekHypos (n_hypos-1) (p_hypo `plusPtr` (#size PgfTypeHypo))
          return ((bt,cid,ty):hs)

        peekExprs 0       p_expr = return []
        peekExprs n_exprs p_expr = do
          e  <- peek p_expr >>= deRefStablePtr
          es <- peekExprs (n_exprs-1) (p_expr `plusPtr` (#size uintptr_t))
          return (e:es)


marshalBindType :: BindType -> (#type PgfBindType)
marshalBindType Explicit = (#const PGF_BIND_TYPE_EXPLICIT)
marshalBindType Implicit = (#const PGF_BIND_TYPE_IMPLICIT)

unmarshalBindType :: (#type PgfBindType) -> BindType
unmarshalBindType (#const PGF_BIND_TYPE_EXPLICIT) = Explicit
unmarshalBindType (#const PGF_BIND_TYPE_IMPLICIT) = Implicit

withHypos hypos f =
  let n_hypos = length hypos
  in allocaBytes (n_hypos * (#size PgfTypeHypo)) $ \c_hypos ->
     mask_ $ do
       marshalHypos c_hypos hypos
       res <- f (fromIntegral n_hypos :: CSize) c_hypos
       freeHypos n_hypos c_hypos
       return res
  where
    marshalHypos _   []               = return ()
    marshalHypos ptr ((bt,var,ty):hs) = do
      (#poke PgfTypeHypo, bind_type) ptr (marshalBindType bt)
      newText var     >>= (#poke PgfTypeHypo, cid)  ptr
      newStablePtr ty >>= (#poke PgfTypeHypo, type) ptr
      marshalHypos (ptr `plusPtr` (#size PgfTypeHypo)) hs

    freeHypos 0 ptr = return ()
    freeHypos n ptr = do
      (#peek PgfTypeHypo, cid)  ptr >>= free
      (#peek PgfTypeHypo, type) ptr >>= freeStablePtr
      freeHypos (n-1) (ptr `plusPtr` (#size PgfTypeHypo))

getConcretes c_db c_revision = do
  ref <- newIORef Map.empty
  (withForeignPtr c_revision $ \c_revision ->
   allocaBytes (#size PgfItor) $ \itor ->
   bracket (wrapItorCallback (getConcretes ref)) freeHaskellFunPtr $ \fptr -> do
     (#poke PgfItor, fn) itor fptr
     withPgfExn "getConcretes" (pgf_iter_concretes c_db c_revision itor)
     readIORef ref)
  where
    getConcretes :: IORef (Map.Map ConcName Concr) -> ItorCallback
    getConcretes ref itor key c_revision exn = do
      concrs <- readIORef ref
      name  <- peekText key
      fptr <- newForeignPtrEnv pgf_free_concr_revision c_db (castPtr c_revision)
      writeIORef ref (Map.insert name (Concr c_db fptr) concrs)
