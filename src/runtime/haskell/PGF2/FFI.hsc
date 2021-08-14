{-# LANGUAGE ForeignFunctionInterface, MagicHash, BangPatterns #-}

module PGF2.FFI where

import Data.Word
import Foreign
import Foreign.C
import Foreign.Ptr
import qualified Data.Map as Map

import PGF2.Expr

#include <pgf/pgf.h>

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
data PgfTypeHypo
data PgfMarshaller
data PgfUnmarshaller

foreign import ccall unsafe "pgf_utf8_decode"
  pgf_utf8_decode :: Ptr CString -> IO Word32

foreign import ccall unsafe "pgf_utf8_encode"
  pgf_utf8_encode :: Word32 -> Ptr CString -> IO ()

foreign import ccall "pgf_read_pgf"
  pgf_read_pgf :: CString -> Ptr PgfUnmarshaller -> Ptr PgfExn -> IO (Ptr PgfPGF)

foreign import ccall "pgf_boot_ngf"
  pgf_boot_ngf :: CString -> CString -> Ptr PgfUnmarshaller -> Ptr PgfExn -> IO (Ptr PgfPGF)

foreign import ccall "pgf_read_ngf"
  pgf_read_ngf :: CString -> Ptr PgfUnmarshaller -> Ptr PgfExn -> IO (Ptr PgfPGF)

foreign import ccall "&pgf_free"
  pgf_free_fptr :: FinalizerPtr PgfPGF

foreign import ccall "pgf_abstract_name"
  pgf_abstract_name :: Ptr PgfPGF -> IO (Ptr PgfText)

foreign import ccall "pgf_read_expr"
  pgf_read_expr :: Ptr PgfText -> Ptr PgfUnmarshaller -> IO (StablePtr Expr)

foreign import ccall "pgf_read_type"
  pgf_read_type :: Ptr PgfText -> Ptr PgfUnmarshaller -> IO (StablePtr Type)

type ItorCallback = Ptr PgfItor -> Ptr PgfText -> IO ()

foreign import ccall "wrapper"
  wrapItorCallback :: ItorCallback -> IO (FunPtr ItorCallback)

foreign import ccall "pgf_iter_categories"
  pgf_iter_categories :: Ptr PgfPGF -> Ptr PgfItor -> IO ()

foreign import ccall "pgf_start_cat"
  pgf_start_cat :: Ptr PgfPGF -> IO (StablePtr Type)

foreign import ccall "pgf/pgf.h pgf_category_context"
  pgf_category_context :: Ptr PgfPGF -> Ptr PgfText -> Ptr CSize -> IO (Ptr PgfTypeHypo)

foreign import ccall "pgf/pgf.h pgf_category_prob"
  pgf_category_prob :: Ptr PgfPGF -> Ptr PgfText -> IO (#type prob_t)

foreign import ccall "pgf_iter_functions"
  pgf_iter_functions :: Ptr PgfPGF -> Ptr PgfItor -> IO ()

foreign import ccall "pgf_iter_functions_by_cat"
  pgf_iter_functions_by_cat :: Ptr PgfPGF -> Ptr PgfText -> Ptr PgfItor -> IO ()

foreign import ccall "pgf/pgf.h pgf_function_type"
   pgf_function_type :: Ptr PgfPGF -> Ptr PgfText -> IO (StablePtr Type)

foreign import ccall "pgf/expr.h pgf_function_is_constructor"
   pgf_function_is_constructor :: Ptr PgfPGF -> Ptr PgfText -> IO (#type int)

foreign import ccall "pgf/expr.h pgf_function_is_constructor"
   pgf_function_prob :: Ptr PgfPGF -> Ptr PgfText -> IO (#type prob_t)

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


type CBindType = (#type PgfBindType)

type EAbsFun = Ptr PgfUnmarshaller -> (#type PgfBindType) -> Ptr PgfText -> StablePtr Expr -> IO (StablePtr Expr)

foreign import ccall "dynamic"
  callEAbsFun :: FunPtr EAbsFun -> EAbsFun

foreign import ccall "wrapper"
  wrapEAbsFun :: EAbsFun -> IO (FunPtr EAbsFun)

type EAppFun = Ptr PgfUnmarshaller -> StablePtr Expr -> StablePtr Expr -> IO (StablePtr Expr)

foreign import ccall "dynamic"
  callEAppFun :: FunPtr EAppFun -> EAppFun

foreign import ccall "wrapper"
  wrapEAppFun :: EAppFun -> IO (FunPtr EAppFun)

type ELitFun = Ptr PgfUnmarshaller -> StablePtr Literal -> IO (StablePtr Expr)

foreign import ccall "dynamic"
  callELitFun :: FunPtr ELitFun -> ELitFun

foreign import ccall "wrapper"
  wrapELitFun :: ELitFun -> IO (FunPtr ELitFun)

type EMetaFun = Ptr PgfUnmarshaller -> (#type PgfMetaId) -> IO (StablePtr Expr)

foreign import ccall "dynamic"
  callEMetaFun :: FunPtr EMetaFun -> EMetaFun

foreign import ccall "wrapper"
  wrapEMetaFun :: EMetaFun -> IO (FunPtr EMetaFun)

type EFunFun = Ptr PgfUnmarshaller -> Ptr PgfText -> IO (StablePtr Expr)

foreign import ccall "dynamic"
  callEFunFun :: FunPtr EFunFun -> EFunFun

foreign import ccall "wrapper"
  wrapEFunFun :: EFunFun -> IO (FunPtr EFunFun)

type EVarFun = Ptr PgfUnmarshaller -> CInt -> IO (StablePtr Expr)

foreign import ccall "dynamic"
  callEVarFun :: FunPtr EVarFun -> EVarFun

foreign import ccall "wrapper"
  wrapEVarFun :: EVarFun -> IO (FunPtr EVarFun)

type ETypedFun = Ptr PgfUnmarshaller -> StablePtr Expr -> StablePtr Type -> IO (StablePtr Expr)

foreign import ccall "dynamic"
  callETypedFun :: FunPtr ETypedFun -> ETypedFun

foreign import ccall "wrapper"
  wrapETypedFun :: ETypedFun -> IO (FunPtr ETypedFun)

type EImplArgFun = Ptr PgfUnmarshaller -> StablePtr Expr -> IO (StablePtr Expr)

foreign import ccall "dynamic"
  callEImplArgFun :: FunPtr EImplArgFun -> EImplArgFun

foreign import ccall "wrapper"
  wrapEImplArgFun :: EImplArgFun -> IO (FunPtr EImplArgFun)

type LIntFun = Ptr PgfUnmarshaller -> CInt -> IO (StablePtr Literal)

foreign import ccall "dynamic"
  callLIntFun :: FunPtr LIntFun -> LIntFun

foreign import ccall "wrapper"
  wrapLIntFun :: LIntFun -> IO (FunPtr LIntFun)

type LFltFun = Ptr PgfUnmarshaller -> CDouble -> IO (StablePtr Literal)

foreign import ccall "dynamic"
  callLFltFun :: FunPtr LFltFun -> LFltFun

foreign import ccall "wrapper"
  wrapLFltFun :: LFltFun -> IO (FunPtr LFltFun)

type LStrFun = Ptr PgfUnmarshaller -> Ptr PgfText -> IO (StablePtr Literal)

foreign import ccall "dynamic"
  callLStrFun :: FunPtr LStrFun -> LStrFun

foreign import ccall "wrapper"
  wrapLStrFun :: LStrFun -> IO (FunPtr LStrFun)

type DTypFun = Ptr PgfUnmarshaller -> CInt -> Ptr PgfTypeHypo -> Ptr PgfText -> CInt -> Ptr (StablePtr Expr) -> IO (StablePtr Type)

foreign import ccall "dynamic"
  callDTypFun :: FunPtr DTypFun -> DTypFun

foreign import ccall "wrapper"
  wrapDTypFun :: DTypFun -> IO (FunPtr DTypFun)

foreign import ccall "&hs_free_reference" hs_free_reference :: FunPtr (Ptr PgfUnmarshaller -> StablePtr a -> IO ())

foreign import ccall "&hs_free_unmarshaller" hs_free_unmarshaller :: FunPtr (Ptr PgfUnmarshaller -> IO ())

foreign import ccall "hs_free_unmarshaller" freeUnmarshaller :: Ptr PgfUnmarshaller -> IO ()

type MatchFun a = Ptr PgfMarshaller -> Ptr PgfUnmarshaller -> StablePtr a -> IO (StablePtr a)

foreign import ccall "wrapper"
  wrapMatchFun :: MatchFun a -> IO (FunPtr (MatchFun a))

mkMarshaller = do
  vtbl <- mallocBytes (#size PgfMarshallerVtbl)
  wrapMatchFun matchLit  >>= (#poke PgfMarshallerVtbl, match_lit)  vtbl
  wrapMatchFun matchExpr >>= (#poke PgfMarshallerVtbl, match_expr) vtbl
  wrapMatchFun matchType >>= (#poke PgfMarshallerVtbl, match_type) vtbl
  ptr <- mallocBytes (#size PgfMarshaller)
  (#poke PgfMarshaller, vtbl) ptr vtbl
  return ptr
  where
    matchLit this u c_lit = do
      vtbl <- (#peek PgfUnmarshaller, vtbl) u
      lit <- deRefStablePtr c_lit
      case lit of
        LStr s -> do fun <- (#peek PgfUnmarshallerVtbl, lstr) vtbl
                     c_s <- newText s
                     callLStrFun fun u c_s
        LInt n -> do fun <- (#peek PgfUnmarshallerVtbl, lint) vtbl
                     callLIntFun fun u (fromIntegral n)
        LFlt d -> do fun <- (#peek PgfUnmarshallerVtbl, lflt) vtbl
                     callLFltFun fun u (realToFrac d)

    matchExpr this u c_expr = do
      vtbl <- (#peek PgfUnmarshaller, vtbl) u
      expr <- deRefStablePtr c_expr
      case expr of
        EAbs bt var e-> do c_e <- newStablePtr e
                           fun <- (#peek PgfUnmarshallerVtbl, eabs) vtbl
                           c_var <- newText var
                           callEAbsFun fun u (marshalBindType bt) c_var c_e
        EApp fun arg -> do c_fun <- newStablePtr fun
                           c_arg <- newStablePtr arg
                           fun <- (#peek PgfUnmarshallerVtbl, eapp) vtbl
                           callEAppFun fun u c_fun c_arg
        ELit lit     -> do c_lit <- newStablePtr lit
                           fun <- (#peek PgfUnmarshallerVtbl, elit) vtbl
                           callELitFun fun u c_lit
        EMeta id     -> do fun <- (#peek PgfUnmarshallerVtbl, emeta) vtbl
                           callEMetaFun fun u (fromIntegral id)
        EFun name    -> do fun <- (#peek PgfUnmarshallerVtbl, efun) vtbl
                           c_name <- newText name
                           callEFunFun fun u c_name
        EVar index   -> do fun <- (#peek PgfUnmarshallerVtbl, evar) vtbl
                           callEVarFun fun u (fromIntegral index)
        ETyped e ty  -> do c_e  <- newStablePtr e
                           c_ty <- newStablePtr ty
                           fun <- (#peek PgfUnmarshallerVtbl, etyped) vtbl
                           callETypedFun fun u c_e c_ty
        EImplArg arg -> do c_arg <- newStablePtr arg
                           fun <- (#peek PgfUnmarshallerVtbl, eimplarg) vtbl
                           callEImplArgFun fun u c_arg

    matchType this u c_ty = do
      vtbl <- (#peek PgfUnmarshaller, vtbl) u
      ty   <- deRefStablePtr c_ty
      case ty of
        DTyp hypos cat es -> do fun <- (#peek PgfUnmarshallerVtbl, dtyp) vtbl
                                let n_hypos = length hypos
                                c_hypos <- mallocBytes (n_hypos * (#size PgfTypeHypo))
                                marshalHypos c_hypos hypos
                                c_cat <- newText cat
                                c_es <- mapM newStablePtr es >>= newArray
                                callDTypFun fun u
                                            (fromIntegral n_hypos)
                                            c_hypos
                                            c_cat
                                            (fromIntegral (length es))
                                            c_es
      where
        marshalHypos _   []               = return ()
        marshalHypos ptr ((bt,var,ty):hs) = do
          (#poke PgfTypeHypo, bind_type) ptr (marshalBindType bt)
          newText var     >>= (#poke PgfTypeHypo, cid)  ptr
          newStablePtr ty >>= (#poke PgfTypeHypo, type) ptr
          marshalHypos (ptr `plusPtr` (#size PgfTypeHypo)) hs

mkUnmarshaller = do
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
  (#poke PgfUnmarshallerVtbl, free_me)  vtbl hs_free_unmarshaller
  ptr <- mallocBytes (#size PgfUnmarshaller)
  (#poke PgfUnmarshaller, vtbl) ptr vtbl
  return ptr
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

    unmarshalLInt this c_v = do
      newStablePtr (LInt (fromIntegral c_v))

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
