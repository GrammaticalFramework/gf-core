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

type ItorCallback = Ptr PgfItor -> Ptr PgfText -> IO ()

foreign import ccall "wrapper"
  wrapItorCallback :: ItorCallback -> IO (FunPtr ItorCallback)

foreign import ccall "pgf_iter_categories"
  pgf_iter_categories :: Ptr PgfPGF -> Ptr PgfItor -> IO ()

foreign import ccall "pgf_iter_functions"
  pgf_iter_functions :: Ptr PgfPGF -> Ptr PgfItor -> IO ()

foreign import ccall "pgf_iter_functions_by_cat"
  pgf_iter_functions_by_cat :: Ptr PgfPGF -> Ptr PgfText -> Ptr PgfItor -> IO ()

foreign import ccall "pgf/pgf.h pgf_function_type"
   pgf_function_type :: Ptr PgfPGF -> Ptr PgfText -> IO (StablePtr Type)


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


type CBindType = (#type PgfBindType)

type EAbsUnmarshaller = (#type PgfBindType) -> Ptr PgfText -> StablePtr Expr -> IO (StablePtr Expr)

foreign import ccall "wrapper"
  wrapEAbsUnmarshaller :: EAbsUnmarshaller -> IO (FunPtr EAbsUnmarshaller)

type EAppUnmarshaller = StablePtr Expr -> StablePtr Expr -> IO (StablePtr Expr)

foreign import ccall "wrapper"
  wrapEAppUnmarshaller :: EAppUnmarshaller -> IO (FunPtr EAppUnmarshaller)

type ELitUnmarshaller = StablePtr Literal -> IO (StablePtr Expr)

foreign import ccall "wrapper"
  wrapELitUnmarshaller :: ELitUnmarshaller -> IO (FunPtr ELitUnmarshaller)

type EMetaUnmarshaller = (#type PgfMetaId) -> IO (StablePtr Expr)

foreign import ccall "wrapper"
  wrapEMetaUnmarshaller :: EMetaUnmarshaller -> IO (FunPtr EMetaUnmarshaller)

type EFunUnmarshaller = Ptr PgfText -> IO (StablePtr Expr)

foreign import ccall "wrapper"
  wrapEFunUnmarshaller :: EFunUnmarshaller -> IO (FunPtr EFunUnmarshaller)

type EVarUnmarshaller = CInt -> IO (StablePtr Expr)

foreign import ccall "wrapper"
  wrapEVarUnmarshaller :: EVarUnmarshaller -> IO (FunPtr EVarUnmarshaller)

type ETypedUnmarshaller = StablePtr Expr -> StablePtr Type -> IO (StablePtr Expr)

foreign import ccall "wrapper"
  wrapETypedUnmarshaller :: ETypedUnmarshaller -> IO (FunPtr ETypedUnmarshaller)

type EImplArgUnmarshaller = StablePtr Expr -> IO (StablePtr Expr)

foreign import ccall "wrapper"
  wrapEImplArgUnmarshaller :: EImplArgUnmarshaller -> IO (FunPtr EImplArgUnmarshaller)

type LIntUnmarshaller = CInt -> IO (StablePtr Literal)

foreign import ccall "wrapper"
  wrapLIntUnmarshaller :: LIntUnmarshaller -> IO (FunPtr LIntUnmarshaller)

type LFltUnmarshaller = CDouble -> IO (StablePtr Literal)

foreign import ccall "wrapper"
  wrapLFltUnmarshaller :: LFltUnmarshaller -> IO (FunPtr LFltUnmarshaller)

type LStrUnmarshaller = Ptr PgfText -> IO (StablePtr Literal)

foreign import ccall "wrapper"
  wrapLStrUnmarshaller :: LStrUnmarshaller -> IO (FunPtr LStrUnmarshaller)

type TypeUnmarshaller = CInt -> Ptr PgfTypeHypo -> Ptr PgfText -> CInt -> Ptr (StablePtr Expr) -> IO (StablePtr Type)

foreign import ccall "wrapper"
  wrapTypeUnmarshaller :: TypeUnmarshaller -> IO (FunPtr TypeUnmarshaller)

foreign import ccall "&hs_free_stable_ptr" hs_free_stable_ptr :: FunPtr (StablePtr a -> IO ())

foreign import ccall "&hs_free_unmarshaller" hs_free_unmarshaller :: FunPtr (Ptr PgfUnmarshaller -> IO ())

foreign import ccall "hs_free_unmarshaller" freeUnmarshaller :: Ptr PgfUnmarshaller -> IO ()

mkUnmarshaller = do
  eabs    <- wrapEAbsUnmarshaller     unmarshalEAbs
  eapp    <- wrapEAppUnmarshaller     unmarshalEApp
  elit    <- wrapELitUnmarshaller     unmarshalELit
  emeta   <- wrapEMetaUnmarshaller    unmarshalEMeta
  efun    <- wrapEFunUnmarshaller     unmarshalEFun
  evar    <- wrapEVarUnmarshaller     unmarshalEVar
  etyped  <- wrapETypedUnmarshaller   unmarshalETyped
  eimplarg<- wrapEImplArgUnmarshaller unmarshalEImplArg
  lint    <- wrapLIntUnmarshaller     unmarshalLInt
  lflt    <- wrapLFltUnmarshaller     unmarshalLFlt
  lstr    <- wrapLStrUnmarshaller     unmarshalLStr
  dtyp    <- wrapTypeUnmarshaller     unmarshalType
  ptr <- mallocBytes (#size PgfUnmarshaller)
  (#poke PgfUnmarshaller, eabs)     ptr eabs
  (#poke PgfUnmarshaller, eapp)     ptr eapp
  (#poke PgfUnmarshaller, elit)     ptr elit
  (#poke PgfUnmarshaller, emeta)    ptr emeta
  (#poke PgfUnmarshaller, efun)     ptr efun
  (#poke PgfUnmarshaller, evar)     ptr evar
  (#poke PgfUnmarshaller, etyped)   ptr etyped
  (#poke PgfUnmarshaller, eimplarg) ptr eimplarg
  (#poke PgfUnmarshaller, lint)     ptr lint
  (#poke PgfUnmarshaller, lflt)     ptr lflt
  (#poke PgfUnmarshaller, lstr)     ptr lstr
  (#poke PgfUnmarshaller, dtyp)     ptr dtyp
  (#poke PgfUnmarshaller, free_ref) ptr hs_free_stable_ptr
  (#poke PgfUnmarshaller, free_me)  ptr hs_free_unmarshaller
  return ptr
  where
    unmarshalEAbs c_btype c_var c_body = do
      let btype = unmarshalBindType c_btype
      var <- peekText c_var
      body <- deRefStablePtr c_body
      newStablePtr (EAbs btype var body)

    unmarshalEApp c_fun c_arg = do
      fun <- deRefStablePtr c_fun
      arg <- deRefStablePtr c_arg
      newStablePtr (EApp fun arg)

    unmarshalELit c_lit = do
      lit <- deRefStablePtr c_lit
      newStablePtr (ELit lit)

    unmarshalEMeta c_metaid = do
      newStablePtr (EMeta (fromIntegral c_metaid))

    unmarshalEFun c_name = do
      name <- peekText c_name
      newStablePtr (EFun name)

    unmarshalEVar c_var = do
      newStablePtr (EVar (fromIntegral c_var))

    unmarshalETyped c_expr c_typ = do
      expr <- deRefStablePtr c_expr
      typ  <- deRefStablePtr c_typ
      newStablePtr (ETyped expr typ)

    unmarshalEImplArg c_expr = do
      expr <- deRefStablePtr c_expr
      newStablePtr (EImplArg expr)

    unmarshalLInt c_v = do
      newStablePtr (LInt (fromIntegral c_v))

    unmarshalLFlt c_v = do
      newStablePtr (LFlt (realToFrac c_v))

    unmarshalLStr c_v = do
      s <- peekText c_v
      newStablePtr (LStr s)

    unmarshalType n_hypos hypos c_cat n_exprs exprs = do
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
          hs  <- peekExprs (n_hypos-1) (p_hypo `plusPtr` (#size PgfTypeHypo))
          return ((bt,cid,ty):hs)

        peekExprs 0       p_expr = return []
        peekExprs n_exprs p_expr = do
          e  <- peek p_expr >>= deRefStablePtr
          es <- peekExprs (n_exprs-1) (p_expr `plusPtr` (#size uintptr_t))
          return (e:es)


unmarshalBindType :: (#type PgfBindType) -> BindType
unmarshalBindType (#const PGF_BIND_TYPE_EXPLICIT) = Explicit
unmarshalBindType (#const PGF_BIND_TYPE_IMPLICIT) = Implicit
