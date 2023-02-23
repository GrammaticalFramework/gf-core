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
             PGF,readPGF,bootNGF,readNGF,newNGF,writePGF,showPGF,
             readPGFWithProbs, bootNGFWithProbs,

             -- * Abstract syntax
             AbsName,abstractName,globalFlag,abstractFlag,

             -- ** Categories
             Cat,categories,categoryContext,categoryProbability,

             -- ** Functions
             Fun, functions, functionsByCat,
             functionType, functionIsConstructor, functionProbability,

             -- ** Expressions
             Expr(..), Literal(..), showExpr, readExpr,
             mkAbs,    unAbs, Var,
             mkApp,    unApp, unapply,
             mkVar,    unVar,
             mkStr,    unStr,
             mkInt,    unInt,
             mkDouble, unDouble,
             mkFloat,  unFloat,
             mkMeta,   unMeta,
             -- extra
             exprSize, exprFunctions, exprSubstitute, exprProbability,

             -- ** Types
             Type(..), Hypo, BindType(..), startCat,
             readType, showType, readContext, showContext,
             mkType, unType,
             mkHypo, mkDepHypo, mkImplHypo,

             -- ** Type checking
             -- | Dynamically-built expressions should always be type-checked before using in other functions,
             -- as the exceptions thrown by using invalid expressions may not catchable.
             checkExpr, inferExpr, checkType, checkContext,

             -- ** Computing
             compute,

             -- ** Generation
             generateAll, generateAllFrom, generateRandom, generateRandomFrom,

             -- ** Morphological Analysis
             MorphoAnalysis, lookupMorpho, lookupCohorts, fullFormLexicon,
             filterBest, filterLongest,

             -- ** Visualizations
             GraphvizOptions(..), graphvizDefaults,
             graphvizAbstractTree, graphvizParseTree,
             Labels, getDepLabels,
             graphvizDependencyTree, conlls2latexDoc, getCncDepLabels,
             graphvizWordAlignment,

             -- * Concrete syntax
             ConcName,Concr,languages,concreteName,languageCode,concreteFlag,

             -- ** Linearization
             linearize, linearizeAll, tabularLinearize, tabularLinearizeAll,
             FId, BracketedString(..), showBracketedString, flattenBracketedString,
             bracketedLinearize, bracketedLinearizeAll,
             hasLinearization, categoryFields,
             printName, alignWords, gizaAlignment,

             -- ** Parsing
             ParseOutput(..), parse, parseWithHeuristics, complete,

             -- * Exceptions
             PGFError(..),

             -- * Auxiliaries             
             readProbabilitiesFromFile
             ) where

import Prelude hiding ((<>))

import PGF2.Expr
import PGF2.FFI

import Foreign
import Foreign.C
import Control.Monad(forM,forM_)
import Control.Exception(bracket,mask_,throwIO)
import System.IO.Unsafe(unsafePerformIO, unsafeInterleaveIO)
import System.Random
import qualified Data.Map as Map
import Data.IORef
import Data.List(intersperse,groupBy)
import Data.Char(isUpper,isSpace,isPunctuation)
import Data.Maybe(maybe)
import Text.PrettyPrint

#include <pgf/pgf.h>

-- | Reads a PGF file and keeps it in memory.
readPGF :: FilePath -> IO PGF
readPGF fpath = readPGFWithProbs fpath Nothing

readPGFWithProbs :: FilePath -> Maybe (Map.Map String Double) -> IO PGF
readPGFWithProbs fpath mb_probs =
  withCString fpath $ \c_fpath ->
  alloca $ \p_revision ->
  withProbsCallback mb_probs $ \c_pcallback ->
  mask_ $ do
    c_db <- withPgfExn "readPGF" (pgf_read_pgf c_fpath p_revision c_pcallback)
    c_revision <- peek p_revision
    fptr <- newForeignPtrEnv pgf_free_revision c_db c_revision
    langs <- getConcretes c_db fptr
    return (PGF c_db fptr langs)

-- | Reads a PGF file and stores the unpacked data in an NGF file
-- ready to be shared with other process, or used for quick startup.
-- The NGF file is platform dependent and should not be copied
-- between machines.
bootNGF :: FilePath -> FilePath -> IO PGF
bootNGF pgf_path ngf_path = bootNGFWithProbs pgf_path Nothing ngf_path

bootNGFWithProbs :: FilePath -> Maybe (Map.Map String Double) -> FilePath -> IO PGF
bootNGFWithProbs pgf_path mb_probs ngf_path =
  withCString pgf_path $ \c_pgf_path ->
  withCString ngf_path $ \c_ngf_path ->
  alloca $ \p_revision ->
  withProbsCallback mb_probs $ \c_pcallback ->
  mask_ $ do
    c_db <- withPgfExn "bootNGF" (pgf_boot_ngf c_pgf_path c_ngf_path p_revision c_pcallback)
    c_revision <- peek p_revision
    fptr <- newForeignPtrEnv pgf_free_revision c_db c_revision
    langs <- getConcretes c_db fptr
    return (PGF c_db fptr langs)

withProbsCallback :: Maybe (Map.Map String Double) -> (Ptr PgfProbsCallback -> IO a) -> IO a
withProbsCallback Nothing      f = f nullPtr
withProbsCallback (Just probs) f =
  allocaBytes (#size PgfProbsCallback) $ \callback ->
  bracket (wrapProbsCallback getProb) freeHaskellFunPtr $ \fptr -> do
    (#poke PgfProbsCallback, fn) callback fptr
    f callback
  where
    getProb _ c_name = do
      name <- peekText c_name
      case Map.lookup name probs of
        Nothing -> return nan
        Just p  -> return p

    nan = log (-1)

-- | Reads the grammar from an already booted NGF file.
-- The function fails if the file does not exist.
readNGF :: FilePath -> IO PGF
readNGF fpath =
  withCString fpath $ \c_fpath ->
  alloca $ \p_revision ->
  mask_ $ do
    c_db <- withPgfExn "readNGF" (pgf_read_ngf c_fpath p_revision)
    c_revision <- peek p_revision
    fptr <- newForeignPtrEnv pgf_free_revision c_db c_revision
    langs <- getConcretes c_db fptr
    return (PGF c_db fptr langs)

-- | Creates a new NGF file with a grammar with the given abstract_name.
-- Aside from the name, the grammar is otherwise empty but can be later
-- populated with new functions and categories. If fpath is Nothing then
-- the file is not stored on the disk but only in memory.
newNGF :: AbsName -> Maybe FilePath -> Int -> IO PGF
newNGF abs_name mb_fpath init_size =
  withText abs_name $ \c_abs_name ->
  maybe (\f -> f nullPtr) withCString mb_fpath $ \c_fpath ->
  alloca $ \p_revision ->
  mask_ $ do
    c_db <- withPgfExn "newNGF" (pgf_new_ngf c_abs_name c_fpath (fromIntegral init_size) p_revision)
    c_revision <- peek p_revision
    fptr <- newForeignPtrEnv pgf_free_revision c_db c_revision
    return (PGF c_db fptr Map.empty)

writePGF :: FilePath -> PGF -> Maybe [ConcName] -> IO ()
writePGF fpath p mb_langs =
  withCString fpath $ \c_fpath ->
  withForeignPtr (a_revision p) $ \c_revision ->
  maybe (\f -> f nullPtr) (withLangs []) mb_langs $ \c_langs ->
    withPgfExn "writePGF" (pgf_write_pgf c_fpath (a_db p) c_revision c_langs)
  where
    withLangs clangs []           f = withArray0 nullPtr (reverse clangs) f
    withLangs clangs (lang:langs) f = withText lang $ \clang -> withLangs (clang:clangs) langs f

showPGF :: PGF -> String
showPGF p =
  render (text "abstract" <+> ppAbstractName p <+> char '{' $$
          nest 2 (ppStartCat p $$
                  ppAbsCats p $$
                  ppAbsFuns p) $$
          char '}' $$
          Map.foldrWithKey (\name concr doc -> ppConcr name concr $$ doc) empty (languages p))
  where
    ppStartCat p =
      unsafePerformIO $
      withForeignPtr (a_revision p) $ \c_revision -> do
        c_text <- withPgfExn "showPGF" (pgf_print_start_cat_internal (a_db p) c_revision)
        if c_text == nullPtr
          then return empty
          else do s <- peekText c_text
                  return (text "flags" <+> text s)

    ppAbstractName p =
      unsafePerformIO $
      withForeignPtr (a_revision p) $ \c_revision ->
      bracket (withPgfExn "showPGF" (pgf_abstract_name (a_db p) c_revision)) free $ \c_text ->
      bracket (pgf_print_ident c_text) free $ \c_text ->
        fmap text (peekText c_text)

    ppAbsCats p = unsafePerformIO $ do
      ref <- newIORef empty
      (allocaBytes (#size PgfItor) $ \itor ->
       bracket (wrapItorCallback (getCategories ref)) freeHaskellFunPtr $ \fptr ->
       withForeignPtr (a_revision p) $ \c_revision -> do
         (#poke PgfItor, fn) itor fptr
         withPgfExn "showPGF" (pgf_iter_categories (a_db p) c_revision itor))
      readIORef ref
      where
        getCategories :: IORef Doc -> ItorCallback
        getCategories ref itor key val exn = do
          def <- bracket (pgf_print_category_internal val) free peekText
          modifyIORef ref $ (\doc -> doc $$ text def)

    ppAbsFuns p = unsafePerformIO $ do
      ref <- newIORef empty
      (allocaBytes (#size PgfItor) $ \itor ->
       bracket (wrapItorCallback (getFunctions ref)) freeHaskellFunPtr $ \fptr ->
       withForeignPtr (a_revision p) $ \c_revision -> do
         (#poke PgfItor, fn) itor fptr
         withPgfExn "showPGF" (pgf_iter_functions (a_db p) c_revision itor))
      readIORef ref
      where
        getFunctions :: IORef Doc -> ItorCallback
        getFunctions ref itor key val exn = do
          def <- bracket (pgf_print_function_internal val) free peekText
          modifyIORef ref (\doc -> doc $$ text def)

    ppConcr name c = unsafePerformIO $ do
      (seq_ids,doc3) <- prepareSequences c -- run first to update all seq_id
      doc1 <- ppLincats seq_ids c
      doc2 <- ppLins seq_ids c
      pgf_release_phrasetable_ids seq_ids
      return (text "concrete" <+> text name <+> char '{' $$
              nest 2 (doc1 $$
                      doc2 $$
                      (text "sequences" <+> char '{' $$ 
                       nest 2 doc3 $$ 
                       char '}')) $$
              char '}')

    ppLincats seq_ids c = do
      ref <- newIORef empty
      (allocaBytes (#size PgfItor) $ \itor ->
       bracket (wrapItorCallback (getLincats ref)) freeHaskellFunPtr $ \fptr ->
       withForeignPtr (c_revision c) $ \c_revision -> do
         (#poke PgfItor, fn) itor fptr
         withPgfExn "showPGF" (pgf_iter_lincats (a_db p) c_revision itor))
      readIORef ref
      where
        getLincats :: IORef Doc -> ItorCallback
        getLincats ref itor key val exn = do
          name   <- bracket (pgf_print_ident key) free $ \c_text -> do
                      peekText c_text
          (n_fields,n_lindefs,n_linrefs) <-
                    allocaBytes (3*(#size size_t)) $ \pcounts -> do
                      pgf_get_lincat_counts_internal val pcounts
                      n_fields  <- peekElemOff pcounts 0
                      n_lindefs <- peekElemOff pcounts 1
                      n_linrefs <- peekElemOff pcounts 2
                      return (n_fields,n_lindefs,n_linrefs)
          fields <- forM (init [0..n_fields]) $ \i -> do
                      pgf_get_lincat_field_internal val i >>= peekText
          let def = text "lincat" <+> (text name <+> char '=' <+> char '[' $$
                                       nest 2 (vcat (map (text.show) fields)) $$
                                       char ']')
          modifyIORef ref $ (\doc -> doc $$ def)
          forM_ (init [0..n_lindefs]) $ \i -> do
            def <- bracket (pgf_print_lindef_internal seq_ids val i) free $ \c_text -> do
                     fmap text (peekText c_text)
            modifyIORef ref (\doc -> doc $$ text "lindef" <+> def)
          forM_ (init [0..n_linrefs]) $ \i -> do
            def <- bracket (pgf_print_linref_internal seq_ids val i) free $ \c_text -> do
                     fmap text (peekText c_text)
            modifyIORef ref $ (\doc -> doc $$ text "linref" <+> def)

    ppLins seq_ids c = do
      ref <- newIORef empty
      (allocaBytes (#size PgfItor) $ \itor ->
       bracket (wrapItorCallback (getLins ref)) freeHaskellFunPtr $ \fptr ->
       withForeignPtr (c_revision c) $ \c_revision -> do
         (#poke PgfItor, fn) itor fptr
         withPgfExn "showPGF" (pgf_iter_lins (a_db p) c_revision itor))
      readIORef ref
      where
        getLins :: IORef Doc -> ItorCallback
        getLins ref itor key val exn = do
          n_prods <- pgf_get_lin_get_prod_count val
          forM_ (init [0..n_prods]) $ \i -> do
            def <- bracket (pgf_print_lin_internal seq_ids val i) free $ \c_text -> do
                     fmap text (peekText c_text)
            modifyIORef ref (\doc -> doc $$ text "lin" <+> def)
            return ()

    prepareSequences c = do
      ref <- newIORef empty
      seq_ids <- (allocaBytes (#size PgfSequenceItor) $ \itor ->
                  bracket (wrapSequenceItorCallback (getSequences ref)) freeHaskellFunPtr $ \fptr ->
                  withForeignPtr (c_revision c) $ \c_revision -> do
                    (#poke PgfSequenceItor, fn) itor fptr
                    withPgfExn "showPGF" (pgf_iter_sequences (a_db p) c_revision itor nullPtr))
      doc <- readIORef ref
      return (seq_ids, doc)
      where
        getSequences :: IORef Doc -> SequenceItorCallback
        getSequences ref itor seq_id val exn = do
          def <- bracket (pgf_print_sequence_internal seq_id val) free $ \c_text -> do
                     fmap text (peekText c_text)
          modifyIORef ref $ (\doc -> doc $$ def)
          return 0

-- | The abstract language name is the name of the top-level
-- abstract module
abstractName :: PGF -> AbsName
abstractName p =
  unsafePerformIO $
  withForeignPtr (a_revision p) $ \c_revision ->
  bracket (withPgfExn "abstractName" (pgf_abstract_name (a_db p) c_revision)) free $ \c_text ->
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
  withForeignPtr (a_revision p) $ \c_revision -> do
    c_typ <- withPgfExn "startCat" (pgf_start_cat (a_db p) c_revision u)
    typ <- deRefStablePtr c_typ
    freeStablePtr c_typ
    return typ

-- | The type of a function
functionType :: PGF -> Fun -> Maybe Type
functionType p fn =
  unsafePerformIO $
  withForeignPtr unmarshaller $ \u ->
  withForeignPtr (a_revision p) $ \c_revision ->
  withText fn $ \c_fn -> do
    c_typ <- withPgfExn "functionType" (pgf_function_type (a_db p) c_revision c_fn u)
    if c_typ == castPtrToStablePtr nullPtr
      then return Nothing
      else do typ <- deRefStablePtr c_typ
              freeStablePtr c_typ
              return (Just typ)

functionIsConstructor :: PGF -> Fun -> Bool
functionIsConstructor p fun =
  unsafePerformIO $
  withText fun $ \c_fun ->
  withForeignPtr (a_revision p) $ \c_revision ->
      do res <- withPgfExn "functionIsConstructor" (pgf_function_is_constructor (a_db p) c_revision c_fun)
         return (res /= 0)

functionProbability :: PGF -> Fun -> Float
functionProbability p fun =
  unsafePerformIO $
  withText fun $ \c_fun ->
  withForeignPtr (a_revision p) $ \c_revision ->
      withPgfExn "functionProbability" (pgf_function_prob (a_db p) c_revision c_fun)

exprProbability :: PGF -> Expr -> Float
exprProbability p e =
  unsafePerformIO $
  withForeignPtr (a_revision p) $ \c_revision ->
  bracket (newStablePtr e) freeStablePtr $ \c_e ->
  withForeignPtr marshaller $ \m ->
    withPgfExn "exprProbability" (pgf_expr_prob (a_db p) c_revision c_e m)

checkExpr :: PGF -> Expr -> Type -> Either String Expr
checkExpr = error "TODO: checkExpr"

-- | Tries to infer the type of an expression. Note that
-- even if the expression is type correct it is not always
-- possible to infer its type in the GF type system.
-- In this case the function returns an error.
inferExpr :: PGF -> Expr -> Either String (Expr, Type)
inferExpr p e =
  unsafePerformIO $
  withForeignPtr marshaller $ \m ->
  withForeignPtr unmarshaller $ \u ->
  withForeignPtr (a_revision p) $ \c_revision ->
  bracket (newStablePtr e) freeStablePtr $ \c_e ->
  alloca $ \p_e ->
  allocaBytes (#size PgfExn) $ \c_exn -> do
    poke p_e c_e
    c_ty <- pgf_infer_expr (a_db p) c_revision p_e m u c_exn
    ex_type <- (#peek PgfExn, type) c_exn :: IO (#type PgfExnType)
    case ex_type of
      (#const PGF_EXN_NONE) -> do
         c_e <- peek p_e
         e  <- deRefStablePtr c_e
         freeStablePtr c_e
         ty <- deRefStablePtr c_ty
         freeStablePtr c_ty
         return (Right (e,ty))
      (#const PGF_EXN_SYSTEM_ERROR) -> do
         errno <- (#peek PgfExn, code) c_exn
         c_msg <- (#peek PgfExn, msg) c_exn
         mb_fpath <- if c_msg == nullPtr
                       then return Nothing
                       else fmap Just (peekCString c_msg)
         ioError (errnoToIOError "inferExpr" (Errno errno) Nothing mb_fpath)
      (#const PGF_EXN_PGF_ERROR) -> do
         c_msg <- (#peek PgfExn, msg) c_exn
         msg <- peekCString c_msg
         free c_msg
         return (Left msg)
      _ -> throwIO (PGFError "inferExpr" "An unidentified error occurred")

-- | Check whether a type is consistent with the abstract
-- syntax of the grammar.
checkType :: PGF -> Type -> Either String Type
checkType pgf ty = Right ty

-- | Check whether a context is consistent with the abstract
-- syntax of the grammar.
checkContext :: PGF -> [Hypo] -> Either String [Hypo]
checkContext pgf ctxt = Right ctxt

compute :: PGF -> Expr -> Expr
compute = error "TODO: compute"

concreteName :: Concr -> ConcName
concreteName c =
  unsafePerformIO $
  withForeignPtr (c_revision c) $ \c_revision ->
  bracket (withPgfExn "concreteName" (pgf_concrete_name (c_db c) c_revision)) free $ \c_text ->
    peekText c_text

languageCode :: Concr -> Maybe String
languageCode c =
  unsafePerformIO $
  withForeignPtr (c_revision c) $ \c_revision ->
  bracket (withPgfExn "languageCode" (pgf_concrete_language_code (c_db c) c_revision)) free $ \c_text ->
    if c_text == nullPtr
      then return Nothing
      else fmap Just (peekText c_text)

concreteFlag :: Concr -> String -> Maybe Literal
concreteFlag c name =
  unsafePerformIO $
  withText name $ \c_name ->
  withForeignPtr (c_revision c) $ \c_revision ->
  withForeignPtr unmarshaller $ \u -> do
    c_lit <- withPgfExn "concreteFlag" (pgf_get_concrete_flag (c_db c) c_revision c_name u)
    if c_lit == castPtrToStablePtr nullPtr
      then return Nothing
      else do lit <- deRefStablePtr c_lit
              freeStablePtr c_lit
              return (Just lit)

printName :: Concr -> Fun -> Maybe String
printName c fun =
  unsafePerformIO $
  withText fun $ \c_fun ->
  withForeignPtr (c_revision c) $ \c_revision ->
  bracket (withPgfExn "printName" (pgf_get_printname (c_db c) c_revision c_fun)) free $ \c_name -> do
    if c_name /= nullPtr
      then fmap Just $ peekText c_name
      else return Nothing

alignWords :: Concr -> Expr -> [(String, [Int])]
alignWords c e = unsafePerformIO $
  withForeignPtr (c_revision c) $ \c_revision ->
  bracket (newStablePtr e) freeStablePtr $ \c_e ->
  withForeignPtr marshaller $ \m ->
  alloca $ \p_n_phrases -> do
    c_phrases <-  withPgfExn "alignWords" (pgf_align_words (c_db c) c_revision c_e nullPtr m p_n_phrases)
    n_phrases <- peek p_n_phrases
    arr <- peekArray (fromIntegral n_phrases) c_phrases
    free c_phrases
    mapM peekAlignmentPhrase arr
  where
    peekAlignmentPhrase :: Ptr PgfAlignmentPhrase -> IO (String, [Int])
    peekAlignmentPhrase ptr = do
      c_phrase <- (#peek PgfAlignmentPhrase, phrase) ptr
      phrase <- peekText c_phrase
      n_fids <- (#peek PgfAlignmentPhrase, n_fids) ptr
      (fids :: [CInt]) <- peekArray (fromIntegral (n_fids :: CInt)) (ptr `plusPtr` (#offset PgfAlignmentPhrase, fids))
      free c_phrase
      free ptr
      return (phrase, map fromIntegral fids)

gizaAlignment = error "TODO: gizaAlignment"

-----------------------------------------------------------------------------
-- Functions using Concr
-- Morpho analyses, parsing & linearization

-- | This triple is returned by all functions that deal with
-- the grammar's lexicon. Its first element is the name of an abstract
-- lexical function which can produce a given word or
-- a multiword expression (i.e. this is the lemma).
-- After that follows a string which describes
-- the particular inflection form.
--
-- The last element is a logarithm from the
-- the probability of the function. The probability is not
-- conditionalized on the category of the function. This makes it
-- possible to compare the likelihood of two functions even if they
-- have different types.
type MorphoAnalysis = (Fun,String,Float)

-- | 'lookupMorpho' takes a string which must be a single word or
-- a multiword expression. It then computes the list of all possible
-- morphological analyses.
lookupMorpho :: Concr -> String -> [MorphoAnalysis]
lookupMorpho c sent = unsafePerformIO $ do
  ref <- newIORef []
  (withText sent $ \c_sent ->
   allocaBytes (#size PgfMorphoCallback) $ \itor ->
   bracket (wrapMorphoCallback (getMorphology ref)) freeHaskellFunPtr $ \fptr ->
   withForeignPtr (c_revision c) $ \c_revision -> do
     (#poke PgfMorphoCallback, fn) itor fptr
     withPgfExn "lookupMorpho" (pgf_lookup_morpho (c_db c) c_revision c_sent itor))
  fmap reverse (readIORef ref)

-- | 'lookupCohorts' takes an arbitrary string an produces
-- a list of all places where lexical items from the grammar have been
-- identified (i.e. cohorts). The list consists of triples of the format @(start,ans,end)@,
-- where @start-end@ identifies the span in the text and @ans@ is
-- the list of possible morphological analyses similar to 'lookupMorpho'.
--
-- The list is sorted first by the @start@ position and after than
-- by the @end@ position. This can be used for instance if you want to
-- filter only the longest matches.
lookupCohorts :: Concr -> String -> [(Int,String,[MorphoAnalysis],Int)]
lookupCohorts c sent = unsafePerformIO $ do
  morpho_ref  <- newIORef []
  cohorts_ref <- newIORef []
  (withText sent $ \c_sent ->
   allocaBytes (#size PgfCohortsCallback) $ \itor ->
   bracket (wrapMorphoCallback (getMorphology morpho_ref)) freeHaskellFunPtr $ \morpho_fptr ->
   bracket (wrapCohortsCallback (getCohorts morpho_ref cohorts_ref)) freeHaskellFunPtr $ \cohorts_fptr ->
   withForeignPtr (c_revision c) $ \c_revision -> do
     (#poke PgfCohortsCallback, morpho.fn) itor morpho_fptr
     (#poke PgfCohortsCallback, fn) itor cohorts_fptr
     withPgfExn "lookupCohorts" (pgf_lookup_cohorts (c_db c) c_revision c_sent itor))
  fmap reverse (readIORef cohorts_ref)
  where
    getCohorts morpho_ref cohorts_ref _ start' end' exn = do
      ans <- readIORef morpho_ref
      let start = fromIntegral start'
          end   = fromIntegral end'
          word  = take (end-start) (drop start sent)
      modifyIORef cohorts_ref ((:) (start, word, reverse ans, end))
      writeIORef morpho_ref []

getMorphology ref _ c_name c_field c_prob exn = do
  name  <- peekText c_name
  field <- peekText c_field
  let prob = realToFrac c_prob
      ann = (name,field,prob)
  modifyIORef ref ((:) ann)

filterBest :: [(Int,String,[MorphoAnalysis],Int)] -> [(Int,String,[MorphoAnalysis],Int)]
filterBest ans =
  reverse (iterate (maxBound :: Int) [(0,0,[],ans)] [] [])
  where
    iterate v0 []                      []  res = res
    iterate v0 []                      new res = iterate v0 new []  res
    iterate v0 ((_,v,conf,    []):old) new res =
      case compare v0 v of
        LT                                    -> res
        EQ                                    -> iterate v0 old new (merge conf res)
        GT                                    -> iterate v  old new conf
    iterate v0 ((_,v,conf,an:ans):old) new res = iterate v0 old (insert (v+valueOf an) conf an ans [] new) res

    valueOf (_,_,[],_) = 2
    valueOf _          = 1

    insert v conf an@(start,_,_,end) ans l_new []                               =
      match start v conf ans ((end,v,comb conf an,filter end ans):l_new) []
    insert v conf an@(start,_,_,end) ans l_new (new@(end0,v0,conf0,ans0):r_new) =
      case compare end0 end of
        LT         -> insert v conf an ans (new:l_new) r_new
        EQ -> case compare v0 v of
                LT -> match start v conf ans ((end,v,                     conf0,ans0):    l_new) r_new
                EQ -> match start v conf ans ((end,v,merge (comb conf an) conf0,ans0):    l_new) r_new
                GT -> match start v conf ans ((end,v,comb conf an,              ans0):    l_new) r_new
        GT         -> match start v conf ans ((end,v,comb conf an,    filter end ans):new:l_new) r_new

    match start0 v conf (an@(start,_,_,end):ans) l_new r_new
      | start0 == start                 = insert v conf an ans l_new r_new
    match start0 v conf ans l_new r_new = revOn l_new r_new

    comb ((start0,w0,an0,end0):conf) (start,w,an,end)
      | end0 == start && (unk w0 an0 || unk w an) = (start0,w0++w,[],end):conf
    comb conf an = an:conf

    filter end                     [] = []
    filter end (next@(start,_,_,_):ans)
      | end <= start                  = next:ans
      | otherwise                     = filter end ans

    revOn []     ys = ys
    revOn (x:xs) ys = revOn xs (x:ys)

    merge []                           ans                          = ans
    merge ans                          []                           = ans
    merge (an1@(start1,_,_,end1):ans1) (an2@(start2,_,_,end2):ans2) =
      case compare (start1,end1) (start2,end2) of
        GT -> an1 : merge ans1 (an2:ans2)
        EQ -> an1 : merge ans1 ans2
        LT -> an2 : merge (an1:ans1) ans2

filterLongest :: [(Int,String,[MorphoAnalysis],Int)] -> [(Int,String,[MorphoAnalysis],Int)]
filterLongest []       = []
filterLongest (an:ans) = longest an ans
  where
    longest prev                   [] = [prev]
    longest prev@(start0,_,_,end0) (next@(start,_,_,end):ans)
      | start0 == start               = longest next ans
      | otherwise                     = filter prev (next:ans)

    filter prev                      [] = [prev]
    filter prev@(start0,w0,an0,end0) (next@(start,w,an,end):ans)
      | end0 == start && (unk w0 an0 || unk w an)
                                        = filter (start0,w0++w,[],end) ans
      | end0 <=  start                  = prev : longest next ans
      | otherwise                       = filter prev ans

unk w [] | any (not . isPunctuation) w = True
unk _ _                                = False

fullFormLexicon :: Concr -> [(String, [MorphoAnalysis])]
fullFormLexicon c = unsafePerformIO $ do
  ref <- newIORef []
  (allocaBytes (#size PgfSequenceItor) $ \itor1 ->
   bracket (wrapSequenceItorCallback (getSequences ref)) freeHaskellFunPtr $ \fptr1 ->
   allocaBytes (#size PgfMorphoCallback) $ \itor2 ->
   bracket (wrapMorphoCallback (getMorphology ref)) freeHaskellFunPtr $ \fptr2 ->
   withForeignPtr (c_revision c) $ \c_revision -> do
     (#poke PgfSequenceItor,   fn) itor1 fptr1
     (#poke PgfMorphoCallback, fn) itor2 fptr2
     seq_ids <- withPgfExn "fullFormLexicon" (pgf_iter_sequences (c_db c) c_revision itor1 itor2)
     pgf_release_phrasetable_ids seq_ids)
  fmap (reverse2 []) (readIORef ref)
  where
    getSequences ref _ seq_id val exn = do
      bracket (pgf_sequence_get_text_internal val) free $ \c_text ->
        if c_text == nullPtr
          then return 1
          else do form <- peekText c_text
                  case form of
                    [] -> return 1
                    _  -> do modifyIORef ref $ (\lexicon -> (form, []) : lexicon)
                             return 0

    getMorphology ref _ c_name c_field c_prob exn = do
      name  <- peekText c_name
      field <- peekText c_field
      let prob = realToFrac c_prob
          ann = (name,field,prob)
      modifyIORef ref (\((form,anns) : lexicon) -> (form,ann:anns) : lexicon)

    reverse2 ys []           = ys
    reverse2 ys ((x1,x2):xs) = reverse2 ((x1,reverse x2):ys) xs


-- | This data type encodes the different outcomes which you could get from the parser.
data ParseOutput a
  = ParseFailed Int String         -- ^ The integer is the position in number of unicode characters where the parser failed.
                                   -- The string is the token where the parser have failed.
  | ParseOk a                      -- ^ If the parsing and the type checking are successful
                                   -- we get the abstract syntax trees as either a list or a chart.
  | ParseIncomplete                -- ^ The sentence is not complete.

parse :: Concr -> Type -> String -> ParseOutput [(Expr,Float)]
parse c ty sent =
  unsafePerformIO $
  withForeignPtr (c_revision c) $ \c_revision ->
  withForeignPtr marshaller $ \m ->
  bracket (newStablePtr ty) freeStablePtr $ \c_ty ->
  withText sent $ \c_sent -> do
    c_enum <- withPgfExn "parse" (pgf_parse (c_db c) c_revision c_ty m c_sent)
    c_fetch <- (#peek PgfExprEnumVtbl, fetch) =<< (#peek PgfExprEnum, vtbl) c_enum
    exprs <- unsafeInterleaveIO (fetchLazy c_fetch c_enum)
    return (ParseOk exprs)
  where
    fetchLazy c_fetch c_enum =
      withForeignPtr (c_revision c) $ \c_revision ->
      withForeignPtr unmarshaller $ \u -> 
      alloca $ \p_prob -> do
        c_expr <- callFetch c_fetch c_enum (c_db c) u p_prob
        if c_expr == castPtrToStablePtr nullPtr
          then do pgf_free_expr_enum c_enum
                  return []
          else do expr <- deRefStablePtr c_expr
                  freeStablePtr c_expr
                  prob <- peek p_prob
                  rest <- unsafeInterleaveIO (fetchLazy c_fetch c_enum)
                  return ((expr,prob) : rest)

parseWithHeuristics :: Concr      -- ^ the language with which we parse
                    -> Type       -- ^ the start category
                    -> String     -- ^ the input sentence
                    -> Double     -- ^ the heuristic factor.
                                  -- A negative value tells the parser
                                  -- to lookup up the default from
                                  -- the grammar flags
                    -> [(Cat, String -> Int -> Maybe (Expr,Float,Int))]
                                  -- ^ a list of callbacks for literal categories.
                                  -- The arguments of the callback are:
                                  -- the index of the constituent for the literal category;
                                  -- the input sentence; the current offset in the sentence.
                                  -- If a literal has been recognized then the output should
                                  -- be Just (expr,probability,end_offset)
                    -> ParseOutput [(Expr,Float)]
parseWithHeuristics = error "TODO: parseWithHeuristics"

-- | Returns possible completions of the current partial input.
complete :: Concr      -- ^ the language with which we parse
         -> Type       -- ^ the start category
         -> String     -- ^ the input sentence (excluding token being completed)
         -> String     -- ^ prefix (partial token being completed)
         -> ParseOutput [(String, Fun, Cat, Float)]  -- ^ (token, category, function, probability)
complete = error "TODO: complete"

-- | Returns True if there is a linearization defined for that function in that language
hasLinearization :: Concr -> Fun -> Bool
hasLinearization c name =
  unsafePerformIO $
  withText name $ \c_name ->
  withForeignPtr (c_revision c) $ \c_revision -> do
    c_res <- withPgfExn "hasLinearization" (pgf_has_linearization (c_db c) c_revision c_name)
    return (c_res /= 0)

categoryFields :: Concr -> Cat -> Maybe [String]
categoryFields c cat =
  unsafePerformIO $
  withForeignPtr (c_revision c) $ \c_revision ->
  withText cat $ \c_cat ->
  alloca $ \p_n_fields ->
  bracket (withPgfExn "categoryFields" (pgf_category_fields (c_db c) c_revision c_cat p_n_fields)) free $ \c_fields ->
    if c_fields == nullPtr
      then return Nothing
      else do n_fields <- peek p_n_fields
              fs <- peekFields n_fields c_fields
              return (Just fs)
  where
    peekFields n_fields c_fields
      | n_fields == 0 = return []
      | otherwise     = do c_text <- peek c_fields
                           f  <- peekText c_text
                           free c_text
                           fs <- peekFields (n_fields-1) (c_fields `plusPtr` (#size PgfText*))
                           return (f:fs)

-- | Linearizes an expression as a string in the language
linearize :: Concr -> Expr -> String
linearize c e =
  unsafePerformIO $
  withForeignPtr (c_revision c) $ \c_revision ->
  bracket (newStablePtr e) freeStablePtr $ \c_e ->
  withForeignPtr marshaller $ \m ->
  bracket (withPgfExn "linearize" (pgf_linearize (c_db c) c_revision c_e nullPtr m)) free $ \c_text ->
    if c_text == nullPtr
      then return ""
      else peekText c_text

-- | Generates all possible linearizations of an expression
linearizeAll :: Concr -> Expr -> [String]
linearizeAll c e =
  unsafePerformIO $
  withForeignPtr (c_revision c) $ \c_revision ->
  bracket (newStablePtr e) freeStablePtr $ \c_e ->
  withForeignPtr marshaller $ \m ->
  alloca $ \p_n_fields ->
  bracket (withPgfExn "linearizeAll" (pgf_linearize_all (c_db c) c_revision c_e nullPtr m p_n_fields)) free $ \c_texts -> do
    n_fields <- peek p_n_fields
    peekTexts n_fields c_texts
  where
    peekTexts 0 c_texts = return []
    peekTexts n c_texts = do
      c_text <- peek c_texts
      text <- peekText c_text
      free c_text
      texts <- peekTexts (n-1) (c_texts `plusPtr` (#size PgfText*))
      return (text:texts)

-- | Generates a table of linearizations for an expression
tabularLinearize :: Concr -> Expr -> [(String, String)]
tabularLinearize c e =
  unsafePerformIO $
  withForeignPtr (c_revision c) $ \c_revision ->
  bracket (newStablePtr e) freeStablePtr $ \c_e ->
  withForeignPtr marshaller $ \m ->
  bracket (withPgfExn "tabularLinearize" (pgf_tabular_linearize (c_db c) c_revision c_e nullPtr m)) free $ \c_texts -> do
    if c_texts == nullPtr
      then return []
      else peekTable c_texts
  where
    peekTable c_texts = do
      c_field <- peekElemOff c_texts 0
      c_lin   <- peekElemOff c_texts 1
      if c_field == nullPtr && c_lin == nullPtr
        then return []
        else do field <- peekText c_field
                free c_field
                lin   <- peekText c_lin
                free c_lin
                table <- peekTable (c_texts `plusPtr` (2*(#size PgfText*)))
                return ((field,lin):table)

-- | Generates a table of linearizations for an expression
tabularLinearizeAll :: Concr -> Expr -> [[(String, String)]]
tabularLinearizeAll c e =
  unsafePerformIO $
  withForeignPtr (c_revision c) $ \c_revision ->
  bracket (newStablePtr e) freeStablePtr $ \c_e ->
  withForeignPtr marshaller $ \m ->
  bracket (withPgfExn "tabularLinearizeAll" (pgf_tabular_linearize_all (c_db c) c_revision c_e nullPtr m)) free peekTables
  where
    peekTables c_texts = do
      c_field <- peekElemOff c_texts 0
      c_lin   <- peekElemOff c_texts 1
      if c_field == nullPtr && c_lin == nullPtr
        then return [[]]
        else if c_field == nullPtr
               then do tables <- peekTables (c_texts `plusPtr` (#size PgfText*))
                       return ([]:tables)
               else do field <- peekText c_field
                       free c_field
                       lin   <- peekText c_lin
                       free c_lin
                       (table:tables) <- peekTables (c_texts `plusPtr` (2*(#size PgfText*)))
                       return (((field,lin):table):tables)

type FId = Int

-- | BracketedString represents a sentence that is linearized
-- as usual but we also want to retain the ''brackets'' that
-- mark the beginning and the end of each constituent.
data BracketedString
  = Leaf String                                                                -- ^ this is the leaf i.e. a single token
  | BIND                                                                       -- ^ the surrounding tokens must be bound together
  | Bracket Cat {-# UNPACK #-} !FId String Fun [BracketedString]
                                                                               -- ^ this is a bracket. The 'Cat' is the category of
                                                                               -- the phrase. The 'FId' is an unique identifier for
                                                                               -- every phrase in the sentence. For context-free grammars
                                                                               -- i.e. without discontinuous constituents this identifier
                                                                               -- is also unique for every bracket. When there are discontinuous
                                                                               -- phrases then the identifiers are unique for every phrase but
                                                                               -- not for every bracket since the bracket represents a constituent.
                                                                               -- The different constituents could still be distinguished by using
                                                                               -- the analysis string. If the grammar is reduplicating
                                                                               -- then the constituent indices will be the same for all brackets
                                                                               -- that represents the same constituent.
                                                                               -- The 'Fun' is the name of the abstract function that generated
                                                                               -- this phrase.

-- | Renders the bracketed string as a string where
-- the brackets are shown as @(S ...)@ where
-- @S@ is the category.
showBracketedString :: BracketedString -> String
showBracketedString = render . ppBracketedString

ppBracketedString (Leaf t) = text t
ppBracketedString BIND     = text "&+"
ppBracketedString (Bracket cat fid _ _ bss) = parens (text cat <> colon <> int fid <+> hsep (map ppBracketedString bss))

-- | Extracts the sequence of tokens from the bracketed string
flattenBracketedString :: BracketedString -> [String]
flattenBracketedString (Leaf w)              = [w]
flattenBracketedString (Bracket _ _ _ _ bss) = concatMap flattenBracketedString bss

bracketedLinearize :: Concr -> Expr -> [BracketedString]
bracketedLinearize c e = unsafePerformIO $ do
  ref <- newIORef (False,[],[])
  (withForeignPtr (c_revision c) $ \c_revision ->
   bracket (newStablePtr e) freeStablePtr $ \c_e ->
   withForeignPtr marshaller $ \m ->
   allocaBytes (#size PgfLinearizationOutputIface) $ \c_out ->
   allocaBytes (#size PgfLinearizationOutputIfaceVtbl) $ \vtbl ->
   bracket (wrapSymbol1 (symbol_token ref)) freeHaskellFunPtr $ \c_symbol_token ->
   bracket (wrapSymbol2 (begin_phrase ref)) freeHaskellFunPtr $ \c_begin_phrase ->
   bracket (wrapSymbol2 (end_phrase ref)) freeHaskellFunPtr $ \c_end_phrase ->
   bracket (wrapSymbol0 (symbol_bind ref)) freeHaskellFunPtr $ \c_symbol_bind ->
   bracket (wrapSymbol0 (symbol_ne ref)) freeHaskellFunPtr $ \c_symbol_ne -> do
   bracket (wrapSymbol0 (flush ref)) freeHaskellFunPtr $ \c_flush -> do
     (#poke PgfLinearizationOutputIfaceVtbl, symbol_token) vtbl c_symbol_token
     (#poke PgfLinearizationOutputIfaceVtbl, begin_phrase) vtbl c_begin_phrase
     (#poke PgfLinearizationOutputIfaceVtbl, end_phrase) vtbl c_end_phrase
     (#poke PgfLinearizationOutputIfaceVtbl, symbol_bind) vtbl c_symbol_bind
     (#poke PgfLinearizationOutputIfaceVtbl, symbol_ne) vtbl c_symbol_ne
     (#poke PgfLinearizationOutputIfaceVtbl, flush) vtbl c_flush
     (#poke PgfLinearizationOutputIface, vtbl) c_out vtbl
     withPgfExn "bracketedLinearize" (pgf_bracketed_linearize (c_db c) c_revision c_e nullPtr m c_out))
  (ne,_,bs) <- readIORef ref
  (if ne
     then return []
     else return (reverse bs))
  where
    symbol_token ref _ c_text = do
      (ne,stack,bs) <- readIORef ref
      token <- peekText c_text
      writeIORef ref (ne,stack,Leaf token : bs)

    begin_phrase ref _ c_cat c_fid c_ann c_fun = do
      (ne,stack,bs) <- readIORef ref
      writeIORef ref (ne,bs:stack,[])

    end_phrase ref _ c_cat c_fid c_ann c_fun = do
      (ne,bs':stack,bs) <- readIORef ref
      if null bs
        then writeIORef ref (ne,stack, bs')
        else do cat <- peekText c_cat
                let fid = fromIntegral c_fid
                ann <- peekText c_ann
                fun <- peekText c_fun
                writeIORef ref (ne,stack,Bracket cat fid ann fun (reverse bs) : bs')

    symbol_bind ref _ = do
      (ne,stack,bs) <- readIORef ref
      writeIORef ref (ne,stack,BIND : bs)

    symbol_ne ref _ = do
      (ne,stack,bs) <- readIORef ref
      writeIORef ref (True,stack,bs)

    flush _ _ = return ()

bracketedLinearizeAll :: Concr -> Expr -> [[BracketedString]]
bracketedLinearizeAll c e = unsafePerformIO $ do
  ref <- newIORef (False,[],[],[])
  (withForeignPtr (c_revision c) $ \c_revision ->
   bracket (newStablePtr e) freeStablePtr $ \c_e ->
   withForeignPtr marshaller $ \m ->
   allocaBytes (#size PgfLinearizationOutputIface) $ \c_out ->
   allocaBytes (#size PgfLinearizationOutputIfaceVtbl) $ \vtbl ->
   bracket (wrapSymbol1 (symbol_token ref)) freeHaskellFunPtr $ \c_symbol_token ->
   bracket (wrapSymbol2 (begin_phrase ref)) freeHaskellFunPtr $ \c_begin_phrase ->
   bracket (wrapSymbol2 (end_phrase ref)) freeHaskellFunPtr $ \c_end_phrase ->
   bracket (wrapSymbol0 (symbol_bind ref)) freeHaskellFunPtr $ \c_symbol_bind ->
   bracket (wrapSymbol0 (symbol_ne ref)) freeHaskellFunPtr $ \c_symbol_ne -> do
   bracket (wrapSymbol0 (flush ref)) freeHaskellFunPtr $ \c_flush -> do
     (#poke PgfLinearizationOutputIfaceVtbl, symbol_token) vtbl c_symbol_token
     (#poke PgfLinearizationOutputIfaceVtbl, begin_phrase) vtbl c_begin_phrase
     (#poke PgfLinearizationOutputIfaceVtbl, end_phrase) vtbl c_end_phrase
     (#poke PgfLinearizationOutputIfaceVtbl, symbol_bind) vtbl c_symbol_bind
     (#poke PgfLinearizationOutputIfaceVtbl, symbol_ne) vtbl c_symbol_ne
     (#poke PgfLinearizationOutputIfaceVtbl, flush) vtbl c_flush
     (#poke PgfLinearizationOutputIface, vtbl) c_out vtbl
     withPgfExn "bracketedLinearizeAll" (pgf_bracketed_linearize_all (c_db c) c_revision c_e nullPtr m c_out))
  (_,_,_,all) <- readIORef ref
  return all
  where
    symbol_token ref _ c_text = do
      (ne,stack,bs,all) <- readIORef ref
      token <- peekText c_text
      writeIORef ref (ne,stack,Leaf token : bs,all)

    begin_phrase ref _ c_cat c_fid c_ann c_fun = do
      (ne,stack,bs,all) <- readIORef ref
      writeIORef ref (ne,bs:stack,[],all)

    end_phrase ref _ c_cat c_fid c_ann c_fun = do
      (ne,bs':stack,bs,all) <- readIORef ref
      if null bs
        then writeIORef ref (ne,stack,bs',all)
        else do cat <- peekText c_cat
                let fid = fromIntegral c_fid
                ann <- peekText c_ann
                fun <- peekText c_fun
                writeIORef ref (ne,stack,Bracket cat fid ann fun (reverse bs) : bs',all)

    symbol_bind ref _ = do
      (ne,stack,bs,all) <- readIORef ref
      writeIORef ref (ne,stack,BIND : bs,all)

    symbol_ne ref _ = do
      (ne,stack,bs,all) <- readIORef ref
      writeIORef ref (True,[],[],all)

    flush ref _ = do
      (ne,_,bs,all) <- readIORef ref
      if ne
        then writeIORef ref (False,[],[],all)
        else writeIORef ref (False,[],[],reverse bs:all)

generateAll :: PGF -> Type -> [(Expr,Float)]
generateAll p ty = error "TODO: generateAll"

generateAllFrom :: PGF -> Expr -> [(Expr,Float)]
generateAllFrom p ty = error "TODO: generateAllFrom"

generateRandom :: StdGen -> PGF -> Type -> [a]
generateRandom = error "TODO: generateRandom"

generateRandomFrom :: StdGen -> PGF -> Expr -> [a]
generateRandomFrom = error "TODO: generateRandomFrom"

-- | List of all functions defined in the abstract syntax
categories :: PGF -> [Cat]
categories p =
  unsafePerformIO $ do
    ref <- newIORef []
    (allocaBytes (#size PgfItor) $ \itor ->
     bracket (wrapItorCallback (getCategories ref)) freeHaskellFunPtr $ \fptr ->
     withForeignPtr (a_revision p) $ \c_revision -> do
      (#poke PgfItor, fn) itor fptr
      withPgfExn "categories" (pgf_iter_categories (a_db p) c_revision itor)
      cs <- readIORef ref
      return (reverse cs))
  where
    getCategories :: IORef [String] -> ItorCallback
    getCategories ref itor key _ exn = do
      names <- readIORef ref
      name  <- peekText key
      writeIORef ref $ (name : names)

categoryContext :: PGF -> Cat -> Maybe [Hypo]
categoryContext p cat =
  unsafePerformIO $
  withText cat $ \c_cat ->
  alloca $ \p_n_hypos ->
  withForeignPtr unmarshaller $ \u ->
  withForeignPtr (a_revision p) $ \c_revision ->
  mask_ $ do
    c_hypos <- withPgfExn "categoryContext" (pgf_category_context (a_db p) c_revision c_cat p_n_hypos u)
    if c_hypos == nullPtr
      then return Nothing
      else do n_hypos <- peek p_n_hypos
              hypos <- peekHypos c_hypos 0 n_hypos
              free c_hypos
              return (Just hypos)
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

categoryProbability :: PGF -> Cat -> Float
categoryProbability p cat =
  unsafePerformIO $
  withText cat $ \c_cat ->
  withForeignPtr (a_revision p) $ \c_revision ->
      withPgfExn "categoryProbability" (pgf_category_prob (a_db p) c_revision c_cat)

-- | List of all functions defined in the abstract syntax
functions :: PGF -> [Fun]
functions p =
  unsafePerformIO $ do
    ref <- newIORef []
    (allocaBytes (#size PgfItor) $ \itor ->
     bracket (wrapItorCallback (getFunctions ref)) freeHaskellFunPtr $ \fptr ->
     withForeignPtr (a_revision p) $ \c_revision -> do
      (#poke PgfItor, fn) itor fptr
      withPgfExn "functions" (pgf_iter_functions (a_db p) c_revision itor)
      fs <- readIORef ref
      return (reverse fs))
  where
    getFunctions :: IORef [String] -> ItorCallback
    getFunctions ref itor key _ exn = do
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
     withForeignPtr (a_revision p) $ \c_revision -> do
      (#poke PgfItor, fn) itor fptr
      withPgfExn "functionsByCat" (pgf_iter_functions_by_cat (a_db p) c_revision c_cat itor)
      fs <- readIORef ref
      return (reverse fs))
  where
    getFunctions :: IORef [String] -> ItorCallback
    getFunctions ref itor key _ exn = do
      names <- readIORef ref
      name  <- peekText key
      writeIORef ref $ (name : names)

globalFlag :: PGF -> String -> Maybe Literal
globalFlag p name =
  unsafePerformIO $
  withText name $ \c_name ->
  withForeignPtr (a_revision p) $ \c_revision ->
  withForeignPtr unmarshaller $ \u -> do
    c_lit <- withPgfExn "globalFlag" (pgf_get_global_flag (a_db p) c_revision c_name u)
    if c_lit == castPtrToStablePtr nullPtr
      then return Nothing
      else do lit <- deRefStablePtr c_lit
              freeStablePtr c_lit
              return (Just lit)

abstractFlag :: PGF -> String -> Maybe Literal
abstractFlag p name =
  unsafePerformIO $
  withText name $ \c_name ->
  withForeignPtr (a_revision p) $ \c_revision ->
  withForeignPtr unmarshaller $ \u -> do
    c_lit <- withPgfExn "abstractFlag" (pgf_get_abstract_flag (a_db p) c_revision c_name u)
    if c_lit == castPtrToStablePtr nullPtr
      then return Nothing
      else do lit <- deRefStablePtr c_lit
              freeStablePtr c_lit
              return (Just lit)

-----------------------------------------------------------------------------
-- Graphviz

data GraphvizOptions = GraphvizOptions {noLeaves :: Bool,
                                        noFun :: Bool,
                                        noCat :: Bool,
                                        noDep :: Bool,
                                        nodeFont :: String,
                                        leafFont :: String,
                                        nodeColor :: String,
                                        leafColor :: String,
                                        nodeEdgeStyle :: String,
                                        leafEdgeStyle :: String
                                       }

graphvizDefaults = GraphvizOptions False False False True "" "" "" "" "" ""

withGraphvizOptions :: GraphvizOptions -> (Ptr PgfGraphvizOptions -> IO a) -> IO a
withGraphvizOptions opts f =
  allocaBytes (#size PgfGraphvizOptions) $ \c_opts ->
  withCString (nodeFont opts) $ \c_nodeFont ->
  withCString (leafFont opts) $ \c_leafFont ->
  withCString (nodeColor opts) $ \c_nodeColor ->
  withCString (leafColor opts) $ \c_leafColor ->
  withCString (nodeEdgeStyle opts) $ \c_nodeEdgeStyle ->
  withCString (leafEdgeStyle opts) $ \c_leafEdgeStyle -> do
    (#poke PgfGraphvizOptions, noLeaves) c_opts (if noLeaves opts then 1 else 0 :: CInt)
    (#poke PgfGraphvizOptions, noFun)    c_opts (if noFun    opts then 1 else 0 :: CInt)
    (#poke PgfGraphvizOptions, noCat)    c_opts (if noCat    opts then 1 else 0 :: CInt)
    (#poke PgfGraphvizOptions, noDep)    c_opts (if noDep    opts then 1 else 0 :: CInt)
    (#poke PgfGraphvizOptions, nodeFont) c_opts c_nodeFont
    (#poke PgfGraphvizOptions, leafFont) c_opts c_leafFont
    (#poke PgfGraphvizOptions, nodeColor) c_opts c_nodeColor
    (#poke PgfGraphvizOptions, leafColor) c_opts c_leafColor
    (#poke PgfGraphvizOptions, nodeEdgeStyle) c_opts c_nodeEdgeStyle
    (#poke PgfGraphvizOptions, leafEdgeStyle) c_opts c_leafEdgeStyle
    f c_opts

-- | Renders an abstract syntax tree in a Graphviz format.
graphvizAbstractTree :: PGF -> GraphvizOptions -> Expr -> String
graphvizAbstractTree p opts e =
  unsafePerformIO $
  withForeignPtr (a_revision p) $ \c_revision ->
  bracket (newStablePtr e) freeStablePtr $ \c_e ->
  withForeignPtr marshaller $ \m ->
  withGraphvizOptions opts $ \c_opts ->
  bracket (withPgfExn "graphvizAbstractTree" (pgf_graphviz_abstract_tree (a_db p) c_revision c_e m c_opts)) free $ \c_text ->
    peekText c_text

graphvizParseTree :: Concr -> GraphvizOptions -> Expr -> String
graphvizParseTree c opts e =
  unsafePerformIO $
  withForeignPtr (c_revision c) $ \c_revision ->
  bracket (newStablePtr e) freeStablePtr $ \c_e ->
  withForeignPtr marshaller $ \m ->
  withGraphvizOptions opts $ \c_opts ->
  bracket (withPgfExn "graphvizParseTree" (pgf_graphviz_parse_tree  (c_db c) c_revision c_e nullPtr m c_opts)) free $ \c_text ->
    if c_text == nullPtr
      then return ""
      else peekText c_text

graphvizWordAlignment :: [Concr] -> GraphvizOptions -> Expr -> String
graphvizWordAlignment [] opts e = ""
graphvizWordAlignment cs opts e =
  unsafePerformIO $
  withPgfConcrs cs $ \c_db c_revisions n_revisions ->
  bracket (newStablePtr e) freeStablePtr $ \c_e ->
  withForeignPtr marshaller $ \m ->
  withGraphvizOptions opts $ \c_opts ->
  bracket (withPgfExn "graphvizWordAlignment" (pgf_graphviz_word_alignment c_db c_revisions n_revisions c_e nullPtr m c_opts)) free $ \c_text ->
    if c_text == nullPtr
      then return ""
      else peekText c_text
  where
    withPgfConcrs cs f =
      allocaArray len $ \array ->
        pokeAll array nullPtr array cs
      where
        len = length cs

        pokeAll ptr c_db0 array []     = f c_db0 array (fromIntegral len)
        pokeAll ptr c_db0 array (c:cs)
          | c_db0 /= nullPtr && c_db0 /= c_db c =
                throwIO (PGFError "graphvizWordAlignment" "The concrete languages must be from the same grammar")
          | otherwise =
              withForeignPtr (c_revision c) $ \c_revision -> do
                poke ptr c_revision
                pokeAll (ptr `plusPtr` (#size PgfConcrRevision)) (c_db c) array cs


type Labels = Map.Map Fun [String]

getDepLabels :: String -> Labels
getDepLabels s = Map.fromList [(f,ls) | f:ls <- map words (lines s)]

-- | Visualize word dependency tree.
graphvizDependencyTree
  :: String -- ^ Output format: @"latex"@, @"conll"@, @"malt_tab"@, @"malt_input"@ or @"dot"@
  -> Bool -- ^ Include extra information (debug)
  -> Maybe Labels -- ^ abstract label information obtained with 'getDepLabels'
  -> Maybe CncLabels -- ^ concrete label information obtained with ' ' (was: unused (was: @Maybe String@))
  -> Concr
  -> Expr
  -> String -- ^ Rendered output in the specified format
graphvizDependencyTree format debug mlab mclab concr t = error "TODO: graphvizDependencyTree"

---------------------- should be a separate module?

-- visualization with latex output. AR Nov 2015

conlls2latexDoc :: [String] -> String
conlls2latexDoc =
  render .
  latexDoc .
  vcat .
  intersperse (text "" $+$ app "vspace" (text "4mm")) .
  map conll2latex .
  filter (not . null)

conll2latex :: String -> Doc
conll2latex = ppLaTeX . conll2latex' . parseCoNLL

conll2latex' :: CoNLL -> [LaTeX]
conll2latex' = dep2latex . conll2dep'

data Dep = Dep {
    wordLength  :: Int -> Double        -- length of word at position int       -- was: fixed width, millimetres (>= 20.0)
  , tokens      :: [(String,String)]    -- word, pos (0..)
  , deps        :: [((Int,Int),String)] -- from, to, label
  , root        :: Int                  -- root word position
  }

-- some general measures
defaultWordLength = 20.0  -- the default fixed width word length, making word 100 units
defaultUnit       = 0.2   -- unit in latex pictures, 0.2 millimetres
spaceLength       = 10.0
charWidth = 1.8

wsize rwld  w  = 100 * rwld w + spaceLength                   -- word length, units
wpos rwld i    = sum [wsize rwld j | j <- [0..i-1]]           -- start position of the i'th word
wdist rwld x y = sum [wsize rwld i | i <- [min x y .. max x y - 1]]    -- distance between words x and y
labelheight h  = h + arcbase + 3    -- label just above arc; 25 would put it just below
labelstart c   = c - 15.0           -- label starts 15u left of arc centre
arcbase        = 30.0               -- arcs start and end 40u above the bottom
arcfactor r    = r * 600            -- reduction of arc size from word distance
xyratio        = 3                  -- width/height ratio of arcs

putArc :: (Int -> Double) -> Int -> Int -> Int -> String -> [DrawingCommand]
putArc frwld height x y label = [oval,arrowhead,labelling] where
  oval = Put (ctr,arcbase) (OvalTop (wdth,hght))
  arrowhead = Put (endp,arcbase + 5) (ArrowDown 5)   -- downgoing arrow 5u above the arc base
  labelling = Put (labelstart ctr,labelheight (hght/2)) (TinyText label)
  dxy  = wdist frwld x y             -- distance between words, >>= 20.0
  ndxy = 100 * rwld * fromIntegral height  -- distance that is indep of word length
  hdxy = dxy / 2                     -- half the distance
  wdth = dxy - (arcfactor rwld)/dxy  -- longer arcs are wider in proportion
  hght = ndxy / (xyratio * rwld)      -- arc height is independent of word length
  begp = min x y                     -- begin position of oval
  ctr  = wpos frwld begp + hdxy + (if x < y then 20 else  10)  -- LR arcs are farther right from center of oval
  endp = (if x < y then (+) else (-)) ctr (wdth/2)            -- the point of the arrow
  rwld = 0.5 ----

dep2latex :: Dep -> [LaTeX]
dep2latex d =
  [Comment (unwords (map fst (tokens d))),
   Picture defaultUnit (width,height) (
     [Put (wpos rwld i,0) (Text w) | (i,w) <- zip [0..] (map fst (tokens d))]   -- words
  ++ [Put (wpos rwld i,15) (TinyText w) | (i,w) <- zip [0..] (map snd (tokens d))]   -- pos tags 15u above bottom
  ++ concat [putArc rwld (aheight x y) x y label | ((x,y),label) <- deps d]    -- arcs and labels
  ++ [Put (wpos rwld (root d) + 15,height) (ArrowDown (height-arcbase))]
  ++ [Put (wpos rwld (root d) + 20,height - 10) (TinyText "ROOT")]
  )]
 where
   wld i  = wordLength d i  -- >= 20.0
   rwld i = (wld i) / defaultWordLength       -- >= 1.0
   aheight x y = depth (min x y) (max x y) + 1    ---- abs (x-y)
   arcs = [(min u v, max u v) | ((u,v),_) <- deps d]
   depth x y = case [(u,v) | (u,v) <- arcs, (x < u && v <= y) || (x == u && v < y)] of ---- only projective arcs counted
     [] -> 0
     uvs -> 1 + maximum (0:[depth u v | (u,v) <- uvs])
   width = {-round-} (sum [wsize rwld w | (w,_) <- zip [0..] (tokens d)]) + {-round-} spaceLength * fromIntegral ((length (tokens d)) - 1)
   height = 50 + 20 * {-round-} (maximum (0:[aheight x y | ((x,y),_) <- deps d]))

type CoNLL = [[String]]
parseCoNLL :: String -> CoNLL
parseCoNLL = map words . lines

--conll2dep :: String -> Dep
--conll2dep = conll2dep' . parseCoNLL

conll2dep' :: CoNLL -> Dep
conll2dep' ls = Dep {
    wordLength = wld 
  , tokens = toks
  , deps = dps
  , root = head $ [read x-1 | x:_:_:_:_:_:"0":_ <- ls] ++ [1]
  }
 where
   wld i = maximum (0:[charWidth * fromIntegral (length w) | w <- let (tok,pos) = toks !! i in [tok,pos]])
   toks = [(w,c) | _:w:_:c:_ <- ls]
   dps = [((read y-1, read x-1),lab) | x:_:_:_:_:_:y:lab:_ <- ls, y /="0"]
   --maxdist = maximum [abs (x-y) | ((x,y),_) <- dps]


-- * LaTeX Pictures (see https://en.wikibooks.org/wiki/LaTeX/Picture)

-- We render both LaTeX and SVG from this intermediate representation of
-- LaTeX pictures.

data LaTeX = Comment String | Picture UnitLengthMM Size [DrawingCommand]
data DrawingCommand = Put Position Object
data Object = Text String | TinyText String | OvalTop Size | ArrowDown Length

type UnitLengthMM = Double
type Size = (Double,Double)
type Position = (Double,Double)
type Length = Double


-- * latex formatting
ppLaTeX = vcat . map ppLaTeX1
  where
    ppLaTeX1 el =
      case el of
        Comment s -> comment s
        Picture unit size cmds ->
          app "setlength{\\unitlength}" (text (show unit ++ "mm"))
          $$ hang (app "begin" (text "picture")<>text (show size)) 2
                  (vcat (map ppDrawingCommand cmds))
          $$ app "end" (text "picture")
          $$ text ""

    ppDrawingCommand (Put pos obj) = put pos (ppObject obj)

    ppObject obj =
      case obj of
        Text s -> text s
        TinyText s -> small (text s)
        OvalTop size -> text "\\oval" <> text (show size) <> text "[t]"
        ArrowDown len -> app "vector(0,-1)" (text (show len))

    put p@(_,_) = app ("put" ++ show p)
    small w = text "{\\tiny" <+> w <> text "}"
    comment s = text "%%" <+> text s -- line break show follow
    
app macro arg = text "\\" <> text macro <> text "{" <> arg <> text "}"


latexDoc :: Doc -> Doc
latexDoc body =
  vcat [text "\\documentclass{article}",
        text "\\usepackage[utf8]{inputenc}",
        text "\\begin{document}",
        body,
        text "\\end{document}"]


----------------------------------
-- concrete syntax annotations (local) on top of conll
-- examples of annotations:
-- UseComp {"not"} PART neg head 
-- UseComp {*} AUX cop head

type CncLabels = [(String, String -> Maybe (String -> String,String,String))]
-- (fun, word -> (pos,label,target))
-- the pos can remain unchanged, as in the current notation in the article

fixCoNLL :: CncLabels -> CoNLL -> CoNLL
fixCoNLL labels conll = map fixc conll where
  fixc row = case row of
    (i:word:fun:pos:cat:x_:"0":"dep":xs) -> (i:word:fun:pos:cat:x_:"0":"root":xs) --- change the root label from dep to root 
    (i:word:fun:pos:cat:x_:j:label:xs) -> case look (fun,word) of
      Just (pos',label',"head") -> (i:word:fun:pos' pos:cat:x_:j :label':xs)
      Just (pos',label',target) -> (i:word:fun:pos' pos:cat:x_: getDep j target:label':xs)
      _ -> row
    _ -> row
    
  look (fun,word) = case lookup fun labels of
    Just relabel -> case relabel word of
      Just row -> Just row
      _ -> case lookup "*" labels of
        Just starlabel -> starlabel word
        _ -> Nothing
    _ -> case lookup "*" labels of
        Just starlabel -> starlabel word
        _ -> Nothing
  
  getDep j label = maybe j id $ lookup (label,j) [((label,j),i) | i:word:fun:pos:cat:x_:j:label:xs <- conll]

getCncDepLabels :: String -> CncLabels
getCncDepLabels = map merge .  groupBy (\ (x,_) (a,_) -> x == a) . concatMap analyse . filter choose . lines where
  --- choose is for compatibility with the general notation
  choose line = notElem '(' line && elem '{' line --- ignoring non-local (with "(") and abstract (without "{") rules
  
  analyse line = case break (=='{') line of
    (beg,_:ws) -> case break (=='}') ws of
      (toks,_:target) -> case (words beg, words target) of
        (fun:_,[    label,j]) -> [(fun, (tok, (id,       label,j))) | tok <- getToks toks]
        (fun:_,[pos,label,j]) -> [(fun, (tok, (const pos,label,j))) | tok <- getToks toks]
        _ -> []
      _ -> []
    _ -> []
  merge rules@((fun,_):_) = (fun, \tok ->
    case lookup tok (map snd rules) of
      Just new -> return new
      _ -> lookup "*"  (map snd rules)
    )
  getToks = words . map (\c -> if elem c "\"," then ' ' else c)

printCoNLL :: CoNLL -> String
printCoNLL = unlines . map (concat . intersperse "\t")

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

showContext :: [Var] -> [(BindType,Var,Type)] -> String
showContext scope hypos =
  unsafePerformIO $
  withHypos hypos $ \n_hypos c_hypos ->
  bracket (newPrintCtxt scope) freePrintCtxt $ \pctxt ->
  withForeignPtr marshaller $ \m ->
  bracket (pgf_print_context n_hypos c_hypos pctxt 0 m) free $ \c_text ->
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

readContext :: String -> Maybe [Hypo]
readContext str =
  unsafePerformIO $
  withText str $ \c_str ->
  withForeignPtr unmarshaller $ \u ->
  alloca $ \p_n_hypos -> do
    c_hypos <- pgf_read_context c_str u p_n_hypos
    n_hypos <- peek p_n_hypos
    if c_hypos == nullPtr && n_hypos /= 0
      then return Nothing
      else do hypos <- peekHypos Nothing n_hypos c_hypos
              free c_hypos
              return (Just hypos)
  where
    peekHypos mb_last 0       p_hypo = do
      case mb_last of
        Just last -> freeStablePtr last
        _         -> return ()
      return []
    peekHypos mb_last n_hypos p_hypo = do
      bt  <- fmap unmarshalBindType ((#peek PgfTypeHypo, bind_type) p_hypo)
      c_cid <- (#peek PgfTypeHypo, cid) p_hypo
      cid <- peekText c_cid
      free c_cid
      c_ty <- (#peek PgfTypeHypo, type) p_hypo
      ty <- deRefStablePtr c_ty
      case mb_last of
        Just last | last /= c_ty -> freeStablePtr last
        _                        -> return ()
      hs  <- peekHypos (Just c_ty) (n_hypos-1) (p_hypo `plusPtr` (#size PgfTypeHypo))
      return ((bt,cid,ty):hs)

readProbabilitiesFromFile :: FilePath -> IO (Map.Map String Double)
readProbabilitiesFromFile fpath = do
  s <- readFile fpath
  return $ Map.fromList [(f,read p) | f:p:_ <- map words (lines s)]
