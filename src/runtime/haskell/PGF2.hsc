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

             -- * Abstract syntax
             AbsName,abstractName,globalFlag,abstractFlag,

             -- ** Categories
             Cat,categories,categoryContext,categoryProbability,

             -- ** Functions
             Fun, functions, functionsByCat,
             functionType, functionIsConstructor, functionProbability,

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
             exprSize, exprFunctions, exprSubstitute, exprProbability,

             -- ** Types
             Type(..), Hypo, BindType(..), startCat,
             readType, showType, showContext,
             mkType, unType,
             mkHypo, mkDepHypo, mkImplHypo,

             -- ** Type checking
             -- | Dynamically-built expressions should always be type-checked before using in other functions,
             -- as the exceptions thrown by using invalid expressions may not catchable.
             checkExpr, inferExpr, checkType,

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
             hasLinearization,
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
import Control.Exception(mask_,bracket)
import System.IO.Unsafe(unsafePerformIO)
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
readPGF fpath =
  withCString fpath $ \c_fpath ->
  alloca $ \p_revision ->
  mask_ $ do
    c_db <- withPgfExn "readPGF" (pgf_read_pgf c_fpath p_revision)
    c_revision <- peek p_revision
    fptr <- newForeignPtrEnv pgf_free_revision c_db c_revision
    langs <- getConcretes c_db fptr
    return (PGF c_db fptr langs)

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
    c_db <- withPgfExn "bootNGF" (pgf_boot_ngf c_pgf_path c_ngf_path p_revision)
    c_revision <- peek p_revision
    fptr <- newForeignPtrEnv pgf_free_revision c_db c_revision
    langs <- getConcretes c_db fptr
    return (PGF c_db fptr langs)

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
newNGF :: AbsName -> Maybe FilePath -> IO PGF
newNGF abs_name mb_fpath =
  withText abs_name $ \c_abs_name ->
  maybe (\f -> f nullPtr) withCString mb_fpath $ \c_fpath ->
  alloca $ \p_revision ->
  mask_ $ do
    c_db <- withPgfExn "newNGF" (pgf_new_ngf c_abs_name c_fpath p_revision)
    c_revision <- peek p_revision
    fptr <- newForeignPtrEnv pgf_free_revision c_db c_revision
    return (PGF c_db fptr Map.empty)

writePGF :: FilePath -> PGF -> IO ()
writePGF fpath p =
  withCString fpath $ \c_fpath ->
  withForeignPtr (a_revision p) $ \c_revision ->
    withPgfExn "writePGF" (pgf_write_pgf c_fpath (a_db p) c_revision)

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

    ppConcr name c =
      text "concrete" <+> text name <+> char '{' $$
      nest 2 (ppLincats c $$
              ppLins c) $$
      char '}'

    ppLincats c = unsafePerformIO $ do
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
          fields <- allocaBytes (1*(#size size_t)) $ \pcounts -> do
                      pgf_get_lincat_counts_internal val pcounts
                      n_fields <- peekElemOff pcounts 0
                      forM [0..n_fields-1] $ \i -> do
                        pgf_get_lincat_field_internal val i >>= peekText
          let def = text "lincat" <+> (text name <+> char '=' <+> char '[' $$
                                       nest 2 (vcat (map text fields)) $$
                                       char ']')
          modifyIORef ref $ (\doc -> doc $$ def)

    ppLins c = unsafePerformIO $ do
      ref <- newIORef empty
      (allocaBytes (#size PgfItor) $ \itor ->
       bracket (wrapItorCallback (getLins ref)) freeHaskellFunPtr $ \fptr ->
       withForeignPtr (c_revision c) $ \c_revision -> do
         (#poke PgfItor, fn) itor fptr
         withPgfExn "showPGF" (pgf_iter_lins (a_db p) c_revision itor))
      readIORef ref
      where
        getLins :: IORef Doc -> ItorCallback
        getLins ref itor key val exn =
          allocaBytes (2*(#size size_t)) $ \pcounts -> do
            pgf_get_lin_counts_internal val pcounts
            n_prods <- peekElemOff pcounts 0
            n_seqs  <- peekElemOff pcounts 1
            forM_ [0..n_prods-1] $ \i -> do
              sig <- bracket (pgf_print_lin_sig_internal val i) free $ \c_text -> do
                       fmap text (peekText c_text)
              syms <- forM [0..n_seqs-1] $ \j ->
                        bracket (pgf_print_lin_seq_internal val i j) free $ \c_text -> do
                          fmap text (peekText c_text)
              let def = text "lin" <+> (sig <+> char '=' <+> char '[' $$
                                        nest 2 (vcat syms) $$
                                        char ']')
              modifyIORef ref $ (\doc -> doc $$ def)

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
inferExpr = error "TODO: inferExpr"

-- | Check whether a type is consistent with the abstract
-- syntax of the grammar.
checkType :: PGF -> Type -> Either String Type
checkType = error "TODO: checkType"
  
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
printName lang fun = error "TODO: printName"

alignWords :: Concr -> Expr -> [(String, [Int])]
alignWords = error "TODO: alignWords"

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
lookupMorpho = error "TODO: lookupMorpho"

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
lookupCohorts = error "TODO: lookupCohorts"

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
fullFormLexicon lang = error "TODO: fullFormLexicon"

-- | This data type encodes the different outcomes which you could get from the parser.
data ParseOutput a
  = ParseFailed Int String         -- ^ The integer is the position in number of unicode characters where the parser failed.
                                   -- The string is the token where the parser have failed.
  | ParseOk a                      -- ^ If the parsing and the type checking are successful
                                   -- we get the abstract syntax trees as either a list or a chart.
  | ParseIncomplete                -- ^ The sentence is not complete.

parse :: Concr -> Type -> String -> ParseOutput [(Expr,Float)]
parse lang ty sent = parseWithHeuristics lang ty sent (-1.0) []

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

-- | Linearizes an expression as a string in the language
linearize :: Concr -> Expr -> String
linearize lang e = error "TODO: linearize"

-- | Generates all possible linearizations of an expression
linearizeAll :: Concr -> Expr -> [String]
linearizeAll lang e = error "TODO: linearizeAll"

-- | Generates a table of linearizations for an expression
tabularLinearize :: Concr -> Expr -> [(String, String)]
tabularLinearize lang e =
  case tabularLinearizeAll lang e of
    (lins:_) -> lins
    _        -> []

-- | Generates a table of linearizations for an expression
tabularLinearizeAll :: Concr -> Expr -> [[(String, String)]]
tabularLinearizeAll lang e = error "TODO: tabularLinearizeAll"

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
bracketedLinearize = error "TODO: bracketedLinearize"

bracketedLinearizeAll :: Concr -> Expr -> [[BracketedString]]
bracketedLinearizeAll = error "TODO: bracketedLinearizeAll"

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

-- | Renders an abstract syntax tree in a Graphviz format.
graphvizAbstractTree :: PGF -> GraphvizOptions -> Expr -> String
graphvizAbstractTree p opts e = error "TODO: graphvizAbstractTree"

graphvizParseTree :: Concr -> GraphvizOptions -> Expr -> String
graphvizParseTree c opts e = error "TODO: graphvizParseTree"

graphvizWordAlignment :: [Concr] -> GraphvizOptions -> Expr -> String
graphvizWordAlignment cs opts e = error "TODO: graphvizWordAlignment"


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

readProbabilitiesFromFile :: FilePath -> IO (Map.Map String Double)
readProbabilitiesFromFile fpath = do
  s <- readFile fpath
  return $ Map.fromList [(f,read p) | f:p:_ <- map words (lines s)]
