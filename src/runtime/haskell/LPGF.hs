{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Linearisation-only portable grammar format.
--
-- LPGF is an output format from the GF compiler, intended as a smaller and faster alternative to PGF.
-- This API allows LPGF files to be used in Haskell programs.
--
-- The implementation closely follows description in Section 2 of Angelov, Bringert, Ranta (2009):
-- "PGF: A Portable Run-Time Format for Type-Theoretical Grammars".
-- http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.640.6330&rep=rep1&type=pdf
module LPGF (
  -- * LPGF
  LPGF,
  showLPGF,
  readLPGF,

  -- * Identifiers
  CId,
  mkCId,
  showCId,
  readCId,

  -- * Abstract syntax
  Abstract,
  abstractName,

  -- ** Categories

  -- ** Functions

  -- ** Expressions
  Expr,
  PGF.showExpr,
  PGF.readExpr,

  -- ** Types

  -- ** Type checking

  -- * Concrete syntax
  Language,
  PGF.showLanguage,
  PGF.readLanguage,
  languages,
  Concrete,
  LPGF.concretes,

  -- ** Linearization
  linearize,
  linearizeText,
  linearizeConcrete,
  linearizeConcreteText
) where

import LPGF.Internal
import PGF (Language)
import PGF.CId
import PGF.Expr (Expr, Literal (..))
import PGF.Tree (Tree (..), expr2tree, prTree)
import qualified PGF

import Data.Binary (decodeFile)
import Data.Either (isLeft)
import qualified Data.IntMap as IntMap
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Numeric (showFFloat)
import Text.Printf (printf)

import Prelude hiding ((!!))
import qualified Prelude

-- | The abstract language name is the name of the top-level abstract module.
abstractName :: LPGF -> CId
abstractName = absname

-- | List of all languages available in the given grammar.
languages :: LPGF -> [Language]
languages = Map.keys . LPGF.Internal.concretes

-- | Map of all languages and their corresponding concrete sytaxes.
concretes :: LPGF -> Map.Map Language Concrete
concretes = LPGF.Internal.concretes

-- | Reads file in LPGF and produces 'LPGF' term.
-- The file is usually produced with:
--
-- > $ gf --make --output-format=lpgf <grammar file name>
readLPGF :: FilePath -> IO LPGF
readLPGF = Data.Binary.decodeFile

-- | Produce pretty-printed representation of an LPGF.
showLPGF :: LPGF -> String
showLPGF = render . pp

-- | Main linearize function, to 'String'
linearize :: LPGF -> Language -> Expr -> String
linearize lpgf lang expr = T.unpack $ linearizeText lpgf lang expr

-- | Main linearize function, to 'Data.Text.Text'
linearizeText :: LPGF -> Language -> Expr -> Text
linearizeText lpgf lang =
  case Map.lookup lang (LPGF.Internal.concretes lpgf) of
    Just concr -> linearizeConcreteText concr
    Nothing -> error $ printf "Unknown language: %s" (showCId lang)

-- | Language-specific linearize function, to 'String'
linearizeConcrete :: Concrete -> Expr -> String
linearizeConcrete concr expr = T.unpack $ linearizeConcreteText concr expr

-- | Language-specific linearize function, to 'Data.Text.Text'
linearizeConcreteText :: Concrete -> Expr -> Text
linearizeConcreteText concr expr = lin2string $ lin (expr2tree expr)
  where
    lin :: Tree -> LinFun
    lin = \case
      Fun f as ->
        case Map.lookup f (lins concr) of
          Just t -> eval cxt t
            where cxt = Context { cxToks = toks concr, cxArgs = map lin as }
          _ -> Missing f
      Lit l -> Tuple [Token (T.pack s)]
        where
          s = case l of
            LStr s -> s
            LInt i -> show i
            LFlt f -> showFFloat (Just 6) f ""
      x -> error $ printf "Cannot lin: %s" (prTree x)

-- | Evaluation context
data Context = Context {
  cxArgs :: [LinFun], -- ^  is a sequence of terms
  cxToks :: IntMap.IntMap Text -- ^ token map
}

-- | Operational semantics
eval :: Context -> LinFun -> LinFun
eval cxt t = case t of
  Error err -> error err
  Pre pts df -> Pre pts' df'
    where
      pts' = [(pfxs, eval cxt t) | (pfxs, t) <- pts]
      df' = eval cxt df

  Concat s t -> Concat v w
    where
      v = eval cxt s
      w = eval cxt t
  Tuple ts -> Tuple vs
    where vs = map (eval cxt) ts
  Projection t u ->
    case (eval cxt t, eval cxt u) of
      (Missing f, _) -> Missing f
      (_, Missing f) -> Missing f
      (Tuple vs, Ix i) -> vs !! (i-1)
      (t', tv@(Tuple _)) -> eval cxt $ foldl Projection t' (flattenTuple tv)
      (t',u') -> error $ printf "Incompatible projection:\n- %s\n⇓ %s\n- %s\n⇓ %s" (show t) (show t') (show u) (show u')
  Argument i -> cxArgs cxt !! (i-1)

  PreIx pts df -> Pre pts' df'
    where
      pts' = [(pfxs, eval cxt t) | (ix, t) <- pts, let pfxs = maybe [] (read . T.unpack) $ IntMap.lookup ix (cxToks cxt)]
      df' = eval cxt df
  TokenIx i -> maybe Empty Token $ IntMap.lookup i (cxToks cxt)

  _ -> t

flattenTuple :: LinFun -> [LinFun]
flattenTuple = \case
  Tuple vs -> concatMap flattenTuple vs
  lf -> [lf]

-- | Turn concrete syntax terms into an actual string.
-- This is done in two passes, first to flatten concats & evaluate pre's, then to
-- apply BIND and other predefs.
lin2string :: LinFun -> Text
lin2string lf = T.unwords $ join $ flatten [lf]
  where
    -- Process bind et al into final token list
    join :: [Either LinFun Text] -> [Text]
    join elt = case elt of
      Right tok:Left Bind:ls ->
        case join ls of
          next:ls' -> tok `T.append` next : ls'
          _ -> []
      Right tok:ls -> tok : join ls
      Left Space:ls -> join ls
      Left Capit:ls ->
        case join ls of
          next:ls' -> T.toUpper (T.take 1 next) `T.append` T.drop 1 next : ls'
          _ -> []
      Left AllCapit:ls ->
        case join ls of
          next:ls' -> T.toUpper next : ls'
          _ -> []
      Left (Missing cid):ls -> join (Right (T.pack (printf "[%s]" (show cid))) : ls)
      [] -> []
      x -> error $ printf "Unhandled term in lin2string: %s" (show x)

    -- Process concats, tuples, pre into flat list
    flatten :: [LinFun] -> [Either LinFun Text]
    flatten [] = []
    flatten (l:ls) = case l of
      Empty -> flatten ls
      Token "" -> flatten ls
      Token tok -> Right tok : flatten ls
      Concat l1 l2 -> flatten (l1 : l2 : ls)
      Tuple [l] -> flatten (l:ls)
      Tuple (l:_) -> flatten (l:ls) -- unselected table, just choose first option (see e.g. FoodsJpn)
      Pre pts df ->
        let
          f = flatten ls
          ch = case dropWhile isLeft f of
            Right next:_ ->
              let matches = [ l | (pfxs, l) <- pts, any (`T.isPrefixOf` next) pfxs ]
              in  if null matches then df else head matches
            _ -> df
        in flatten (ch:ls)
      x -> Left x : flatten ls

-- | List indexing with more verbose error messages
(!!) :: (Show a) => [a] -> Int -> a
(!!) xs i
  | i < 0 = error $ printf "!!: index %d too small for list: %s" i (show xs)
  | i > length xs - 1 = error $ printf "!!: index %d too large for list: %s" i (show xs)
  | otherwise = xs Prelude.!! i

isIx :: LinFun -> Bool
isIx (Ix _) = True
isIx _ = False
