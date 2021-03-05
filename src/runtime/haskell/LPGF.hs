{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Linearisation-only grammar format.
-- Closely follows description in Section 2 of Angelov, Bringert, Ranta (2009):
-- "PGF: A Portable Run-Time Format for Type-Theoretical Grammars".
-- http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.640.6330&rep=rep1&type=pdf
module LPGF where

import PGF (Language)
import PGF.CId
import PGF.Expr (Expr)
import PGF.Tree (Tree (..), expr2tree, prTree)

import qualified Control.Exception as EX
import Control.Monad (liftM, liftM2, forM_)
import qualified Control.Monad.Writer as CMW
import Data.Binary (Binary, put, get, putWord8, getWord8, encodeFile, decodeFile)
import qualified Data.IntMap as IntMap
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Text.Printf (printf)

import Prelude hiding ((!!))
import qualified Prelude

-- | Linearisation-only PGF
data LPGF = LPGF {
  absname   :: CId,
  abstract  :: Abstract,
  concretes :: Map.Map CId Concrete
} deriving (Show)

-- | Abstract syntax (currently empty)
data Abstract = Abstract {
} deriving (Show)

-- | Concrete syntax
data Concrete = Concrete {
  toks    :: IntMap.IntMap Text, -- ^ all strings are stored exactly once here
  -- lincats :: Map.Map CId LinType, -- ^ a linearization type for each category
  lins    :: Map.Map CId LinFun  -- ^ a linearization function for each function
} deriving (Show)

-- | Abstract function type
-- data Type = Type [CId] CId
--   deriving (Show)

-- -- | Linearisation type
-- data LinType =
--     StrType
--   | IxType Int
--   | ProductType [LinType]
--   deriving (Show)

-- | Linearisation function
data LinFun =
  -- Additions
    Error String -- ^ a runtime error, should probably not be supported at all
  | Bind -- ^ join adjacent tokens
  | Space -- ^ space between adjacent tokens
  | Capit -- ^ capitalise next character
  | AllCapit -- ^ capitalise next word
  | Pre [([Text], LinFun)] LinFun
  | Missing CId -- ^ missing definition (inserted at runtime)

  -- From original definition in paper
  | Empty
  | Token Text
  | Concat LinFun LinFun
  | Ix Int
  | Tuple [LinFun]
  | Projection LinFun LinFun
  | Argument Int

  -- For reducing LPGF file when stored
  | PreIx [(Int, LinFun)] LinFun -- ^ index into `toks` map (must apply read to convert to list)
  | TokenIx Int -- ^ index into `toks` map

  deriving (Show, Read)

instance Binary LPGF where
  put lpgf = do
    put (absname lpgf)
    put (abstract lpgf)
    put (concretes lpgf)
  get = do
    an <- get
    abs <- get
    concs <- get
    return $ LPGF {
      absname = an,
      abstract = abs,
      concretes = concs
    }

instance Binary Abstract where
  put abs = return ()
  get = return $ Abstract {}

instance Binary Concrete where
  put concr = do
    put (toks concr)
    put (lins concr)
  get = do
    ts <- get
    ls <- get
    return $ Concrete {
      toks = ts,
      lins = ls
    }

instance Binary LinFun where
  put = \case
    Error e          -> putWord8 0 >> put e
    Bind             -> putWord8 1
    Space            -> putWord8 2
    Capit            -> putWord8 3
    AllCapit         -> putWord8 4
    Pre ps d         -> putWord8 5 >> put (ps,d)
    Missing f        -> putWord8 13 >> put f

    Empty            -> putWord8 6
    Token t          -> putWord8 7 >> put t
    Concat l1 l2     -> putWord8 8 >> put (l1,l2)
    Ix i             -> putWord8 9 >> put i
    Tuple ls         -> putWord8 10 >> put ls
    Projection l1 l2 -> putWord8 11 >> put (l1,l2)
    Argument i       -> putWord8 12 >> put i

    PreIx ps d       -> putWord8 15 >> put (ps,d)
    TokenIx i        -> putWord8 14 >> put i
  get = do
    tag <- getWord8
    case tag of
      0  -> liftM  Error get
      1  -> return Bind
      2  -> return Space
      3  -> return Capit
      4  -> return AllCapit
      5  -> liftM2 Pre get get
      13 -> liftM  Missing get

      6  -> return Empty
      7  -> liftM  Token get
      8  -> liftM2 Concat get get
      9  -> liftM  Ix get
      10 -> liftM  Tuple get
      11 -> liftM2 Projection get get
      12 -> liftM  Argument get

      15 -> liftM2 PreIx get get
      14 -> liftM  TokenIx get
      _  -> fail "Failed to decode LPGF binary format"

instance Binary Text where
  put = put . TE.encodeUtf8
  get = liftM TE.decodeUtf8 get

abstractName :: LPGF -> CId
abstractName = absname

encodeFile :: FilePath -> LPGF -> IO ()
encodeFile = Data.Binary.encodeFile

readLPGF :: FilePath -> IO LPGF
readLPGF = Data.Binary.decodeFile

-- | Main linearize function, to 'String'
linearize :: LPGF -> Language -> Expr -> String
linearize lpgf lang expr = T.unpack $ linearizeText lpgf lang expr

-- | Main linearize function, to 'Data.Text.Text'
linearizeText :: LPGF -> Language -> Expr -> Text
linearizeText lpgf lang =
  case Map.lookup lang (concretes lpgf) of
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
    lin tree = case tree of
      Fun f as ->
        case Map.lookup f (lins concr) of
          Just t -> eval cxt t
            where cxt = Context { cxToks = toks concr, cxArgs = map lin as }
          _ -> Missing f
      x -> error $ printf "Cannot lin: %s" (prTree x)

-- | Run a compatation and catch any exception/errors.
-- Ideally this library should never throw exceptions, but we're still in development...
try :: a -> IO (Either String a)
try comp = do
  let f = Right <$> EX.evaluate comp
  EX.catch f (\(e :: EX.SomeException) -> return $ Left (show e))

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
      (tp@(Tuple _), tv@(Tuple _)) | all isIx (flattenTuple tv) -> foldl (\(Tuple vs) (Ix i) -> vs !! (i-1)) tp (flattenTuple tv)
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

-- | Turn concrete syntax terms into an actual string
lin2string :: LinFun -> Text
lin2string l = case l of
  Empty -> ""
  Bind -> "" -- when encountered at beginning/end
  Space -> "" -- when encountered at beginning/end
  Token tok -> tok
  Missing cid -> T.pack $ printf "[%s]" (show cid)
  Tuple [l] -> lin2string l
  Tuple (l:_) -> lin2string l -- unselected table, just choose first option (see e.g. FoodsJpn)
  Pre pts df -> lin2string df -- when encountered at end
  Concat (Pre pts df) l2 -> lin2string $ Concat l1 l2
    where
      l2' = lin2string l2
      matches = [ l | (pfxs, l) <- pts, any (`T.isPrefixOf` l2') pfxs ]
      l1 = if null matches then df else head matches
  Concat l1 (Concat Bind l2) -> lin2string l1 `T.append` lin2string l2
  Concat l1 (Concat Space l2) -> lin2string $ Concat l1 l2
  Concat Capit l2 -> let l = lin2string l2 in T.toUpper (T.take 1 l) `T.append` T.drop 1 l
  Concat AllCapit l2 -> let tks = T.words (lin2string l2) in T.unwords $ T.toUpper (head tks) : tail tks
  Concat l1 l2 -> T.unwords $ filter (not.T.null) [lin2string l1, lin2string l2]
  x -> T.pack $ printf "[%s]" (show x)

-- | List indexing with more verbose error messages
(!!) :: (Show a) => [a] -> Int -> a
(!!) xs i
  | i < 0 = error $ printf "!!: index %d too small for list: %s" i (show xs)
  | i > length xs - 1 = error $ printf "!!: index %d too large for list: %s" i (show xs)
  | otherwise = xs Prelude.!! i

isIx :: LinFun -> Bool
isIx (Ix _) = True
isIx _ = False

-- | Helper for building concat trees
mkConcat :: [LinFun] -> LinFun
mkConcat [] = Empty
mkConcat [x] = x
mkConcat xs = foldl1 Concat xs

-- | Helper for unfolding concat trees
unConcat :: LinFun -> [LinFun]
unConcat (Concat l1 l2) = concatMap unConcat [l1, l2]
unConcat lf = [lf]

------------------------------------------------------------------------------
-- Pretty-printing

type Doc = CMW.Writer [String] ()

render :: Doc -> String
render = unlines . CMW.execWriter

class PP a where
  pp :: a -> Doc

instance PP LPGF where
  pp (LPGF _ _ cncs) = mapM_ pp cncs

instance PP Concrete where
  pp (Concrete toks lins) = do
    forM_ (IntMap.toList toks) $ \(i,tok) ->
      CMW.tell [show i ++ " " ++ T.unpack tok]
    CMW.tell [""]
    forM_ (Map.toList lins) $ \(cid,lin) -> do
      CMW.tell ["# " ++ showCId cid]
      pp lin
      CMW.tell [""]

instance PP LinFun where
  pp = pp' 0
    where
      pp' n = \case
        Pre ps d -> do
          p "Pre"
          CMW.tell [ replicate (2*(n+1)) ' ' ++ show p | p <- ps ]
          pp' (n+1) d

        c@(Concat l1 l2) -> do
          let ts = unConcat c
          if any isDeep ts
          then do
            p "Concat"
            mapM_ (pp' (n+1)) ts
          else
            p $ "Concat " ++ show ts
        Tuple ls | any isDeep ls -> do
          p "Tuple"
          mapM_ (pp' (n+1)) ls
        Projection l1 l2 | isDeep l1 || isDeep l2 -> do
          p "Projection"
          pp' (n+1) l1
          pp' (n+1) l2
        t -> p $ show t
        where
          p :: String -> Doc
          p t = CMW.tell [ replicate (2*n) ' ' ++ t ]

      isDeep = not . isTerm
      isTerm = \case
        Pre _ _ -> False
        Concat _ _ -> False
        Tuple _ -> False
        Projection _ _ -> False
        _ -> True
