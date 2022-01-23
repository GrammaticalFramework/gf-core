{-# LANGUAGE LambdaCase #-}

module LPGF.Internal where

import PGF.CId
import PGF ()

import Control.Monad (liftM, liftM2, forM_)
import qualified Control.Monad.Writer as CMW
import Data.Binary (Binary, put, get, putWord8, getWord8, encodeFile)
import qualified Data.IntMap as IntMap
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

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

encodeFile :: FilePath -> LPGF -> IO ()
encodeFile = Data.Binary.encodeFile

------------------------------------------------------------------------------
-- Utilities

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
