{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Linearisation-only grammar format.
-- Closely follows description in Section 2 of Angelov, Bringert, Ranta (2009):
-- "PGF: A Portable Run-Time Format for Type-Theoretical Grammars".
module LPGF where

import PGF (Language)
import PGF.CId
import PGF.Expr (Expr)
import PGF.Tree (Tree (..), expr2tree, prTree)

import Control.Monad (liftM, liftM2, forM_)
import qualified Control.Monad.Writer as CMW
import Data.Binary (Binary, put, get, putWord8, getWord8, encodeFile, decodeFile)
import qualified Data.Map as Map
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
newtype Concrete = Concrete {
  -- lincats :: Map.Map CId LinType, -- ^ a linearization type for each category
  lins    :: Map.Map CId LinFun  -- ^ a linearization function for each function
} deriving (Show)

-- | Abstract function type
-- data Type = Type [CId] CId
--   deriving (Show)

-- -- | Linearisation type
-- data LinType =
--     LTStr
--   | LTInt Int
--   | LTProduct [LinType]
--   deriving (Show)

-- | Linearisation function
data LinFun =
  -- Additions
    LFError String -- ^ a runtime error, should probably not be supported at all
  | LFBind -- ^ join adjacent tokens
  | LFSpace -- ^ space between adjacent tokens
  | LFCapit -- ^ capitalise next character
  | LFAllCapit -- ^ capitalise next word
  | LFPre [([Text], LinFun)] LinFun
  | LFMissing CId -- ^ missing definition (inserted at runtime)

  -- From original definition in paper
  | LFEmpty
  | LFToken Text
  | LFConcat LinFun LinFun
  | LFInt Int
  | LFTuple [LinFun]
  | LFProjection LinFun LinFun
  | LFArgument Int
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
  put concr = put (lins concr)
  get = do
    ls <- get
    return $ Concrete {
      lins = ls
    }

instance Binary LinFun where
  put = \case
    LFError e          -> putWord8 0 >> put e
    LFBind             -> putWord8 1
    LFSpace            -> putWord8 2
    LFCapit            -> putWord8 3
    LFAllCapit         -> putWord8 4
    LFPre ps d         -> putWord8 5 >> put ([(map TE.encodeUtf8 p,l) | (p,l) <- ps],d)
    LFMissing f        -> putWord8 13 >> put f
    LFEmpty            -> putWord8 6
    LFToken t          -> putWord8 7 >> put (TE.encodeUtf8 t)
    LFConcat l1 l2     -> putWord8 8 >> put (l1,l2)
    LFInt i            -> putWord8 9 >> put i
    LFTuple ls         -> putWord8 10 >> put ls
    LFProjection l1 l2 -> putWord8 11 >> put (l1,l2)
    LFArgument i       -> putWord8 12 >> put i
  get = do
    tag <- getWord8
    case tag of
      0  -> liftM LFError get
      1  -> return LFBind
      2  -> return LFSpace
      3  -> return LFCapit
      4  -> return LFAllCapit
      5  -> liftM2 (\ps -> LFPre [(map TE.decodeUtf8 p,l) | (p,l) <- ps]) get get
      13 -> liftM LFMissing get
      6  -> return LFEmpty
      7  -> liftM (LFToken . TE.decodeUtf8) get
      8  -> liftM2 LFConcat get get
      9  -> liftM LFInt get
      10 -> liftM LFTuple get
      11 -> liftM2 LFProjection get get
      12 -> liftM LFArgument get
      _ -> fail "Failed to decode LPGF binary format"

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
          Just t -> eval (map lin as) t
          -- _ -> error $ printf "Lookup failed for function: %s" (showCId f)
          _ -> LFMissing f
      x -> error $ printf "Cannot lin: %s" (prTree x)

-- | Evaluation context is a sequence of terms
type Context = [LinFun]

-- | Operational semantics
eval :: Context -> LinFun -> LinFun
eval cxt t = case t of
  LFError err -> error err
  LFPre pts df -> LFPre pts' df'
    where
      pts' = [ (strs, eval cxt t) | (strs,t) <- pts]
      df' = eval cxt df
  LFConcat s t -> LFConcat v w
    where
      v = eval cxt s
      w = eval cxt t
  LFTuple ts -> LFTuple vs
    where vs = map (eval cxt) ts
  LFProjection t u ->
    case (eval cxt t, eval cxt u) of
      (LFMissing f, _) -> LFMissing f
      (_, LFMissing f) -> LFMissing f
      (LFTuple vs, LFInt i) -> vs !! (i-1)
      (tp@(LFTuple _), LFTuple is) | all isInt is -> foldl (\(LFTuple vs) (LFInt i) -> vs !! (i-1)) tp is
      (t',u') -> error $ printf "Incompatible projection:\n- %s ~> %s\n- %s ~> %s" (show t) (show t') (show u) (show u')
  LFArgument i -> cxt !! (i-1)
  _ -> t

-- | Turn concrete syntax terms into an actual string
lin2string :: LinFun -> Text
lin2string l = case l of
  LFEmpty -> ""
  LFBind -> "" -- when encountered at beginning/end
  LFSpace -> "" -- when encountered at beginning/end
  LFToken tok -> tok
  LFMissing cid -> T.pack $ printf "[%s]" (show cid)
  LFTuple [l] -> lin2string l
  LFTuple (l:_) -> lin2string l -- unselected table, just choose first option (see e.g. FoodsJpn)
  LFPre pts df -> lin2string df -- when encountered at end
  LFConcat (LFPre pts df) l2 -> lin2string $ LFConcat l1 l2
    where
      l2' = lin2string l2
      matches = [ l | (pfxs, l) <- pts, any (`T.isPrefixOf` l2') pfxs ]
      l1 = if null matches then df else head matches
  LFConcat l1 (LFConcat LFBind l2) -> lin2string l1 `T.append` lin2string l2
  LFConcat l1 (LFConcat LFSpace l2) -> lin2string $ LFConcat l1 l2
  LFConcat LFCapit l2 -> let l = lin2string l2 in T.toUpper (T.take 1 l) `T.append` T.drop 1 l
  LFConcat LFAllCapit l2 -> let tks = T.words (lin2string l2) in T.unwords $ T.toUpper (head tks) : tail tks
  LFConcat l1 l2 -> T.unwords $ filter (not.T.null) [lin2string l1, lin2string l2]
  x -> T.pack $ printf "[%s]" (show x)

-- | List indexing with more verbose error messages
(!!) :: (Show a) => [a] -> Int -> a
(!!) xs i
  | i < 0 = error $ printf "!!: index %d too small for list: %s" i (show xs)
  | i > length xs - 1 = error $ printf "!!: index %d too large for list: %s" i (show xs)
  | otherwise = xs Prelude.!! i

isInt :: LinFun -> Bool
isInt (LFInt _) = True
isInt _ = False

-- | Helper for building concat trees
mkConcat :: [LinFun] -> LinFun
mkConcat [] = LFEmpty
mkConcat [x] = x
mkConcat xs = foldl1 LFConcat xs

-- | Helper for unfolding concat trees
unConcat :: LinFun -> [LinFun]
unConcat (LFConcat l1 l2) = concatMap unConcat [l1, l2]
unConcat lf = [lf]

------------------------------------------------------------------------------
-- Pretty-printing

type Doc = CMW.Writer [Text] ()

render :: Doc -> Text
render = T.unlines . CMW.execWriter

class PP a where
  pp :: a -> Doc

instance PP LPGF where
  pp (LPGF _ _ cncs) = mapM_ pp cncs

instance PP Concrete where
  pp (Concrete lins) =
    forM_ (Map.toList lins) $ \(cid,lin) -> do
      CMW.tell [T.pack ("# " ++ showCId cid)]
      pp lin
      CMW.tell [""]

instance PP LinFun where
  pp = pp' 0
    where
      pp' n = \case
        LFPre ps d -> do
          p "LFPre"
          CMW.tell [ T.replicate (n+1) "  " `T.append` T.pack (show p) | p <- ps ]
          pp' (n+1) d

        c@(LFConcat l1 l2) -> do
          let ts = unConcat c
          if any isDeep ts
          then do
            p "LFConcat"
            mapM_ (pp' (n+1)) ts
          else
            ps $ "LFConcat " ++ show ts
        LFTuple ls | any isDeep ls -> do
          p "LFTuple"
          mapM_ (pp' (n+1)) ls
        LFProjection l1 l2 | isDeep l1 || isDeep l2 -> do
          p "LFProjection"
          pp' (n+1) l1
          pp' (n+1) l2
        t -> ps $ show t
        where
          p :: Text -> Doc
          p t = CMW.tell [ T.replicate n "  " `T.append` t ]
          ps :: String -> Doc
          ps = p . T.pack

      isDeep = not . isTerm
      isTerm = \case
        LFPre _ _ -> False
        LFConcat _ _ -> False
        LFTuple _ -> False
        LFProjection _ _ -> False
        _ -> True
