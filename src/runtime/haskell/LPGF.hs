-- | Linearisation-only PGF format
-- Closely follows description in Section 2 of Angelov, Bringert, Ranta (2009)
-- "PGF: A Portable Run-Time Format for Type-Theoretical Grammars"
module LPGF where

import PGF (Language)
import PGF.CId
import PGF.Expr (Expr)
import PGF.Tree (Tree (..), expr2tree, prTree)

import Data.Binary (Binary, get, put, encodeFile, decodeFile)
import Data.Char (toUpper)
import Data.List (isPrefixOf)
import qualified Data.Map as Map
import Text.Printf (printf)

import Prelude hiding ((!!))
import qualified Prelude

-- | Linearisation-only PGF
data LPGF = LPGF {
  absname   :: CId,
  abstract  :: Abstr,
  concretes :: Map.Map CId Concr
} deriving (Show)

-- | Abstract syntax
data Abstr = Abstr {
} deriving (Show)

-- | Concrete syntax
data Concr = Concr {
  -- lincats :: Map.Map CId LinType, -- ^ assigning a linearization type to each category
  lins    :: Map.Map CId LinFun  -- ^ assigning a linearization function to each function
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
  | LFBind
  | LFSpace
  | LFCapit
  | LFAllCapit
  | LFPre [([String], LinFun)] LinFun

  -- From original definition in paper
  | LFEmpty
  | LFToken String
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

instance Binary Abstr where
  put abs = return ()
  get = return $ Abstr {}

instance Binary Concr where
  put concr = put (lins concr)
  get = do
    ls <- get
    return $ Concr {
      lins = ls
    }

instance Binary LinFun where
  put = put . show
  get = read <$> get

abstractName :: LPGF -> CId
abstractName = absname

encodeFile :: FilePath -> LPGF -> IO ()
encodeFile = Data.Binary.encodeFile

readLPGF :: FilePath -> IO LPGF
readLPGF = Data.Binary.decodeFile

-- | Main linearize function
linearize :: LPGF -> Language -> Expr -> String
linearize lpgf lang =
  case Map.lookup lang (concretes lpgf) of
    Just concr -> linearizeConcr concr
    Nothing -> error $ printf "Unknown language: %s" (showCId lang)

-- | Language-specific linearize function
linearizeConcr :: Concr -> Expr -> String
linearizeConcr concr expr = lin2string $ lin (expr2tree expr)
  where
    lin :: Tree -> LinFun
    lin tree = case tree of
      Fun f as ->
        case Map.lookup f (lins concr) of
          Just t -> eval (map lin as) t
          _ -> error $ printf "Lookup failed for function: %s" (showCId f)
      x -> error $ printf "Cannot lin: %s" (prTree x)

-- | Evaluation context is a sequence of terms
type Context = [LinFun]

-- | Operational semantics, Table 2
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
      (LFTuple vs, LFInt i) -> vs !! (i-1)
      (tp@(LFTuple _), LFTuple is) | all isInt is -> foldl (\(LFTuple vs) (LFInt i) -> vs !! (i-1)) tp is
      (t',u') -> error $ printf "Incompatible projection:\n%s\n%s" (show t) (show u)
  LFArgument i -> cxt !! (i-1)
  _ -> t

-- | Turn concrete syntax terms into an actual string
lin2string :: LinFun -> String
lin2string l = case l of
  LFEmpty -> ""
  LFBind -> "" -- when encountered at beginning/end
  LFSpace -> "" -- when encountered at beginning/end
  LFToken tok -> tok
  LFTuple [l] -> lin2string l
  LFConcat (LFPre pts df) l2 -> lin2string $ LFConcat l1 l2
    where
      l2' = lin2string l2
      matches = [ l | (pfxs, l) <- pts, any (`isPrefixOf` l2') pfxs ]
      l1 = if null matches then df else head matches
  LFConcat l1 (LFConcat LFBind l2) -> lin2string l1 ++ lin2string l2
  LFConcat l1 (LFConcat LFSpace l2) -> lin2string $ LFConcat l1 l2
  LFConcat LFCapit l2 -> let l = lin2string l2 in toUpper (head l) : tail l
  LFConcat LFAllCapit l2 -> let tks = words (lin2string l2) in unwords $ map toUpper (head tks) : tail tks
  LFConcat l1 l2 -> unwords $ filter (not.null) [lin2string l1, lin2string l2]
  x -> printf "[%s]" (show x)

(!!) :: (Show a) => [a] -> Int -> a
(!!) xs i
  | i < 0 = error $ printf "!!: index %d too small for list: %s" i (show xs)
  | i > length xs - 1 = error $ printf "!!: index %d too large for list: %s" i (show xs)
  | otherwise = xs Prelude.!! i

isInt :: LinFun -> Bool
isInt (LFInt _) = True
isInt _ = False
