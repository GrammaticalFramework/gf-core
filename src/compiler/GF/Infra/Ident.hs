----------------------------------------------------------------------
-- |
-- Module      : Ident
-- Maintainer  : AR
-- Stability   : (stable)
-- Portability : (portable)
--
-- > CVS $Date: 2005/11/15 11:43:33 $
-- > CVS $Author: aarne $
-- > CVS $Revision: 1.8 $
--
-- (Description of the module)
-----------------------------------------------------------------------------

module GF.Infra.Ident (-- ** Identifiers
  ModuleName(..), moduleNameS,
  Ident, ident2utf8, showIdent, prefixIdent,
  -- *** Normal identifiers (returned by the parser)
  identS, identC, identW,
  -- *** Special identifiers for internal use
  identV,
  varStr, varX, varIndex,
  -- *** Raw identifiers
  RawIdent, rawIdentS, rawIdentC, ident2raw, prefixRawIdent,
  isPrefixOf, showRawIdent
) where

import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Char8 as BS(append,isPrefixOf)
                 -- Limit use of BS functions to the ones that work correctly on
                 -- UTF-8-encoded bytestrings!
import Data.Char(isDigit)
import Data.Binary(Binary(..))
import GF.Text.Pretty


-- | Module names
newtype ModuleName = MN Ident deriving (Eq,Ord)

moduleNameS = MN . identS

instance Show ModuleName where showsPrec d (MN m) = showsPrec d m
instance Pretty ModuleName where pp (MN m) = pp m

instance Binary ModuleName where
  put (MN id) = put id
  get = fmap MN get

-- | the constructors labelled /INTERNAL/ are
-- internal representation never returned by the parser
data Ident =
   IC  {-# UNPACK #-} !RawIdent                                           -- ^ raw identifier after parsing, resolved in Rename
 | IW                                                                     -- ^ wildcard
--
-- below this constructor: internal representation never returned by the parser
 | IV  {-# UNPACK #-} !RawIdent {-# UNPACK #-} !Int                       -- ^ /INTERNAL/ variable
  deriving (Eq, Ord, Show, Read)

-- | Identifiers are stored as UTF-8-encoded bytestrings.
-- (It is also possible to use regular Haskell 'String's, with somewhat
-- reduced performance and increased memory use.)
newtype RawIdent = Id { rawId2utf8 :: UTF8.ByteString }
  deriving (Eq, Ord, Show, Read)

pack = UTF8.fromString
unpack = UTF8.toString

rawIdentS = Id . pack
rawIdentC = Id
showRawIdent = unpack . rawId2utf8

prefixRawIdent (Id x) (Id y) = Id (BS.append x y)
isPrefixOf (Id x) (Id y) = BS.isPrefixOf x y

instance Binary Ident where
  put id = put (ident2utf8 id)
  get    = do bs <- get
              if bs == wild
                then return identW
                else return (identC (rawIdentC bs))

instance Binary RawIdent where
  put = put . rawId2utf8
  get = fmap rawIdentC get

-- | This function should be used with care, since the returned ByteString is
-- UTF-8-encoded.
ident2utf8 :: Ident -> UTF8.ByteString
ident2utf8 i = case i of
  IC (Id s) -> s
  IV (Id s) n -> BS.append s (pack ('_':show n))
  IW -> wild

ident2raw :: Ident -> RawIdent
ident2raw = Id . ident2utf8

showIdent :: Ident -> String
showIdent i = unpack $! ident2utf8 i

instance Pretty Ident where pp = pp . showIdent

instance Pretty RawIdent where pp = pp . showRawIdent

identS :: String -> Ident
identS = identC . rawIdentS

identC :: RawIdent -> Ident
identW :: Ident

prefixIdent :: String -> Ident -> Ident
prefixIdent pref = identC . Id . BS.append (pack pref) . ident2utf8

identV :: RawIdent -> Int -> Ident

(identC, identV, identW) =
    (IC,     IV,     IW)

-- | used in lin defaults
varStr :: Ident
varStr = identS "str"

-- | refreshing variables
varX :: Int -> Ident
varX = identV (rawIdentS "x")

wild = pack "_"

varIndex :: Ident -> Int
varIndex (IV _ n) = n
varIndex _ = -1 --- other than IV should not count
