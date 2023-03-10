module PGF ( PGF2.PGF, readPGF
           , abstractName

           , CId, mkCId, wildCId, showCId, readCId, pIdent

           , PGF2.categories, PGF2.categoryContext, PGF2.startCat
           , functions, functionsByCat

           , PGF2.Expr(..), PGF2.Literal(..), Tree
           , PGF2.readExpr, PGF2.showExpr, pExpr
           , PGF2.mkAbs,    PGF2.unAbs
           , PGF2.mkApp,    PGF2.unApp, PGF2.unapply
           , PGF2.mkStr,    PGF2.unStr
           , PGF2.mkInt,    PGF2.unInt
           , PGF2.mkDouble, PGF2.unDouble
           , PGF2.mkFloat,  PGF2.unFloat
           , PGF2.mkMeta,   PGF2.unMeta
           , PGF2.exprSize, PGF2.exprFunctions

           , PGF2.Type(..), PGF2.Hypo
           , PGF2.readType, PGF2.showType
           , PGF2.mkType, PGF2.unType
           , PGF2.mkHypo, PGF2.mkDepHypo, PGF2.mkImplHypo

           , PGF2.PGFError(..)
           ) where

import PGF2.FFI

import Foreign
import Foreign.C
import Control.Exception(mask_)
import Control.Monad
import qualified PGF2 as PGF2
import qualified Text.ParserCombinators.ReadP as RP
import System.IO.Unsafe(unsafePerformIO)

#include <pgf/pgf.h>

newtype CId = CId String deriving (Show,Read,Eq,Ord)

type Language = CId

readPGF = PGF2.readPGF


readLanguage = readCId
showLanguage (CId s) = s


abstractName gr = CId (PGF2.abstractName gr)


categories gr = map CId (PGF2.categories gr)


functions gr  = map CId (PGF2.functions gr)
functionsByCat gr (CId c) = map CId (PGF2.functionsByCat gr c)

type Tree = PGF2.Expr


mkCId x = CId x
wildCId = CId "_"
showCId (CId x) = x
readCId s = Just (CId s)


pIdent :: RP.ReadP String
pIdent = 
  liftM2 (:) (RP.satisfy isIdentFirst) (RP.munch isIdentRest)
  `mplus`
  do RP.char '\''
     cs <- RP.many1 insideChar
     RP.char '\''
     return cs
--  where
insideChar = RP.readS_to_P $ \s ->
  case s of
    []             -> []
    ('\\':'\\':cs) -> [('\\',cs)]
    ('\\':'\'':cs) -> [('\'',cs)]
    ('\\':cs)      -> []
    ('\'':cs)      -> []
    (c:cs)         -> [(c,cs)]

isIdentFirst c =
  (c == '_') ||
  (c >= 'a' && c <= 'z') ||
  (c >= 'A' && c <= 'Z') ||
  (c >= '\192' && c <= '\255' && c /= '\247' && c /= '\215')
isIdentRest c = 
  (c == '_') ||
  (c == '\'') ||
  (c >= '0' && c <= '9') ||
  (c >= 'a' && c <= 'z') ||
  (c >= 'A' && c <= 'Z') ||
  (c >= '\192' && c <= '\255' && c /= '\247' && c /= '\215')

pExpr :: RP.ReadP PGF2.Expr
pExpr =
  RP.readS_to_P $ \str ->
  unsafePerformIO $
  withText str $ \c_str ->
  alloca  $ \c_pos ->
  mask_ $ do
    c_expr <- pgf_read_expr_ex c_str c_pos unmarshaller
    if c_expr == castPtrToStablePtr nullPtr
      then return []
      else do expr <- deRefStablePtr c_expr
              freeStablePtr c_expr
              pos <- peek c_pos
              size <- ((#peek PgfText, size) c_str) :: IO CSize
              let c_text = castPtr c_str `plusPtr` (#offset PgfText, text)
              s    <- peekUtf8CString pos (c_text `plusPtr` fromIntegral size)
              return [(expr,s)]
