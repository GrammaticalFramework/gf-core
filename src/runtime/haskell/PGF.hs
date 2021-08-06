module PGF ( PGF2.PGF, readPGF
           , abstractName

           , CId, mkCId, wildCId, showCId, readCId

           , categories
           , functions, functionsByCat

           , PGF2.Expr(..), PGF2.Literal(..), Tree

           , PGF2.Type, PGF2.Hypo
           ) where

import qualified PGF2 as PGF2

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
