module PGF ( PGF2.PGF, readPGF
           , abstractName

           , CId, mkCId, wildCId, showCId, readCId

           , PGF2.categories, PGF2.categoryContext, PGF2.startCat
           , functions, functionsByCat

           , PGF2.Expr(..), PGF2.Literal(..), Tree
           , PGF2.readExpr
           , PGF2.mkAbs,    PGF2.unAbs
           , PGF2.mkApp,    PGF2.unApp, PGF2.unapply
           , PGF2.mkStr,    PGF2.unStr
           , PGF2.mkInt,    PGF2.unInt
           , PGF2.mkDouble, PGF2.unDouble
           , PGF2.mkFloat,  PGF2.unFloat
           , PGF2.mkMeta,   PGF2.unMeta

           , PGF2.Type(..), PGF2.Hypo
           , PGF2.mkType, PGF2.unType
           , PGF2.mkHypo, PGF2.mkDepHypo, PGF2.mkImplHypo

           , PGF2.PGFError(..)
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
