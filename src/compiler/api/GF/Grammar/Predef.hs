----------------------------------------------------------------------
-- |
-- Module      : GF.Grammar.Predef
-- Maintainer  : kr.angelov
-- Stability   : (stable)
-- Portability : (portable)
--
-- Predefined identifiers and labels which the compiler knows
----------------------------------------------------------------------

module GF.Grammar.Predef where

import GF.Infra.Ident(Ident,identS,moduleNameS)

cType = identS "Type"
cPType = identS "PType"
cTok = identS "Tok"
cStr = identS "Str"
cStrs = identS "Strs"
cPredefAbs = moduleNameS "PredefAbs"
cPredefCnc = moduleNameS "PredefCnc"
cPredef = moduleNameS "Predef"
cInt = identS "Int"
cFloat = identS "Float"
cString = identS "String"
cInts = identS "Ints"
cPBool = identS "PBool"
cBool = identS "Bool"
cErrorType = identS "Error"
cOverload = identS "overload"
cNonExist = identS "nonExist"
cBIND = identS "BIND"
cSOFT_BIND = identS "SOFT_BIND"
cSOFT_SPACE = identS "SOFT_SPACE"
cCAPIT = identS "CAPIT"
cALL_CAPIT = identS "ALL_CAPIT"
cMarkup = identS "Markup"

isPredefCat :: Ident -> Bool
isPredefCat c = elem c [cInt,cString,cFloat]

cPTrue  = identS "PTrue"
cPFalse = identS "PFalse"
cTrue  = identS "True"
cFalse = identS "False"
cLength = identS "length"
cDrop = identS "drop"
cTake = identS "take"
cTk = identS "tk"
cDp = identS "dp"
cToUpper = identS "toUpper"
cToLower = identS "toLower"
cIsUpper = identS "isUpper"
cEqStr = identS "eqStr"
cEqVal = identS "eqVal"
cOccur = identS "occur"
cOccurs = identS "occurs"
cEqInt = identS "eqInt"
cLessInt = identS "lessInt"
cPlus = identS "plus"
cShow = identS "show"
cRead = identS "read"
cToStr = identS "toStr"
cMapStr = identS "mapStr"
cError = identS "error"

-- * Used in the delimited continuations
cConcat = identS "concat"
cConcat' = identS "concat'"
cOne = identS "one"
cSelect = identS "select"
cFilter = identS "filter"
cDefault = identS "default"
cList = identS "list"
cLen = identS "len"
cConst = identS "const"

cp1 = identS "p1"
cp2 = identS "p2"
