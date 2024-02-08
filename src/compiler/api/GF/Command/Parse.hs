module GF.Command.Parse(readCommandLine, readTransactionCommand, pCommand) where

import PGF(pExpr,pIdent)
import PGF2(BindType(..),readType,readContext)
import GF.Infra.Ident(identS)
import GF.Grammar.Grammar(Term(Abs))
import GF.Grammar.Parser(runPartial,pTerm)
import GF.Command.Abstract

import Data.Char(isDigit,isSpace)
import Control.Monad(liftM2)
import Text.ParserCombinators.ReadP

readCommandLine :: String -> Maybe CommandLine
readCommandLine s =
    case [x | (x,cs) <- readP_to_S pCommandLine s, all isSpace cs] of
      [x] -> Just x
      _   -> Nothing

pCommandLine =
  (skipSpaces >> char '-' >> char '-' >> pTheRest >> return [])   -- comment
  <++
  (sepBy (skipSpaces >> pPipe) (skipSpaces >> char ';'))

pPipe = sepBy1 (skipSpaces >> pCommand) (skipSpaces >> char '|')

pCommand = (do
  cmd  <- pIdent <++ (char '%' >> fmap ('%':) pIdent)
  skipSpaces
  opts <- sepBy pOption skipSpaces
  arg  <- if getCommandOp cmd `elem` ["cc","sd","so"] then pArgTerm else pArgument
  return (Command cmd opts arg)
  )
    <++ (do
  char '?'
  skipSpaces
  c <- pSystemCommand
  return (Command "sp" [OFlag "command" (LStr c)] ANoArg)
  )

readTransactionCommand :: String -> Maybe TransactionCommand
readTransactionCommand s =
    case [x | (x,cs) <- readP_to_S pTransactionCommand s, all isSpace cs] of
      [x] -> Just x
      _   -> Nothing

pTransactionCommand = do
  skipSpaces
  cmd <- pIdent
  skipSpaces
  opts <- sepBy pOption skipSpaces
  skipSpaces
  kwd <- pIdent
  skipSpaces
  case kwd of
    "fun" | take 1 cmd == "c" -> do
               f <- pIdent
               skipSpaces
               char ':'
               skipSpaces
               ty <- readS_to_P (\s -> case readType s of
                                         Just ty -> [(ty,"")]
                                         Nothing -> [])
               return (CreateFun opts f ty)
          | take 1 cmd == "d" -> do
                f <- pIdent
                return (DropFun opts f)
    "cat" | take 1 cmd == "c" -> do
               c <- pIdent
               skipSpaces
               ctxt <- readS_to_P (\s -> case readContext s of
                                           Just ty -> [(ty,"")]
                                           Nothing -> [])
               return (CreateCat opts c ctxt)
          | take 1 cmd == "d" -> do
                c <- pIdent
                return (DropCat opts c)
    "concrete"
          | take 1 cmd == "c" -> do
               name <- pIdent
               return (CreateConcrete opts name)
          | take 1 cmd == "d" -> do
               name <- pIdent
               return (DropConcrete opts name)
    "lin" | elem (take 1 cmd) ["c","a"] -> do
               f <- pIdent
               skipSpaces
               args <- sepBy pIdent skipSpaces
               skipSpaces
               char '='
               skipSpaces
               t  <- readS_to_P (\s -> case runPartial pTerm s of
                                         Right (s,t) -> [(t,s)]
                                         _ -> [])
               return (CreateLin opts f (foldr (Abs Explicit . identS) t args) (take 1 cmd == "a"))
          | take 1 cmd == "d" -> do
                f <- pIdent
                return (DropLin opts f)
    "lincat"
          | take 1 cmd == "c" -> do
               f <- pIdent
               skipSpaces
               char '='
               skipSpaces
               t  <- readS_to_P (\s -> case runPartial pTerm s of
                                         Right (s,t) -> [(t,s)]
                                         _ -> [])
               return (CreateLincat opts f t)
          | take 1 cmd == "d" -> do
                f <- pIdent
                return (DropLincat opts f)
    _     -> pfail

pOption = do
  char '-'
  flg <- pIdent
  option (OOpt flg) (fmap (OFlag flg) (char '=' >> pValue))

pValue = do
  fmap LInt (readS_to_P reads)
  <++
  fmap LFlt (readS_to_P reads)
  <++
  fmap LStr (readS_to_P reads)
  <++
  fmap LStr pFilename

pFilename = liftM2 (:) (satisfy isFileFirst) (munch (not . isSpace)) where
  isFileFirst c = not (isSpace c) && not (isDigit c)

pArgument =
  option ANoArg
    (fmap AExpr pExpr
              <++
    (skipSpaces >> char '%' >> fmap AMacro pIdent))

pArgTerm = ATerm `fmap` readS_to_P sTerm
  where
    sTerm s = case runPartial pTerm s of
                Right (s,t) -> [(t,s)]
                _ -> []

pSystemCommand =
    (char '"' >> (manyTill (pEsc <++ get) (char '"')))
      <++
    pTheRest
  where
    pEsc = char '\\' >> get

pTheRest = munch (const True)
