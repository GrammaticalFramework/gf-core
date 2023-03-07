module GF.Command.Abstract(module GF.Command.Abstract,Expr,showExpr,Literal(..),Term) where

import PGF2
import GF.Grammar.Grammar(Term)

type Ident = String

type CommandLine = [Pipe]

type Pipe = [Command]

data Command
   = Command Ident [Option] Argument
   deriving Show

data TransactionCommand
   = CreateFun [Option] Fun Type
   | CreateCat [Option] Cat [Hypo]
   | CreateConcrete [Option] ConcName
   | CreateLin [Option] Fun Term Bool
   | CreateLincat [Option] Cat Term
   | DropFun [Option] Fun
   | DropCat [Option] Cat
   | DropConcrete [Option] ConcName
   | DropLin [Option] Fun
   | DropLincat [Option] Cat
   deriving Show

data Option
  = OOpt Ident
  | OFlag Ident Literal
  deriving (Eq,Ord,Show)

data Argument
  = AExpr Expr
  | ATerm Term
  | ANoArg
  | AMacro Ident
  deriving Show

valIntOpts :: String -> Int -> [Option] -> Int
valIntOpts flag def opts =
  case [v | OFlag f (LInt v) <- opts, f == flag] of
    (v:_) -> fromIntegral v
    _     -> def

valFltOpts :: String -> Double -> [Option] -> Double
valFltOpts flag def opts =
  case [v | OFlag f v <- opts, v <- toFlt v, f == flag] of
    (v:_) -> v
    _     -> def
  where
    toFlt (LInt v) = [fromIntegral v]
    toFlt (LFlt f) = [f]
    toFlt _        = []

valStrOpts :: String -> String -> [Option] -> String
valStrOpts flag def opts =
  case listFlags flag opts of
    v:_ -> valueString v
    _   -> def

maybeIntOpts :: String -> a -> (Int -> a) -> [Option] -> a
maybeIntOpts flag def fn opts =
  case [v | OFlag f (LInt v) <- opts, f == flag] of
    (v:_) -> fn (fromIntegral v)
    _     -> def

maybeStrOpts :: String -> a -> (String -> a) -> [Option] -> a
maybeStrOpts flag def fn opts =
  case listFlags flag opts of
    v:_ -> fn (valueString v)
    _   -> def

listFlags flag opts = [v | OFlag f v <- opts, f == flag]

valueString v =
  case v of
    LInt v -> show v
    LFlt v -> show v
    LStr v -> v

isOpt :: String -> [Option] -> Bool
isOpt o opts = elem (OOpt o) opts

isFlag :: String -> [Option] -> Bool
isFlag o opts = elem o [x | OFlag x _ <- opts]

optsAndFlags :: [Option] -> ([Option],[Option])
optsAndFlags = foldr add ([],[]) where
  add o (os,fs) = case o of
    OOpt _    -> (o:os,fs)
    OFlag _ _ -> (os,o:fs)

prOpt :: Option -> String
prOpt o = case o of
  OOpt i    -> i
  OFlag f x -> f ++ "=" ++ show x

mkOpt :: String -> Option
mkOpt = OOpt

-- abbreviation convention from gf commands
getCommandOp s = case break (=='_') s of
     (a:_,_:b:_) -> [a,b]  -- axx_byy --> ab
     _ -> case s of
       [a,b] -> s          -- ab  --> ab
       a:_ -> [a]          -- axx --> a

