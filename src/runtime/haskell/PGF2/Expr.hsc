#include <pgf/pgf.h>

module PGF2.Expr(Var, Cat, Fun,
                 BindType(..), Literal(..), Expr(..),
                 Type(..), Hypo,
                 Patt(..), Equation(..)
                ) where

type Var = String -- ^ Name of syntactic category
type Cat = String -- ^ Name of syntactic category
type Fun = String -- ^ Name of function

type MetaId = Int

data BindType = 
    Explicit
  | Implicit
  deriving (Eq,Ord,Show)

data Literal =
   LStr String                      -- ^ string constant
 | LInt Int                         -- ^ integer constant
 | LFlt Double                      -- ^ floating point constant
 deriving (Eq,Ord,Show)

-- | An expression in the abstract syntax of the grammar. It could be
-- both parameter of a dependent type or an abstract syntax tree for
-- for some sentence.
data Expr =
   EAbs BindType Var Expr           -- ^ lambda abstraction
 | EApp Expr Expr                   -- ^ application
 | ELit Literal                     -- ^ literal
 | EMeta  {-# UNPACK #-} !MetaId    -- ^ meta variable
 | EFun   Fun                       -- ^ function or data constructor
 | EVar   {-# UNPACK #-} !Int       -- ^ variable with de Bruijn index
 | ETyped Expr Type                 -- ^ local type signature
 | EImplArg Expr                    -- ^ implicit argument in expression
  deriving (Eq,Ord,Show)

-- | To read a type from a 'String', use 'readType'.
data Type =
   DTyp [Hypo] Cat [Expr]
  deriving (Eq,Ord,Show)

-- | 'Hypo' represents a hypothesis in a type i.e. in the type A -> B, A is the hypothesis
type Hypo = (BindType,Var,Type)


-- | The pattern is used to define equations in the abstract syntax of the grammar.
data Patt =
   PApp Fun [Patt]                  -- ^ application. The identifier should be constructor i.e. defined with 'data'
 | PLit Literal                     -- ^ literal
 | PVar Var                         -- ^ variable
 | PAs  Var Patt                    -- ^ variable@pattern
 | PWild                            -- ^ wildcard
 | PImplArg Patt                    -- ^ implicit argument in pattern
 | PTilde Expr
  deriving Show

-- | The equation is used to define lambda function as a sequence
-- of equations with pattern matching. The list of 'Expr' represents
-- the patterns and the second 'Expr' is the function body for this
-- equation.
data Equation =
   Equ [Patt] Expr
  deriving Show
