module PGF2.Expr(Var, Cat, Fun,
                 BindType(..), Literal(..), Expr(..),
                 Type(..), Hypo,
                 Patt(..), Equation(..),

                 mkAbs,    unAbs,
                 mkApp,    unApp, unapply,
                 mkStr,    unStr,
                 mkInt,    unInt,
                 mkDouble, unDouble,
                 mkFloat,  unFloat,
                 mkMeta,   unMeta,

                 mkType, unType,
                 mkHypo, mkDepHypo, mkImplHypo

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
 | LInt Integer                     -- ^ integer constant
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


mkAbs :: BindType -> Var -> Expr -> Expr
mkAbs = EAbs

unAbs :: Expr -> Maybe (BindType, Var, Expr)
unAbs (EAbs bt x e) = Just (bt,x,e)
unAbs (ETyped e ty) = unAbs e
unAbs (EImplArg e)  = unAbs e
unAbs _             = Nothing

-- | Constructs an expression by applying a function to a list of expressions
mkApp :: Fun -> [Expr] -> Expr
mkApp f es = foldl EApp (EFun f) es

-- | Decomposes an expression into application of function
unApp :: Expr -> Maybe (Fun,[Expr])
unApp e = case unapply e of
  (EFun f,es) -> Just (f,es)
  _           -> Nothing

-- | Decomposes an expression into an application of a constructor such as a constant or a metavariable
unapply :: Expr -> (Expr,[Expr])
unapply = extract []
  where
    extract es f@(EFun _)   = (f,es)
    extract es (EApp e1 e2) = extract (e2:es) e1
    extract es (ETyped e ty)= extract es e
    extract es (EImplArg e) = extract es e
    extract es h            = (h,es)

-- | Constructs an expression from string literal
mkStr :: String -> Expr
mkStr s = ELit (LStr s)

-- | Decomposes an expression into string literal
unStr :: Expr -> Maybe String
unStr (ELit (LStr s)) = Just s
unStr (ETyped e ty)   = unStr e
unStr (EImplArg e)    = unStr e
unStr _               = Nothing

-- | Constructs an expression from integer literal
mkInt :: Integer -> Expr
mkInt i = ELit (LInt i)

-- | Decomposes an expression into integer literal
unInt :: Expr -> Maybe Integer
unInt (ELit (LInt i)) = Just i
unInt (ETyped e ty)   = unInt e
unInt (EImplArg e)    = unInt e
unInt _               = Nothing

-- | Constructs an expression from real number literal
mkDouble :: Double -> Expr
mkDouble f = ELit (LFlt f)

-- | Decomposes an expression into real number literal
unDouble :: Expr -> Maybe Double
unDouble (ELit (LFlt f)) = Just f
unDouble (ETyped e ty)   = unDouble e
unDouble (EImplArg e)    = unDouble e
unDouble _               = Nothing

mkFloat = mkDouble
unFloat = unDouble

-- | Constructs an expression which is meta variable
mkMeta :: Int -> Expr
mkMeta i = EMeta i

-- | Checks whether an expression is a meta variable
unMeta :: Expr -> Maybe Int
unMeta (EMeta i)     = Just i
unMeta (ETyped e ty) = unMeta e
unMeta (EImplArg e)  = unMeta e
unMeta _             = Nothing


-- | creates a type from list of hypothesises, category and 
-- list of arguments for the category. The operation 
-- @mkType [h_1,...,h_n] C [e_1,...,e_m]@ will create 
-- @h_1 -> ... -> h_n -> C e_1 ... e_m@
mkType :: [Hypo] -> Cat -> [Expr] -> Type
mkType hyps cat args = DTyp hyps cat args

-- | creates hypothesis for non-dependent type i.e. A
mkHypo :: Type -> Hypo
mkHypo ty = (Explicit,"_",ty)

-- | creates hypothesis for dependent type i.e. (x : A)
mkDepHypo :: Var -> Type -> Hypo
mkDepHypo x ty = (Explicit,x,ty)

-- | creates hypothesis for dependent type with implicit argument i.e. ({x} : A)
mkImplHypo :: Var -> Type -> Hypo
mkImplHypo x ty = (Implicit,x,ty)

unType :: Type -> ([Hypo], Cat, [Expr])
unType (DTyp hyps cat es) = (hyps, cat, es)
