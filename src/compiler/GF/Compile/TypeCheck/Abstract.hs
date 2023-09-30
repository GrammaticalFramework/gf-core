----------------------------------------------------------------------
-- |
-- Module      : TypeCheck
-- Maintainer  : AR
-- Stability   : (stable)
-- Portability : (portable)
--
-- > CVS $Date: 2005/09/15 16:22:02 $
-- > CVS $Author: aarne $
-- > CVS $Revision: 1.16 $
--
-- (Description of the module)
-----------------------------------------------------------------------------

module GF.Compile.TypeCheck.Abstract (-- * top-level type checking functions; TC should not be called directly.
    checkContext,
    checkTyp,
    checkDef,
    checkConstrs,
   ) where

import GF.Data.Operations

import GF.Infra.CheckM
import GF.Grammar
import GF.Grammar.Lookup
import GF.Grammar.Unify
--import GF.Compile.Refresh
--import GF.Compile.Compute.Abstract
import GF.Compile.TypeCheck.TC

import GF.Text.Pretty
import Debug.Trace (traceM)
--import Control.Monad (foldM, liftM, liftM2)

-- | invariant way of creating TCEnv from context
initTCEnv gamma =
  (length gamma,[(x,VGen i x) | ((x,_),i) <- zip gamma [0..]], gamma)

-- interface to TC type checker

type2val :: Type -> Val
type2val = VClos []

type2nval :: Type -> NotVal
type2nval = NVClos []

cont2exp :: Context -> Term
cont2exp c = mkProd c eType [] -- to check a context

cont2val :: Context -> Val
cont2val = type2val . cont2exp

-- some top-level batch-mode checkers for the compiler

justTypeCheck :: SourceGrammar -> Term -> Val -> Err Constraints
justTypeCheck gr e v = do
  (_,constrs0) <- checkExp (grammar2theory gr) (initTCEnv []) e v
  (constrs1,_) <- unifyVal constrs0
  return $ filter notJustMeta constrs1

notJustMeta (c,k) = case (c,k) of
    (VClos g1 (Meta m1), VClos g2 (Meta m2)) -> False
    _ -> True

grammar2theory :: SourceGrammar -> Theory
grammar2theory gr (m,f) = case lookupFunType gr m f of
  Ok t -> case lookupAbsDef gr m f of -- TODO: Don't lookup twice
            Ok (Just n, Just eqs) -> return (type2val t, Just (n, eqs))
            _ -> return (type2val t, Nothing)
  Bad s -> case lookupCatContext gr m f of
    Ok cont -> return (cont2val cont,Nothing) 
    _ -> Bad s

checkContext :: SourceGrammar -> Context -> [Message]
checkContext st = checkTyp st . cont2exp

checkTyp :: SourceGrammar -> Type -> [Message]
checkTyp gr typ = err (\x -> [pp x]) ppConstrs $ justTypeCheck gr typ vType

checkDef :: SourceGrammar -> Fun -> Type -> Equation -> [Message]
checkDef gr (m,fun) typ eq = err (\x -> [pp x]) ppConstrs $ do
  traceM . render $ "\nchecking def: " <+> pp fun <+> ":" <+> pp typ
                   $$ "with equation:" <+> pp fun <+> hsep [ppPatt Unqualified 2 p | p <- fst eq] <+> "=>" <+> snd eq
  (b,cs) <- checkBranch (grammar2theory gr) (initTCEnv []) eq (type2nval typ)
  traceM . render $ "\ngot branches" {- <+> pp (show $ snd b) -} <+> ": with :" <+> pp (vcat $ fst b)
        $$ "with constraints:" <+> vcat [ppValue Unqualified 0 a <+> " = " <+> ppValue Unqualified 0 b | (a,b) <- cs]
  (constrs,_) <- unifyVal cs
  return $ filter notJustMeta constrs

checkConstrs :: SourceGrammar -> Cat -> [Ident] -> [String]
checkConstrs gr cat _ = [] ---- check constructors!
