module GF.Compile.Compute.Value where

import Data.STRef
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.ST
import Control.Applicative

import GF.Grammar.Grammar(MetaId,Term,Label)
import PGF2(BindType)
import GF.Infra.Ident(Ident)

data ThunkState s
  = Unevaluated (Env s) Term
  | Evaluated (Value s)
  | Unbound {-# UNPACK #-} !MetaId

type Thunk s = STRef s (ThunkState s)
type Env s = [(Ident,Thunk s)]

data Value s
  = VApp Ident [Thunk s]
  | VMeta (Thunk s) (Env s) [Thunk s]
  | VGen  {-# UNPACK #-} !Int [Thunk s]
  | VClosure (Env s) Term
  | VR [(Label, Thunk s)]
  | VP (Value s) Label
  | VStr String
  | VC [Value s]
