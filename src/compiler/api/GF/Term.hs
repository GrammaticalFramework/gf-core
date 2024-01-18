module GF.Term (renameSourceTerm, inferLType, checkLType, normalForm) where

import GF.Compile.Rename
import GF.Compile.Compute.Concrete
import GF.Compile.TypeCheck.ConcreteNew
