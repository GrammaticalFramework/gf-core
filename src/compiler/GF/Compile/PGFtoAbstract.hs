-- | Extract the abstract syntax from a PGF and convert to it
-- the AST for canonical GF grammars
module GF.Compile.PGFtoAbstract(abstract2canonical) where
import qualified Data.Map as M
import PGF(CId,mkCId,showCId,wildCId,unType,abstractName)
import PGF.Internal(abstract,cats,funs)
import GF.Grammar.Canonical


abstract2canonical pgf = Abstract (gId (abstractName pgf)) cs fs
  where
    abstr = abstract pgf
    cs = [CatDef (gId c) (convHs' hs) |
            (c,(hs,_,_)) <- M.toList (cats abstr),
            c `notElem` predefCat]
    fs = [FunDef (gId f) (convT ty) | (f,(ty,ar,_,_)) <- M.toList (funs abstr)]

predefCat = map mkCId ["Float","Int","String"]

convHs' = map convH'
convH' (bt,name,ty) =
  case unType ty of
    ([],name,[]) -> gId name -- !!

convT t =
  case unType t of
    (hypos,name,[]) -> Type (convHs hypos) (TypeApp (gId name) []) -- !!

convHs = map convH

convH (bt,name,ty) = TypeBinding (gId name) (convT ty)

--------------------------------------------------------------------------------

class FromCId i where gId :: CId -> i

instance FromCId FunId where gId = FunId . showCId
instance FromCId CatId where gId = CatId . showCId
instance FromCId ModId where gId = ModId . showCId

instance FromCId VarId where
  gId i = if i==wildCId then Anonymous else VarId (showCId i)
