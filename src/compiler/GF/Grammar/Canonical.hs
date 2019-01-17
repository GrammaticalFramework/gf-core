-- | Abstract syntax for canonical GF grammars, i.e. what's left after
-- high-level constructions such as functors and opers have been eliminated
-- by partial evaluation.
module GF.Grammar.Canonical where
import GF.Text.Pretty

-- | A Complete grammar
data Grammar = Grammar Abstract [Concrete] deriving Show

--------------------------------------------------------------------------------
-- ** Abstract Syntax

-- | Abstract Syntax
data Abstract = Abstract ModId [CatDef] [FunDef] deriving Show

data CatDef   = CatDef CatId [CatId]        deriving Show
data FunDef   = FunDef FunId Type           deriving Show
data Type     = Type [TypeBinding] TypeApp  deriving Show
data TypeApp  = TypeApp CatId [Type]        deriving Show

data TypeBinding = TypeBinding VarId Type   deriving Show

--------------------------------------------------------------------------------
-- ** Concreate syntax

-- | Concrete Syntax
data Concrete  = Concrete ModId ModId [ParamDef] [LincatDef] [LinDef]
                 deriving Show

data ParamDef  = ParamDef ParamId [ParamValueDef]
               | ParamAliasDef ParamId LinType
               deriving Show
data LincatDef = LincatDef CatId LinType  deriving Show
data LinDef    = LinDef FunId [VarId] LinValue  deriving Show

-- | Linearization type, RHS of @lincat@
data LinType = FloatType 
             | IntType 
             | ParamType ParamType
             | RecordType [RecordRowType]
             | StrType 
             | TableType LinType LinType 
             | TupleType [LinType]
              deriving (Eq,Ord,Show)

newtype ParamType = ParamTypeId ParamId deriving (Eq,Ord,Show)

-- | Linearization value, RHS of @lin@
data LinValue = ConcatValue LinValue LinValue
              | ErrorValue String
              | FloatConstant Float 
              | IntConstant Int 
              | ParamConstant ParamValue 
              | RecordValue [RecordRowValue]
              | StrConstant String 
              | TableValue LinType [TableRowValue]
---           | VTableValue LinType [LinValue]
              | TupleValue [LinValue]
              | VariantValue [LinValue]
              | VarValue VarValueId
              | PreValue [([String], LinValue)] LinValue
              | Projection LinValue LabelId
              | Selection LinValue LinValue
               deriving (Eq,Show)

data LinPattern = ParamPattern ParamPattern
                | RecordPattern [RecordRow LinPattern]
                | WildPattern
                deriving (Eq,Show)

type ParamValue = Param LinValue
type ParamPattern = Param LinPattern
type ParamValueDef = Param ParamId

data Param arg = Param ParamId [arg] deriving (Eq,Show)

type RecordRowType = RecordRow LinType
type RecordRowValue = RecordRow LinValue

data RecordRow rhs  = RecordRow LabelId rhs  deriving (Eq,Ord,Show)
data TableRowValue  = TableRowValue LinPattern LinValue  deriving (Eq,Show)

-- *** Identifiers in Concrete Syntax

newtype LabelId = LabelId String  deriving (Eq,Ord,Show)
data VarValueId = VarValueId String  deriving (Eq,Show)

-- | Name of param type or param value 
newtype ParamId = ParamId String  deriving (Eq,Ord,Show)

--------------------------------------------------------------------------------
-- ** Used in both Abstract and Concrete Syntax

newtype ModId = ModId String  deriving (Eq,Show)

newtype CatId = CatId String  deriving (Eq,Show)
newtype FunId = FunId String  deriving (Eq,Show)

data VarId = Anonymous | VarId String  deriving Show

--------------------------------------------------------------------------------
-- ** Pretty printing

instance Pretty Grammar where
  pp (Grammar abs cncs) = abs $+$ vcat cncs

instance Pretty Abstract where
  pp (Abstract m cats funs) = "abstract" <+> m <+> "=" <+> "{" $$
                              "cat" <+> fsep cats $$
                              "fun" <+> vcat funs $$
                              "}"

instance Pretty CatDef where
  pp (CatDef c cs) = hsep (c:cs)<>";"

instance Pretty FunDef where
  pp (FunDef f ty) = f <+> ":" <+> ty <>";"

instance Pretty Type where
  pp (Type bs ty) = sep (punctuate " ->" (map pp bs ++ [pp ty]))

instance PPA Type where
  ppA (Type [] (TypeApp c [])) = pp c
  ppA t = parens t

instance Pretty TypeBinding where
  pp (TypeBinding Anonymous (Type [] tapp)) = pp tapp
  pp (TypeBinding Anonymous ty) = parens ty
  pp (TypeBinding (VarId x) ty) = parens (x<+>":"<+>ty)

instance Pretty TypeApp where
 pp (TypeApp c targs) = c<+>hsep (map ppA targs)

instance Pretty VarId where
  pp Anonymous = pp "_"
  pp (VarId x) = pp x

--------------------------------------------------------------------------------

instance Pretty Concrete where
  pp (Concrete cncid absid params lincats lins) =
      "concrete" <+> cncid <+> "of" <+> absid <+> "=" <+> "{" $$
      vcat params $$
      section "lincat" lincats $$
      section "lin" lins $$
      "}"
    where
      section name [] = empty
      section name ds = name <+> vcat (map (<> ";") ds)

instance Pretty ParamDef where
  pp (ParamDef p pvs) = hang ("param"<+> p <+> "=") 4 (punctuate " |" pvs)<>";"
  pp (ParamAliasDef p t) = hang ("oper"<+> p <+> "=") 4 t<>";"

instance PPA arg => Pretty (Param arg) where
  pp (Param p ps) = pp p<+>sep (map ppA ps)

instance PPA arg => PPA (Param arg) where
  ppA (Param p []) = pp p
  ppA pv = parens pv

instance Pretty LincatDef where
  pp (LincatDef c lt) = hang (c <+> "=") 4 lt

instance Pretty LinType where
 pp lt = case lt of
           FloatType -> pp "Float"
           IntType -> pp "Int"
           ParamType pt -> pp pt
           RecordType rs -> block rs
           StrType -> pp "Str"
           TableType pt lt -> pt <+> "=>" <+> lt
           TupleType lts -> "<"<>punctuate "," lts<>">"

instance RhsSeparator LinType  where rhsSep _ = pp ":"

instance Pretty ParamType where
  pp (ParamTypeId p) = pp p

instance Pretty LinDef where
  pp (LinDef f xs lv) = hang (f<+>hsep xs<+>"=") 4 lv

instance Pretty LinValue where
  pp lv = case lv of
            ConcatValue v1 v2 -> sep [v1 <+> "++",pp v2]
            ErrorValue s -> "Predef.error"<+>doubleQuotes s
            Projection lv l -> ppA lv<>"."<>l
            Selection tv pv -> ppA tv<>"!"<>ppA pv
            VariantValue vs -> "variants"<+>block vs
            _ -> ppA lv

instance PPA LinValue where
  ppA lv = case lv of
             FloatConstant f -> pp f
             IntConstant n -> pp n
             ParamConstant pv -> ppA pv
             RecordValue [] -> pp "<>"
             RecordValue rvs -> block rvs
             PreValue alts def ->
               "pre"<+>block (map alt alts++["_"<+>"=>"<+>def])
               where
                 alt (ss,lv) = hang (hcat (punctuate "|" (map doubleQuotes ss)))
                                    2 ("=>"<+>lv)
             StrConstant s -> doubleQuotes s -- hmm
             TableValue _ tvs -> "table"<+>block tvs
--           VTableValue t ts -> "table"<+>t<+>brackets (semiSep ts)
             TupleValue lvs -> "<"<>punctuate "," lvs<>">"
             VarValue v -> pp v
             _ -> parens lv

instance RhsSeparator LinValue where rhsSep _ = pp "="

instance Pretty LinPattern where
  pp p =
    case p of
      ParamPattern pv -> pp pv
      _ -> ppA p

instance PPA LinPattern where
  ppA p =
    case p of
      RecordPattern r -> block r
      WildPattern     -> pp "_"                
      _ -> parens p

instance RhsSeparator LinPattern where rhsSep _ = pp "="

instance RhsSeparator rhs => Pretty (RecordRow rhs) where
  pp (RecordRow l v) = hang (l<+>rhsSep v) 2 v

instance Pretty TableRowValue where
  pp (TableRowValue l v) = hang (l<+>"=>") 2 v

--------------------------------------------------------------------------------
instance Pretty ModId where pp (ModId s) = pp s
instance Pretty CatId where pp (CatId s) = pp s
instance Pretty FunId where pp (FunId s) = pp s
instance Pretty LabelId where pp (LabelId s) = pp s
instance Pretty ParamId where pp = ppA
instance PPA    ParamId where ppA (ParamId s) = pp s
instance Pretty VarValueId where pp (VarValueId s) = pp s

--------------------------------------------------------------------------------
-- | Pretty print atomically (i.e. wrap it in parentheses if necessary)
class Pretty a => PPA a where ppA :: a -> Doc

class Pretty rhs => RhsSeparator rhs where rhsSep :: rhs -> Doc

semiSep xs = punctuate ";" xs
block xs = braces (semiSep xs)