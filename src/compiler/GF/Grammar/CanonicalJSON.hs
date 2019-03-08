module GF.Grammar.CanonicalJSON (
  encodeJSON
  ) where

import Text.JSON
import GF.Grammar.Canonical


encodeJSON :: FilePath -> Grammar -> IO ()
encodeJSON fpath g = writeFile fpath (encode g)


-- in general we encode grammars using JSON objects/records,
-- except for newtypes/coercions/direct values

-- the top-level definitions use normal record labels,
-- but recursive types/values/ids use labels staring with a "."

instance JSON Grammar where
  showJSON (Grammar abs cncs) = makeObj [("abstract", showJSON abs), ("concretes", showJSON cncs)]


--------------------------------------------------------------------------------
-- ** Abstract Syntax

instance JSON Abstract where
  showJSON (Abstract absid flags cats funs) 
    = makeObj [("abs", showJSON absid),
               ("flags", showJSON flags),
               ("cats", showJSON cats),
               ("funs", showJSON funs)]

instance JSON CatDef where
  -- non-dependent categories are encoded as simple strings:
  showJSON (CatDef c []) = showJSON c
  showJSON (CatDef c cs) = makeObj [("cat", showJSON c), ("args", showJSON cs)]

instance JSON FunDef where
  showJSON (FunDef f ty) = makeObj [("fun", showJSON f), ("type", showJSON ty)]

instance JSON Type where
  showJSON (Type bs ty) = makeObj [(".args", showJSON bs), (".result", showJSON ty)]

instance JSON TypeApp where
  -- non-dependent categories are encoded as simple strings:
  showJSON (TypeApp c [])   = showJSON c
  showJSON (TypeApp c args) = makeObj [(".cat", showJSON c), (".args", showJSON args)]

instance JSON TypeBinding where
  -- non-dependent categories are encoded as simple strings:
  showJSON (TypeBinding Anonymous (Type [] (TypeApp c []))) = showJSON c
  showJSON (TypeBinding x ty) = makeObj [(".var", showJSON x), (".type", showJSON ty)]


--------------------------------------------------------------------------------
-- ** Concrete syntax

instance JSON Concrete where
  showJSON (Concrete cncid absid flags params lincats lins) 
    = makeObj [("cnc", showJSON cncid),
               ("abs", showJSON absid),
               ("flags", showJSON flags),
               ("params", showJSON params),
               ("lincats", showJSON lincats),
               ("lins", showJSON lins)]

instance JSON ParamDef where
  showJSON (ParamDef      p pvs) = makeObj [("param", showJSON p), ("values", showJSON pvs)]
  showJSON (ParamAliasDef p t)   = makeObj [("param", showJSON p), ("alias", showJSON t)]

instance JSON LincatDef where
  showJSON (LincatDef c lt) = makeObj [("cat", showJSON c), ("lintype", showJSON lt)]

instance JSON LinDef where
  showJSON (LinDef f xs lv) = makeObj [("fun", showJSON f), ("args", showJSON xs), ("lin", showJSON lv)]

instance JSON LinType where
  -- the basic types (Str, Float, Int) are encoded as strings:
  showJSON (StrType)         = showJSON "Str"
  showJSON (FloatType)       = showJSON "Float"
  showJSON (IntType)         = showJSON "Int"
  -- parameters are also encoded as strings:
  showJSON (ParamType pt)    = showJSON pt
  -- tables/tuples are encoded as JSON objects:
  showJSON (TableType pt lt) = makeObj [(".tblarg", showJSON pt), (".tblval", showJSON lt)]
  showJSON (TupleType lts)   = makeObj [(".tuple",  showJSON lts)]
  -- records are encoded as records:
  showJSON (RecordType rows) = showJSON rows

instance JSON LinValue where
  showJSON (LiteralValue  l ) = showJSON l
  -- concatenation is encoded as a JSON array:
  showJSON (ConcatValue v v') = showJSON [showJSON v, showJSON v']
  -- most values are encoded as JSON objects:
  showJSON (ParamConstant pv) = makeObj [(".param",    showJSON pv)]
  showJSON (PredefValue   p ) = makeObj [(".predef",   showJSON p)]
  showJSON (TableValue t tvs) = makeObj [(".tblarg",   showJSON t), (".tblrows", showJSON tvs)]
  showJSON (TupleValue   lvs) = makeObj [(".tuple",    showJSON lvs)]
  showJSON (VarValue      v ) = makeObj [(".var",      showJSON v)]
  showJSON (ErrorValue    s ) = makeObj [(".error",    showJSON s)]
  showJSON (Projection lv l ) = makeObj [(".project",  showJSON lv), (".label", showJSON l)]
  showJSON (Selection  tv pv) = makeObj [(".select",   showJSON tv), (".key", showJSON pv)]
  showJSON (VariantValue  vs) = makeObj [(".variants", showJSON vs)]
  showJSON (PreValue pre def) = makeObj [(".pre",      showJSON pre),(".default", showJSON def)]
  -- records are encoded directly as JSON records:
  showJSON (RecordValue rows) = showJSON rows

instance JSON LinLiteral where
  -- basic values (Str, Float, Int) are encoded as JSON strings/numbers:
  showJSON (StrConstant   s) = showJSON s
  showJSON (FloatConstant f) = showJSON f
  showJSON (IntConstant   n) = showJSON n

instance JSON LinPattern where
  -- wildcards and patterns without arguments are encoded as strings:
  showJSON (WildPattern) = showJSON "_"
  showJSON (ParamPattern (Param p [])) = showJSON p
  -- complex patterns are encoded as JSON objects:
  showJSON (ParamPattern pv) = showJSON pv
  -- and records as records:
  showJSON (RecordPattern r) = showJSON r

instance JSON arg => JSON (Param arg) where
  -- parameters without arguments are encoded as strings:
  showJSON (Param p [])   = showJSON p
  showJSON (Param p args) = makeObj [(".paramid", showJSON p), (".args", showJSON args)]

instance JSON a => JSON (RecordRow a) where
  -- record rows and lists of record rows are both encoded as JSON records (i.e., objects)
  showJSON  row  = showJSONs [row]
  showJSONs rows = makeObj (map toRow rows)
    where toRow (RecordRow (LabelId lbl) val) = (lbl, showJSON val)


instance JSON TableRowValue where
  showJSON (TableRowValue l v) = makeObj [(".pattern", showJSON l), (".value", showJSON v)]


-- *** Identifiers in Concrete Syntax

instance JSON PredefId   where showJSON (PredefId    s) = showJSON s
instance JSON LabelId    where showJSON (LabelId     s) = showJSON s
instance JSON VarValueId where showJSON (VarValueId  s) = showJSON s
instance JSON ParamId    where showJSON (ParamId     s) = showJSON s
instance JSON ParamType  where showJSON (ParamTypeId s) = showJSON s

--------------------------------------------------------------------------------
-- ** Used in both Abstract and Concrete Syntax

instance JSON ModId where showJSON (ModId s) = showJSON s
instance JSON CatId where showJSON (CatId s) = showJSON s
instance JSON FunId where showJSON (FunId s) = showJSON s

instance JSON VarId where
  -- the anonymous variable is the underscore:
  showJSON Anonymous = showJSON "_"
  showJSON (VarId x) = showJSON x

instance JSON QualId where
  showJSON (Qual (ModId m) n) = showJSON (m++"."++n)
  showJSON (Unqual n) = showJSON n

instance JSON Flags where
  -- flags are encoded directly as JSON records (i.e., objects):
  showJSON (Flags fs) = makeObj [(f, showJSON v) | (f, v) <- fs]

instance JSON FlagValue where
  -- flag values are encoded as basic JSON types:
  showJSON (Str s) = showJSON s
  showJSON (Int i) = showJSON i
  showJSON (Flt f) = showJSON f

