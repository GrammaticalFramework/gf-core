module GF.Grammar.CanonicalJSON (
  encodeJSON
  ) where

import Text.JSON
import Control.Applicative ((<|>))
import Data.Ratio (denominator, numerator)
import GF.Grammar.Canonical


encodeJSON :: FilePath -> Grammar -> IO ()
encodeJSON fpath g = writeFile fpath (encode g)


-- in general we encode grammars using JSON objects/records,
-- except for newtypes/coercions/direct values

-- the top-level definitions use normal record labels,
-- but recursive types/values/ids use labels staring with a "."

instance JSON Grammar where
  showJSON (Grammar abs cncs) = makeObj [("abstract", showJSON abs), ("concretes", showJSON cncs)]

  readJSON o = Grammar <$> o!"abstract" <*> o!"concretes"


--------------------------------------------------------------------------------
-- ** Abstract Syntax

instance JSON Abstract where
  showJSON (Abstract absid flags cats funs) 
    = makeObj [("abs", showJSON absid),
               ("flags", showJSON flags),
               ("cats", showJSON cats),
               ("funs", showJSON funs)]

  readJSON o = Abstract
    <$> o!"abs"
    <*>(o!"flags" <|> return (Flags []))
    <*> o!"cats"
    <*> o!"funs"

instance JSON CatDef where
  -- non-dependent categories are encoded as simple strings:
  showJSON (CatDef c []) = showJSON c
  showJSON (CatDef c cs) = makeObj [("cat", showJSON c), ("args", showJSON cs)]

  readJSON o = CatDef <$> readJSON o <*> return []
    <|>        CatDef <$> o!"cat" <*> o!"args"

instance JSON FunDef where
  showJSON (FunDef f ty) = makeObj [("fun", showJSON f), ("type", showJSON ty)]

  readJSON o = FunDef <$> o!"fun" <*> o!"type"

instance JSON Type where
  showJSON (Type bs ty) = makeObj [(".args", showJSON bs), (".result", showJSON ty)]

  readJSON o = Type <$> o!".args" <*> o!".result"

instance JSON TypeApp where
  -- non-dependent categories are encoded as simple strings:
  showJSON (TypeApp c [])   = showJSON c
  showJSON (TypeApp c args) = makeObj [(".cat", showJSON c), (".args", showJSON args)]

  readJSON o = TypeApp <$> readJSON o <*> return []
    <|>        TypeApp <$> o!".cat" <*> o!".args"

instance JSON TypeBinding where
  -- non-dependent categories are encoded as simple strings:
  showJSON (TypeBinding Anonymous (Type [] (TypeApp c []))) = showJSON c
  showJSON (TypeBinding x ty) = makeObj [(".var", showJSON x), (".type", showJSON ty)]

  readJSON o = do c <- readJSON o
                  return (TypeBinding Anonymous (Type [] (TypeApp c [])))
           <|> TypeBinding <$> o!".var" <*> o!".type"


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

  readJSON o = Concrete
    <$> o!"cnc"
    <*> o!"abs"
    <*>(o!"flags" <|> return (Flags []))
    <*> o!"params"
    <*> o!"lincats"
    <*> o!"lins"

instance JSON ParamDef where
  showJSON (ParamDef      p pvs) = makeObj [("param", showJSON p), ("values", showJSON pvs)]
  showJSON (ParamAliasDef p t)   = makeObj [("param", showJSON p), ("alias", showJSON t)]

  readJSON o = ParamDef      <$> o!"param" <*> o!"values"
    <|>        ParamAliasDef <$> o!"param" <*> o!"alias"

instance JSON LincatDef where
  showJSON (LincatDef c lt) = makeObj [("cat", showJSON c), ("lintype", showJSON lt)]

  readJSON o = LincatDef <$> o!"cat" <*> o!"lintype"

instance JSON LinDef where
  showJSON (LinDef f xs lv) = makeObj [("fun", showJSON f), ("args", showJSON xs), ("lin", showJSON lv)]

  readJSON o = LinDef <$> o!"fun" <*> o!"args" <*> o!"lin"

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

  readJSON o = do "Str"   <- readJSON o; return StrType
    <|>        do "Float" <- readJSON o; return FloatType
    <|>        do "Int"   <- readJSON o; return IntType
    <|>        do  ptype  <- readJSON o; return (ParamType ptype)
    <|>        TableType  <$> o!".tblarg" <*> o!".tblval"
    <|>        TupleType  <$> o!".tuple"
    <|>        RecordType <$> readJSON o

instance JSON LinValue where
  showJSON (LiteralValue  l ) = showJSON l
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
  -- concatenation is encoded as a JSON array:
  showJSON v@(ConcatValue _ _) = showJSON (flatten v [])
    where flatten (ConcatValue v v') = flatten v . flatten v'
          flatten  v                 = (v :)

  readJSON o = LiteralValue  <$> readJSON o
    <|>        ParamConstant <$> o!".param"
    <|>        PredefValue   <$> o!".predef"
    <|>        TableValue    <$> o!".tblarg"  <*> o!".tblrows"
    <|>        TupleValue    <$> o!".tuple"
    <|>        VarValue      <$> o!".var"
    <|>        ErrorValue    <$> o!".error"
    <|>        Projection    <$> o!".project" <*> o!".label"
    <|>        Selection     <$> o!".select"  <*> o!".key"
    <|>        VariantValue  <$> o!".variants"
    <|>        PreValue      <$> o!".pre"     <*> o!".default"
    <|>        RecordValue   <$> readJSON o
    <|>        do vs <- readJSON o :: Result [LinValue]
                  return (foldr1 ConcatValue vs)

instance JSON LinLiteral where
  -- basic values (Str, Float, Int) are encoded as JSON strings/numbers:
  showJSON (StrConstant   s) = showJSON s
  showJSON (FloatConstant f) = showJSON f
  showJSON (IntConstant   n) = showJSON n

  readJSON = readBasicJSON StrConstant IntConstant FloatConstant

instance JSON LinPattern where
  -- wildcards and patterns without arguments are encoded as strings:
  showJSON (WildPattern) = showJSON "_"
  showJSON (ParamPattern (Param p [])) = showJSON p
  -- complex patterns are encoded as JSON objects:
  showJSON (ParamPattern pv) = showJSON pv
  -- and records as records:
  showJSON (RecordPattern r) = showJSON r

  readJSON o = do "_" <- readJSON o; return WildPattern
    <|>        do  p  <- readJSON o; return (ParamPattern (Param p []))
    <|>        ParamPattern  <$> readJSON o
    <|>        RecordPattern <$> readJSON o

instance JSON arg => JSON (Param arg) where
  -- parameters without arguments are encoded as strings:
  showJSON (Param p [])   = showJSON p
  showJSON (Param p args) = makeObj [(".paramid", showJSON p), (".args", showJSON args)]

  readJSON o = Param <$> readJSON o <*> return []
    <|>        Param <$> o!".paramid" <*> o!".args"

instance JSON a => JSON (RecordRow a) where
  -- record rows and lists of record rows are both encoded as JSON records (i.e., objects)
  showJSON  row  = showJSONs [row]
  showJSONs rows = makeObj (map toRow rows)
    where toRow (RecordRow (LabelId lbl) val) = (lbl, showJSON val)

  readJSON  obj = head <$> readJSONs obj
  readJSONs obj = mapM fromRow (assocsJSObject obj)
    where fromRow (lbl, jsvalue) = do value <- readJSON jsvalue
                                      return (RecordRow (LabelId lbl) value)

instance JSON rhs => JSON (TableRow rhs) where
  showJSON (TableRow l v) = makeObj [(".pattern", showJSON l), (".value", showJSON v)]

  readJSON o = TableRow <$> o!".pattern" <*> o!".value"


-- *** Identifiers in Concrete Syntax

instance JSON PredefId   where showJSON (PredefId    s) = showJSON s ; readJSON = fmap PredefId    . readJSON 
instance JSON LabelId    where showJSON (LabelId     s) = showJSON s ; readJSON = fmap LabelId     . readJSON 
instance JSON VarValueId where showJSON (VarValueId  s) = showJSON s ; readJSON = fmap VarValueId  . readJSON 
instance JSON ParamId    where showJSON (ParamId     s) = showJSON s ; readJSON = fmap ParamId     . readJSON 
instance JSON ParamType  where showJSON (ParamTypeId s) = showJSON s ; readJSON = fmap ParamTypeId . readJSON 


--------------------------------------------------------------------------------
-- ** Used in both Abstract and Concrete Syntax

instance JSON ModId where showJSON (ModId s) = showJSON s ; readJSON = fmap ModId . readJSON 
instance JSON CatId where showJSON (CatId s) = showJSON s ; readJSON = fmap CatId . readJSON 
instance JSON FunId where showJSON (FunId s) = showJSON s ; readJSON = fmap FunId . readJSON 

instance JSON VarId where
  -- the anonymous variable is the underscore:
  showJSON Anonymous = showJSON "_"
  showJSON (VarId x) = showJSON x

  readJSON o = do "_" <- readJSON o; return Anonymous
    <|>        VarId <$> readJSON o

instance JSON QualId where
  showJSON (Qual (ModId m) n) = showJSON (m++"."++n)
  showJSON (Unqual n) = showJSON n

  readJSON o = do qualid <- readJSON o
                  let (mod, id) = span (/= '.') qualid
                  return $ if null mod then Unqual id else Qual (ModId mod) id

instance JSON Flags where
  -- flags are encoded directly as JSON records (i.e., objects):
  showJSON (Flags fs) = makeObj [(f, showJSON v) | (f, v) <- fs]

  readJSON obj = Flags <$> mapM fromRow (assocsJSObject obj)
    where fromRow (lbl, jsvalue) = do value <- readJSON jsvalue
                                      return (lbl, value)

instance JSON FlagValue where
  -- flag values are encoded as basic JSON types:
  showJSON (Str s) = showJSON s
  showJSON (Int i) = showJSON i
  showJSON (Flt f) = showJSON f

  readJSON = readBasicJSON Str Int Flt


--------------------------------------------------------------------------------
-- ** Convenience functions

(!) :: JSON a => JSValue -> String -> Result a
obj ! key = maybe (fail $ "CanonicalJSON.(!): Could not find key: " ++ show key)
            readJSON
            (lookup key (assocsJSObject obj))

assocsJSObject :: JSValue -> [(String, JSValue)]
assocsJSObject (JSObject o) = fromJSObject o
assocsJSObject (JSArray  _) = fail $ "CanonicalJSON.assocsJSObject: Expected a JSON object, found an Array"
assocsJSObject  jsvalue     = fail $ "CanonicalJSON.assocsJSObject: Expected a JSON object, found " ++ show jsvalue


readBasicJSON :: (JSON int, Integral int, JSON flt, RealFloat flt) =>
                 (String -> v) -> (int -> v) -> (flt -> v) -> JSValue -> Result v
readBasicJSON str int flt o
  =   str        <$> readJSON o
  <|> int_or_flt <$> readJSON o
  where int_or_flt f | f == fromIntegral n = int n
                     | otherwise           = flt f
          where n = round f
