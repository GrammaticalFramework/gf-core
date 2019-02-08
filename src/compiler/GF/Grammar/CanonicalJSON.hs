{-# language OverloadedStrings, OverloadedLists #-}

module GF.Grammar.CanonicalJSON (
  encodeJSON, encodeYAML,
  decodeJSON, decodeYAML
  ) where


import qualified Control.Monad       as CM    (mapM, msum)
import qualified Data.HashMap.Strict as HM    (toList)
import qualified Data.Yaml           as Yaml  (encodeFile, decodeFileEither, ParseException)
import qualified Data.Aeson          as Aeson (encodeFile, eitherDecodeFileStrict')
import Data.Aeson                             (ToJSON(..), object, (.=))
import Data.Aeson                             (FromJSON(..), Value(..), withObject, (.:), (.:?), (.!=))
import Data.Aeson.Types                       (typeMismatch, modifyFailure, Pair, Parser)
import Data.Text                              (Text, pack, unpack)
import Data.Scientific                        (floatingOrInteger)

import GF.Grammar.Canonical


encodeJSON :: FilePath -> Grammar -> IO ()
encodeJSON = Aeson.encodeFile

encodeYAML :: FilePath -> Grammar -> IO ()
encodeYAML = Yaml.encodeFile

decodeJSON :: FilePath -> IO (Either String Grammar)
decodeJSON = Aeson.eitherDecodeFileStrict'

decodeYAML :: FilePath -> IO (Either Yaml.ParseException Grammar)
decodeYAML = Yaml.decodeFileEither


-- in general we encode grammars using JSON objects/records,
-- except for newtypes/coercions/direct values

-- the top-level definitions use normal record labels,
-- but recursive types/values/ids use labels staring with a "."

instance ToJSON Grammar where
  toJSON (Grammar abs cncs) = object ["abstract" .= abs, "concretes" .= cncs]

instance FromJSON Grammar where
  parseJSON = withObject "Grammar" $ \o -> Grammar <$> o .: "abstract" <*> o .: "concretes"


--------------------------------------------------------------------------------
-- ** Abstract Syntax

instance ToJSON Abstract where
  toJSON (Abstract absid flags cats funs) 
    = object ["abs"   .= absid,
              "flags" .= flags,
              "cats"  .= cats,
              "funs"  .= funs]

instance FromJSON Abstract where
  parseJSON = withObject "Abstract" $ \o -> Abstract
    <$> o .:  "abs"
    <*> o .:? "flags" .!= Flags []
    <*> o .:  "cats"
    <*> o .:  "funs"


instance ToJSON CatDef where
  -- non-dependent categories are encoded as simple strings:
  toJSON (CatDef c []) = toJSON c
  toJSON (CatDef c cs) = object ["cat" .= c, "args" .= cs]

instance FromJSON CatDef where
  parseJSON (String s) = return $ CatDef (CatId (unpack s)) []
  parseJSON (Object o) = CatDef <$> o .: "cat" <*> o .: "args"
  parseJSON val        = typeMismatch "CatDef" val


instance ToJSON FunDef where
  toJSON (FunDef f ty) = object ["fun" .= f, "type" .= ty]

instance FromJSON FunDef where
  parseJSON = withObject "FunDef" $ \o -> FunDef <$> o .: "fun" <*> o .: "type"


instance ToJSON Type where
  toJSON (Type bs ty) = object [".args" .= bs, ".result" .= ty]

instance FromJSON Type where
  parseJSON = withObject "Type" $ \o -> Type <$> o .: ".args" <*> o .: ".result"


instance ToJSON TypeApp where
  -- non-dependent categories are encoded as simple strings:
  toJSON (TypeApp c [])   = toJSON c
  toJSON (TypeApp c args) = object [".cat" .= c, ".args" .= args]

instance FromJSON TypeApp where
  parseJSON (String s) = return $ TypeApp (CatId (unpack s)) []
  parseJSON (Object o) = TypeApp <$> o .: ".cat" <*> o .: ".args"
  parseJSON val        = typeMismatch "TypeApp" val


instance ToJSON TypeBinding where
  -- non-dependent categories are encoded as simple strings:
  toJSON (TypeBinding Anonymous (Type [] (TypeApp c []))) = toJSON c
  toJSON (TypeBinding x ty) = object [".var" .= x, ".type" .= ty]

instance FromJSON TypeBinding where
  parseJSON (String s) = return $ TypeBinding Anonymous (Type [] (TypeApp (CatId (unpack s)) []))
  parseJSON (Object o) = TypeBinding <$> o .: ".var" <*> o .: ".type"
  parseJSON val        = typeMismatch "TypeBinding" val


--------------------------------------------------------------------------------
-- ** Concrete syntax

instance ToJSON Concrete where
  toJSON (Concrete cncid absid flags params lincats lins) 
    = object ["cnc"     .= cncid,
              "abs"     .= absid,
              "flags"   .= flags,
              "params"  .= params,
              "lincats" .= lincats,
              "lins"    .= lins]

instance FromJSON Concrete where
  parseJSON = withObject "Concrete" $ \o -> Concrete
    <$> o .:  "cnc"
    <*> o .:  "abs"
    <*> o .:? "flags" .!= Flags []
    <*> o .:  "params"
    <*> o .:  "lincats"
    <*> o .:  "lins"


instance ToJSON ParamDef where
  toJSON (ParamDef      p pvs) = object ["param" .= p, "values" .= pvs]
  toJSON (ParamAliasDef p t)   = object ["param" .= p, "alias"  .= t]

instance FromJSON ParamDef where
  parseJSON = withObject "ParamDef" $ \o ->
    choose [ ParamDef      <$> o .: "param" <*> o .: "values"
           , ParamAliasDef <$> o .: "param" <*> o .: "alias"
           ]


instance ToJSON LincatDef where
  toJSON (LincatDef c lt) = object ["cat" .= c, "lintype" .= lt]

instance FromJSON LincatDef where
  parseJSON = withObject "LincatDef" $ \v -> LincatDef <$> v .: "cat" <*> v .: "lintype"


instance ToJSON LinDef where
  toJSON (LinDef f xs lv) = object ["fun" .= f, "args" .= xs, "lin" .= lv]

instance FromJSON LinDef where
  parseJSON = withObject "LinDef" $ \v -> LinDef <$> v .: "fun" <*> v .: "args" <*> v .: "lin"


instance ToJSON LinType where
  toJSON lt = case lt of
    -- the basic types (Str, Float, Int) are encoded as strings:
    StrType         -> "Str"
    FloatType       -> "Float"
    IntType         -> "Int"
    -- parameters are also encoded as strings:
    ParamType pt    -> toJSON pt
    -- tables/tuples are encoded as JSON objects:
    TableType pt lt -> object [".tblarg" .= pt, ".tblval" .= lt]
    TupleType lts   -> object [".tuple"  .= lts]
    -- records are encoded as records:
    RecordType rows -> toJSON rows

instance FromJSON LinType where
  parseJSON (String "Str")   = return StrType
  parseJSON (String "Float") = return FloatType
  parseJSON (String "Int")   = return IntType
  parseJSON (String param)   = return (ParamType (ParamTypeId (ParamId (unpack param))))
  parseJSON val@(Object o)   = choose [ (TableType  <$> o .: ".tblarg" <*> o .: ".tblval")
                                      , (TupleType  <$> o .: ".tuple")
                                      , (RecordType <$> parseJSON val)
                                      ]
  parseJSON val = typeMismatch "LinType" val


instance ToJSON LinValue where
  toJSON lv = case lv of
    -- basic values (Str, Float, Int) are encoded as JSON strings/numbers:
    StrConstant    s  -> toJSON s
    FloatConstant  f  -> toJSON f
    IntConstant    n  -> toJSON n
    -- concatenation is encoded as a JSON array:
    ConcatValue  v v' -> Array [toJSON v, toJSON v']
    -- most values are encoded as JSON objects:
    ParamConstant  pv -> object [".param"    .= pv]
    PredefValue    p  -> object [".predef"   .= p]
    TableValue  t tvs -> object [".tblarg"   .= t, ".tblrows"    .= tvs]
--  VTableValue  t ts -> object [".vtblarg"  .= t, ".vtblrows"   .= ts]
    TupleValue    lvs -> object [".tuple"    .= lvs]
    VarValue       v  -> object [".var"      .= v]
    ErrorValue     s  -> object [".error"    .= s]
    Projection  lv l  -> object [".project"  .= lv, ".label" .= l]
    Selection   tv pv -> object [".select"   .= tv, ".key" .= pv]
    VariantValue   vs -> object [".variants" .= vs]
    PreValue alts def -> object [".pre"      .= alts, ".default" .= def]
    -- records are encoded directly as JSON records:
    RecordValue  rows -> toJSON rows

instance FromJSON LinValue where
  parseJSON (String s)     = return (StrConstant (unpack s))
  parseJSON (Number n)     = return (either FloatConstant IntConstant (floatingOrInteger n))
  parseJSON (Array [v,v']) = ConcatValue <$> parseJSON v <*> parseJSON v'
  parseJSON val@(Object o) = choose [ ParamConstant <$> o .: ".param"
                                    , PredefValue   <$> o .: ".predef"
                                    , TableValue    <$> o .: ".tblarg"  <*> o .: ".tblrows"
--                                  , VTableValue   <$> o .: ".vtblarg" <*> o .: ".vtblrows"
                                    , TupleValue    <$> o .: ".tuple"
                                    , VarValue      <$> o .: ".var"
                                    , ErrorValue    <$> o .: ".error"
                                    , Projection    <$> o .: ".project" <*> o .: ".label"
                                    , Selection     <$> o .: ".select"  <*> o .: ".key"
                                    , VariantValue  <$> o .: ".variants"
                                    , PreValue      <$> o .: ".pre"     <*> o .: ".default"
                                    , RecordValue   <$> parseJSON val
                                    ]
  parseJSON val = typeMismatch "LinValue" val


instance ToJSON LinPattern where
  toJSON linpat = case linpat of
    -- wildcards and patterns without arguments are encoded as strings:
    WildPattern -> "_"
    ParamPattern (Param p []) -> toJSON p
    -- complex patterns are encoded as JSON objects:
    ParamPattern pv -> toJSON pv
    -- and records as records:
    RecordPattern r -> toJSON r

instance FromJSON LinPattern where
  parseJSON (String "_") = return WildPattern
  parseJSON (String  s)  = return (ParamPattern (Param (ParamId (unpack s)) []))
  parseJSON val = choose [ ParamPattern  <$> parseJSON val
                         , RecordPattern <$> parseJSON val
                         , typeMismatch "LinPattern" val
                         ]


instance ToJSON arg => ToJSON (Param arg) where
  -- parameters without arguments are encoded as strings:
  toJSON (Param p [])   = toJSON p
  toJSON (Param p args) = object [".paramid" .= p, ".args" .= args]

instance FromJSON arg => FromJSON (Param arg) where
  parseJSON (String p) = return (Param (ParamId (unpack p)) [])
  parseJSON (Object o) = Param <$> o .: ".paramid" <*> o .: ".args"
  parseJSON val = typeMismatch "Param" val


instance ToJSON a => ToJSON (RecordRow a) where
  -- record rows and lists of record rows are both encoded as JSON records (i.e., objects)
  toJSON row = object [toJSONRecordRow row]
  toJSONList = object . map toJSONRecordRow

toJSONRecordRow :: ToJSON a => RecordRow a -> Pair
toJSONRecordRow (RecordRow (LabelId lbl) val) = pack lbl .= val

instance FromJSON a => FromJSON (RecordRow a) where
  parseJSON     = withObject  "RecordRow"  $ \o -> parseJSONRecordRow (head (HM.toList o))
  parseJSONList = withObject "[RecordRow]" $ \o -> CM.mapM parseJSONRecordRow (HM.toList o) 

parseJSONRecordRow :: FromJSON a => (Text, Value) -> Parser (RecordRow a)
parseJSONRecordRow (lbl, val) = do val' <- parseJSON val
                                   return (RecordRow (LabelId (unpack lbl)) val')


instance ToJSON TableRowValue where
  toJSON (TableRowValue l v) = object [".pattern" .= l, ".value" .= v]

instance FromJSON TableRowValue where
  parseJSON = withObject "TableRowValue" $ \v -> TableRowValue <$> v .: ".pattern" <*> v .: ".value"


-- *** Identifiers in Concrete Syntax

instance ToJSON PredefId   where toJSON (PredefId    s) = toJSON s
instance ToJSON LabelId    where toJSON (LabelId     s) = toJSON s
instance ToJSON VarValueId where toJSON (VarValueId  s) = toJSON s
instance ToJSON ParamId    where toJSON (ParamId     s) = toJSON s
instance ToJSON ParamType  where toJSON (ParamTypeId s) = toJSON s

instance FromJSON PredefId   where parseJSON = coerceFrom "PredefId"   PredefId
instance FromJSON LabelId    where parseJSON = coerceFrom "LabelId"    LabelId
instance FromJSON VarValueId where parseJSON = coerceFrom "VarValueId" VarValueId
instance FromJSON ParamId    where parseJSON = coerceFrom "ParamId"    ParamId
instance FromJSON ParamType  where parseJSON = coerceFrom "ParamType"  ParamTypeId


--------------------------------------------------------------------------------
-- ** Used in both Abstract and Concrete Syntax

instance ToJSON ModId where toJSON (ModId s) = toJSON s
instance ToJSON CatId where toJSON (CatId s) = toJSON s
instance ToJSON FunId where toJSON (FunId s) = toJSON s

instance FromJSON ModId where parseJSON = coerceFrom "ModId" ModId
instance FromJSON CatId where parseJSON = coerceFrom "CatId" CatId
instance FromJSON FunId where parseJSON = coerceFrom "FunId" FunId


instance ToJSON VarId where
  -- the anonymous variable is the underscore:
  toJSON Anonymous = "_"
  toJSON (VarId x) = toJSON x

instance FromJSON VarId where
  parseJSON (String "_") = return Anonymous
  parseJSON (String  s)  = return (VarId (unpack s))
  parseJSON val = typeMismatch "VarId" val


instance ToJSON Flags where
  -- flags are encoded directly as JSON records (i.e., objects):
  toJSON (Flags fs) = object [ pack f .= v | (f, v) <- fs ]

instance FromJSON Flags where
  parseJSON = withObject "Flags" $ \o -> Flags <$> CM.mapM parseJSONFlag (HM.toList o)
    where parseJSONFlag (flag, val) = do val' <- parseJSON val
                                         return (unpack flag, val')


instance ToJSON FlagValue where
  -- flag values are encoded as basic JSON types:
  toJSON (Str s) = toJSON s
  toJSON (Int i) = toJSON i
  toJSON (Flt f) = toJSON f

instance FromJSON FlagValue where
  parseJSON (String s) = return $ Str (unpack s)
  parseJSON (Number n) = return $ case floatingOrInteger n of
                                    Left  f -> Flt f
                                    Right i -> Int i
  parseJSON invalid = typeMismatch "FlagValue" invalid


--------------------------------------------------------------------------------
-- ** Helper functions

choose :: [Parser a] -> Parser a
choose = CM.msum

coerceFrom :: FromJSON s => String -> (s -> a) -> Value -> Parser a
coerceFrom expected constructor obj = modifyFailure failure $ fmap constructor $ parseJSON obj
  where failure f = "(while parsing " ++ expected ++ ") " ++ f

