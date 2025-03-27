module GF.Grammar.JSON( TermPrintQual(..),
                        grammar2json,
                        term2json, json2term,
                        patt2json, json2patt
                      ) where

import GF.Infra.Ident
import GF.Grammar.Grammar
import GF.Grammar.Printer(TermPrintQual(..))
import Text.JSON
import Text.JSON.Types
import Control.Monad (forM,(>=>),liftM2,guard)
import Control.Applicative ((<|>))
import qualified Data.Map as Map
import Data.Maybe (catMaybes)

grammar2json :: TermPrintQual -> Grammar -> JSValue
grammar2json q gr =
  makeObj [(showIdent mn, mi2json q mi) | (MN mn,mi) <- modules gr]

mi2json q mi = makeObj [("jments",makeObj (map (jment2json q) (Map.toList (jments mi))))]

jment2json q (id,info) = (showIdent id, info2json q info)

info2json q (AbsCat mb_ctxt) =
  case mb_ctxt of
    Nothing         -> makeObj []
    Just (L _ ctxt) -> makeObj [("context", showJSON (map (hypo2json q) ctxt))]
info2json q (AbsFun mb_ty mb_arity mb_eqs _) =
  (makeObj . catMaybes)
     [ fmap (\(L _ ty) -> ("abstype",term2json q ty)) mb_ty
     , fmap (\a -> ("arity",showJSON a)) mb_arity
     , fmap (\eqs -> ("equations",showJSON (map (\(L _ eq) -> equation2json q eq) eqs))) mb_eqs
     ]
info2json q (ResParam mb_params _) =
  makeObj [("params", case mb_params of
                         Nothing           -> JSArray []
                         Just (L _ params) -> showJSON (map (param2json q) params))]
info2json q (ResValue (L _ ty) _) =
  makeObj [("paramtype",term2json q ty)]
info2json q (ResOper mb_ty mb_def) =
  (makeObj . catMaybes)
     [ fmap (\(L _ ty) -> ("opertype",term2json q ty)) mb_ty
     , fmap (\(L _ def) -> ("operdef",term2json q def)) mb_def
     ]
info2json q (ResOverload mns overloads) =
  makeObj
     [ ("extends",showJSON mns)
     , ("overloads",showJSON (map (overload2json q) overloads))
     ]
info2json q (CncCat mb_ty mb_lindef mb_linref mb_pnm _) =
  (makeObj . catMaybes)
     [ fmap (\(L _ ty) -> ("lintype",term2json q ty)) mb_ty
     , fmap (\(L _ def) -> ("lindef",term2json q def)) mb_lindef
     , fmap (\(L _ ref) -> ("linref",term2json q ref)) mb_linref
     , fmap (\(L _ prn) -> ("printname",term2json q prn)) mb_pnm
     ]
info2json q (CncFun _ mb_lin mb_pnm _) =
  (makeObj . catMaybes)
     [ fmap (\(L _ lin) -> ("lin",term2json q lin)) mb_lin
     , fmap (\(L _ prn) -> ("printname",term2json q prn)) mb_pnm
     ]
info2json q (AnyInd _ mn) = showJSON mn

hypo2json q (bt,x,ty) =
  makeObj [("implicit", showJSON (bt==Implicit)), ("var", showJSON x), ("type", term2json q ty)]

equation2json q (ps,t) =
  makeObj [("patts", showJSON (map (patt2json q) ps)), ("term", term2json q t)]

param2json q (id, ctxt) =
  makeObj [("id", showJSON id), ("context", showJSON (map (hypo2json q) ctxt))]

overload2json q (L _ ty,L _ def) =
  makeObj
     [ ("opertype",term2json q ty)
     , ("operdef",term2json q def)
     ]

term2json :: TermPrintQual -> Term -> JSValue
term2json q (Vr v) = makeObj [("vr", showJSON v)]
term2json q (Cn v) = makeObj [("cn", showJSON v)]
term2json q (Con v) = makeObj [("con", showJSON v)]
term2json q (Sort v) = makeObj [("sort", showJSON v)]
term2json q (EInt n) = showJSON n
term2json q (EFloat f) = showJSON f
term2json q (K s) = showJSON s
term2json q Empty = JSArray []
term2json q (App t1 t2) = makeObj [("fun", term2json q t1), ("arg", term2json q t2)]
term2json q (Abs bt x t) = makeObj [("implicit", showJSON (bt==Implicit)), ("var", showJSON x), ("body", term2json q t)]
term2json q (Meta id) = makeObj [("metaid", showJSON id)]
term2json q (ImplArg t) = makeObj [("implarg", term2json q t)]
term2json q (Prod bt v t1 t2) = makeObj [("implicit", showJSON (bt==Implicit)), ("var", showJSON v), ("hypo", term2json q t1), ("res", term2json q t2)]
term2json q (Typed t ty) = makeObj [("term", term2json q t), ("type", term2json q ty)]
term2json q (Example t s) = makeObj [("term", term2json q t), ("example", showJSON s)]
term2json q (RecType lbls) = makeObj [("rectype", makeObj (map toRow lbls))]
                              where toRow (l,t) = (showLabel l, term2json q t)
term2json q (R lbls) = makeObj [("record", makeObj (map toRow lbls))]
                              where toRow (l,(_,t)) = (showLabel l, term2json q t)
term2json q (P t proj) = makeObj [("project", term2json q t), ("label", showJSON (showLabel proj))]
term2json q (ExtR t1 t2) = makeObj [("term", term2json q t1), ("ext", term2json q t2)]
term2json q (Table t1 t2) = makeObj [("tblhypo", term2json q t1), ("tblres", term2json q t2)]
term2json q (T _ cs) = makeObj [("tblcases", showJSON [(patt2json q p, term2json q t) | (p,t) <- cs])]
term2json q (V ty ts) = makeObj [("tbltype", term2json q ty), ("tblvalues", showJSON (map (term2json q) ts))]
term2json q (S t1 t2) = makeObj [("select", term2json q t1), ("key", term2json q t2)]
term2json q (Let (v,(_,t1)) t2) = makeObj [("letvar", showJSON v), ("letdef", term2json q t1), ("term", term2json q t2)]
term2json q (Q (m,id))  = case q of
                            Qualified -> makeObj [("mod",showJSON m),("q", showJSON id)]
                            _         -> makeObj [("cn", showJSON id)]
term2json q (QC (m,id)) = case q of
                            Qualified -> makeObj [("mod",showJSON m),("qc", showJSON id)]
                            _         -> makeObj [("con", showJSON id)]
term2json q (C t1 t2) = showJSON ((flatten t1 . flatten t2) [])
    where
      flatten Empty     = id
      flatten (C t1 t2) = flatten t1 . flatten t2
      flatten t         = (term2json q t :)
term2json q (Glue t1 t2) = makeObj [("glue1",term2json q t1),("glue2", term2json q t2)]
term2json q (EPattType t) = makeObj [("patttype",term2json q t)]
term2json q (ELincat id t) = makeObj [("lincat",showJSON id), ("term",term2json q t)]
term2json q (ELin id t) = makeObj [("lin",showJSON id), ("term",term2json q t)]
term2json q (AdHocOverload ts) = makeObj [("overloaded",showJSON (map (term2json q) ts))]
term2json q (FV ts) = makeObj [("variants",showJSON (map (term2json q) ts))]
term2json q (Markup tag attrs children) = makeObj [ ("tag",showJSON tag)
                                                  , ("attrs",showJSON (map (\(attr,val) -> (showJSON attr,term2json q val)) attrs))
                                                  , ("children",showJSON (map (term2json q) children))
                                                  ]
term2json q (Reset ctl t) =
  let jctl = case ctl of
               All     -> showJSON "all"
               One     -> showJSON "one"
               Limit n -> showJSON n
               Coordination Nothing    conj cat -> makeObj [("conj",showJSON conj), ("cat",showJSON cat)]
               Coordination (Just mod) conj cat -> makeObj [("mod",showJSON mod), ("conj",showJSON conj), ("cat",showJSON cat)]
  in makeObj [("reset",jctl), ("term",term2json q t)]
term2json q (Alts def alts) = makeObj [("def",term2json q def), ("alts",showJSON (map (\(t1,t2) -> (term2json q t1, term2json q t2)) alts))]
term2json q (Strs ts) = makeObj [("strs",showJSON (map (term2json q) ts))]

json2term o  = Vr      <$> o!:"vr"
    <|>  curry Q       <$> o!:"mod" <*> o!:"cn"
    <|>  curry QC      <$> o!:"mod" <*> o!:"con"
    <|>        Cn      <$> o!:"cn"
    <|>        Con     <$> o!:"con"
    <|>        Sort    <$> o!:"sort"
    <|>        EInt    <$> readJSON o
    <|>        EFloat  <$> readJSON o
    <|>        K       <$> readJSON o
    <|>        App     <$> o!<"fun" <*> o!<"arg"
    <|>        Abs     <$> fmap toBindType (o!:"implicit") <*> o!:"var" <*> o!<"body"
    <|>        Meta    <$> o!:"metaid"
    <|>        ImplArg <$> o!<"implarg"
    <|>        Prod    <$> fmap toBindType (o!:"implicit") <*> o!:"var" <*> o!<"hypo" <*> o!<"res"
    <|>        Typed   <$> o!<"term" <*> o!<"type"
    <|>        Example <$> o!<"term" <*> o!:"example"
    <|>        RecType <$> (o!:"rectype" >>= \o -> mapM fromRow  (assocsJSObject o))
    <|>        R       <$> (o!:"record"  >>= \o -> mapM fromRow' (assocsJSObject o))
    <|>        P       <$> o!<"project" <*> fmap readLabel (o!:"label")
    <|>        ExtR    <$> o!<"term" <*> o!<"ext"
    <|>        Table   <$> o!<"tblhypo" <*> o!<"tblres"
    <|>        do o  <- readJSON o
                  cs <- valFromObj "tblcases" o
                  cs <- forM cs $ \(p,t) -> do
                          p <- json2patt p
                          t <- json2term t
                          return (p,t)
                  return (T TRaw cs)
    <|>        do o  <- readJSON o
                  ty <- valFromObj "tbltype" o >>= json2term
                  ts <- valFromObj "tblvalues" o >>= mapM json2term
                  return (V ty ts)
    <|>        S       <$> o!<"select" <*> o!<":.key"
    <|>  (\v t1 -> Let (v,(Nothing,t1))) <$> o!:"letvar" <*> o!<"letdef" <*> o!<"term"
    <|>      mkC       <$> (readJSON o >>= mapM json2term)
    <|>        Glue    <$> o!<"glue1" <*> o!<"glue2"
    <|>      EPattType <$> o!<"patttype"
    <|>        ELincat <$> o!:"lincat" <*> o!<"term"
    <|>        ELin    <$> o!:"lin" <*> o!<"term"
    <|>  AdHocOverload <$> (o!:"overloaded" >>= mapM json2term)
    <|>        FV      <$> (o!:"variants" >>= mapM json2term)
    <|>        Markup  <$> (o!:"tag") <*>
                           (o!:"attrs" >>= mapM (\(attr,val) -> fmap ((,)attr) (json2term val))) <*>
                           (o!:"children" >>= mapM json2term)
    <|>        Reset   <$> (readJSON >=> valFromObj "reset" >=> json2ctl) o <*> o!<"term"
    <|>        Alts    <$> (o!<"def") <*> (o!:"alts" >>= mapM (\(x,y) -> liftM2 (,) (json2term x) (json2term y)))
    <|>        Strs    <$> (o!:"strs" >>= mapM json2term)
    where
      fromRow  (lbl, jsvalue) = do value <- json2term jsvalue
                                   return (readLabel lbl,value)

      fromRow' (lbl, jsvalue) = do value <- json2term jsvalue
                                   return (readLabel lbl,(Nothing,value))

      toBindType True  = Implicit
      toBindType False = Explicit

      mkC []     = Empty
      mkC (t:ts) = foldl C t ts

      json2ctl (JSString (JSONString "all")) = return All
      json2ctl (JSString (JSONString "one")) = return One
      json2ctl (JSRational _ i) = return (Limit (round i))
      json2ctl (JSObject o) = do
        mb_mod <- fmap Just (valFromObj "mod" o) <|> return Nothing
        conj <- valFromObj "conj" o
        cat  <- valFromObj "cat" o
        return (Coordination mb_mod conj cat)
      json2ctl _ = fail "Invalid control value for reset"


patt2json q (PC id ps)     = makeObj [("pc",showJSON id),("args",showJSON (map (patt2json q) ps))]
patt2json q (PP (mn,id) ps) = 
  case q of
    Qualified -> makeObj [("mod",showJSON mn),("pc",showJSON id),("args",showJSON (map (patt2json q) ps))]
    _         -> makeObj [                     ("pc",showJSON id),("args",showJSON (map (patt2json q) ps))]
patt2json q (PV id)        = makeObj [("pv",showJSON id)]
patt2json q PW             = makeObj [("wildcard",showJSON True)]
patt2json q (PR lbls)      = makeObj (("record", showJSON True) : map toRow lbls)
                               where toRow (l,t) = (showLabel l, patt2json q t)
patt2json q (PString s)    = showJSON s
patt2json q (PInt n)       = showJSON n
patt2json q (PFloat d)     = showJSON d
patt2json q (PT ty p)      = makeObj [("type", term2json q ty), ("patt", patt2json q p)]
patt2json q (PAs id p)     = makeObj [("as", showJSON id), ("patt", patt2json q p)]
patt2json q (PImplArg p)   = makeObj [("implarg", patt2json q p)]
patt2json q (PTilde t)     = makeObj [("tilde", term2json q t)]
patt2json q (PNeg p)       = makeObj [("neg", patt2json q p)]
patt2json q (PAlt p1 p2)   = makeObj [("alt1", patt2json q p1), ("alt2", patt2json q p2)]
patt2json q (PSeq min1 max1 p1 min2 max2 p2)
                           = makeObj [("min1",  showJSON min1)
                                     ,("max1",  showJSON max1)
                                     ,("patt1", patt2json q p1)
                                     ,("min2",  showJSON min2)
                                     ,("max2",  showJSON max2)
                                     ,("patt2", patt2json q p2)
                                     ]
patt2json q (PRep min max p)=makeObj [("min",  showJSON min)
                                     ,("max",  showJSON max)
                                     ,("patt", patt2json q p)
                                     ]
patt2json q PChar          = makeObj [("char",showJSON True)]
patt2json q (PChars cs)    = makeObj [("chars",showJSON cs)]
patt2json q (PMacro id)    = makeObj [("macro",showJSON id)]
patt2json q (PM (mn,id))   = makeObj [("mod",showJSON mn), ("macro",showJSON id)]

json2patt :: JSValue -> Result Patt
json2patt o = PP <$> (liftM2 (\mn id -> (mn,id)) (o!:"mod") (o!:"pc")) <*> (o!:"args" >>= mapM json2patt)
    <|>       PC <$> (o!:"pc") <*> (o!:"args" >>= mapM json2patt)
    <|>       PV <$> (o!:"pv")
    <|>  (o!:"wildcard" >>= guard >> return PW)
    <|> (const PR) <$> (o!:"record" >>= guard) <*> mapM fromRow (assocsJSObject o)
    <|>  PString <$> readJSON o
    <|>  PInt    <$> readJSON o
    <|>  PFloat  <$> readJSON o
    <|>  PT      <$> o!<"type" <*> o!>"patt"
    <|>  PAs     <$> o!:"as" <*> o!>"patt"
    <|>  PImplArg<$> o!>"implarg"
    <|>  PTilde  <$> o!<"tilde"
    <|>  PNeg    <$> o!>"neg"
    <|>  PAlt    <$> o!>"alt1" <*> o!>"alt2"
    <|>  PSeq    <$> o!:"min1" <*> o!:"max1" <*> o!>"patt1" <*> o!:"min2" <*> o!:"max2" <*> o!>"patt2"
    <|>  PRep    <$> o!:"min" <*> o!:"max" <*> o!>"rep"
    <|>  (o!:"char" >>= guard >> return PChar)
    <|>  PChars  <$> o!:"chars"
    <|>  PM      <$> liftM2 (,) (o!:"mod") (o!:"macro")
    <|>  PMacro  <$> o!:"macro"
    where
      fromRow  (lbl, jsvalue) = do patt <- json2patt jsvalue
                                   return (readLabel lbl,patt)


showLabel :: Label -> String
showLabel (LIdent s) = showRawIdent s
showLabel (LVar i)   = '$':show i

readLabel ('$':s) = LVar (read s)
readLabel s = LIdent (rawIdentS s)

(!<) :: JSValue -> String -> Result Term
obj !< key = maybe (fail $ "(!<): could not find key: " ++ key)
            json2term
            (lookup key (assocsJSObject obj))

(!>) :: JSValue -> String -> Result Patt
obj !> key = maybe (fail $ "(!>): could not find key: " ++ key)
             json2patt
             (lookup key (assocsJSObject obj))

(!:) :: JSON a => JSValue -> String -> Result a
obj !: key = maybe (fail $ "(!:): could not find key: " ++ key)
             readJSON
             (lookup key (assocsJSObject obj))

assocsJSObject :: JSValue -> [(String, JSValue)]
assocsJSObject (JSObject o) = fromJSObject o
assocsJSObject (JSArray  _) = fail $ "assocsJSObject: Expected a JSON object, found an Array"
assocsJSObject  jsvalue     = fail $ "assocsJSObject: Expected a JSON object, found " ++ show jsvalue
