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

grammar2json :: Grammar -> JSValue
grammar2json gr =
  makeObj [(showIdent mn, mi2json mi) | (MN mn,mi) <- modules gr]

mi2json mi = makeObj [("type", mtype2json (mtype mi))
                     ,("jments",makeObj (map jment2json (Map.toList (jments mi))))
                     ]

mtype2json MTAbstract     = showJSON "abstract"
mtype2json MTResource     = showJSON "resource"
mtype2json (MTConcrete _) = showJSON "concrete"
mtype2json MTInterface    = showJSON "interface"
mtype2json (MTInstance _) = showJSON "instance"

jment2json (id,info) = (showIdent id, info2json info)

info2json (AbsCat mb_ctxt) =
  case mb_ctxt of
    Nothing         -> makeObj []
    Just (L _ ctxt) -> makeObj [("context", showJSON (map hypo2json ctxt))]
info2json (AbsFun mb_ty mb_arity mb_eqs _) =
  (makeObj . catMaybes)
     [ fmap (\(L _ ty) -> ("abstype",term2json ty)) mb_ty
     , fmap (\a -> ("arity",showJSON a)) mb_arity
     , fmap (\eqs -> ("equations",showJSON (map (\(L _ eq) -> equation2json eq) eqs))) mb_eqs
     ]
info2json (ResParam mb_params _) =
  makeObj [("params", case mb_params of
                         Nothing           -> JSArray []
                         Just (L _ params) -> showJSON (map param2json params))]
info2json (ResValue (L _ ty) _) =
  makeObj [("paramtype",term2json ty)]
info2json (ResOper mb_ty mb_def) =
  (makeObj . catMaybes)
     [ fmap (\(L _ ty) -> ("opertype",term2json ty)) mb_ty
     , fmap (\(L _ def) -> ("operdef",term2json def)) mb_def
     ]
info2json (ResOverload mns overloads) =
  makeObj
     [ ("extends",showJSON mns)
     , ("overloads",showJSON (map overload2json overloads))
     ]
info2json (CncCat mb_ty mb_lindef mb_linref mb_pnm _) =
  (makeObj . catMaybes)
     [ fmap (\(L _ ty) -> ("lintype",term2json ty)) mb_ty
     , fmap (\(L _ def) -> ("lindef",term2json def)) mb_lindef
     , fmap (\(L _ ref) -> ("linref",term2json ref)) mb_linref
     , fmap (\(L _ prn) -> ("printname",term2json prn)) mb_pnm
     ]
info2json (CncFun _ mb_lin mb_pnm _) =
  (makeObj . catMaybes)
     [ fmap (\(L _ lin) -> ("lin",term2json lin)) mb_lin
     , fmap (\(L _ prn) -> ("printname",term2json prn)) mb_pnm
     ]
info2json (AnyInd _ mn) = showJSON mn

hypo2json (bt,x,ty) =
  makeObj [("implicit", showJSON (bt==Implicit)), ("var", showJSON x), ("type", term2json ty)]

equation2json (ps,t) =
  makeObj [("patts", showJSON (map patt2json ps)), ("term", term2json t)]

param2json (id, ctxt) =
  makeObj [("id", showJSON id), ("context", showJSON (map hypo2json ctxt))]

overload2json (L _ ty,L _ def) =
  makeObj
     [ ("opertype",term2json ty)
     , ("operdef",term2json def)
     ]

term2json :: Term -> JSValue
term2json (Vr v) = makeObj [("vr", showJSON v)]
term2json (Cn v) = makeObj [("cn", showJSON v)]
term2json (Con v) = makeObj [("con", showJSON v)]
term2json (Sort v) = makeObj [("sort", showJSON v)]
term2json (EInt n) = showJSON n
term2json (EFloat f) = showJSON f
term2json (K s) = showJSON s
term2json Empty = JSArray []
term2json (App t1 t2) = makeObj [("fun", term2json t1), ("arg", term2json t2)]
term2json (Abs bt x t) = makeObj [("implicit", showJSON (bt==Implicit)), ("var", showJSON x), ("body", term2json t)]
term2json (Meta id) = makeObj [("metaid", showJSON id)]
term2json (ImplArg t) = makeObj [("implarg", term2json t)]
term2json (Prod bt v t1 t2) = makeObj [("implicit", showJSON (bt==Implicit)), ("var", showJSON v), ("hypo", term2json t1), ("res", term2json t2)]
term2json (Typed t ty) = makeObj [("term", term2json t), ("type", term2json ty)]
term2json (Example t s) = makeObj [("term", term2json t), ("example", showJSON s)]
term2json (RecType lbls) = makeObj [("rectype", makeObj (map toRow lbls))]
                              where toRow (l,t) = (showLabel l, term2json t)
term2json (R lbls) = makeObj [("record", makeObj (map toRow lbls))]
                              where toRow (l,(_,t)) = (showLabel l, term2json t)
term2json (P t proj) = makeObj [("project", term2json t), ("label", showJSON (showLabel proj))]
term2json (ExtR t1 t2) = makeObj [("term", term2json t1), ("ext", term2json t2)]
term2json (Table t1 t2) = makeObj [("tblhypo", term2json t1), ("tblres", term2json t2)]
term2json (T _ cs) = makeObj [("tblcases", showJSON [(patt2json p, term2json t) | (p,t) <- cs])]
term2json (V ty ts) = makeObj [("tbltype", term2json ty), ("tblvalues", showJSON (map term2json ts))]
term2json (S t1 t2) = makeObj [("select", term2json t1), ("key", term2json t2)]
term2json (Let (v,(_,t1)) t2) = makeObj [("letvar", showJSON v), ("letdef", term2json t1), ("term", term2json t2)]
term2json (Q (m,id))  = makeObj [("mod",showJSON m),("q",  showJSON id)]
term2json (QC (m,id)) = makeObj [("mod",showJSON m),("qc", showJSON id)]
term2json (C t1 t2) = showJSON ((flatten t1 . flatten t2) [])
    where
      flatten Empty     = id
      flatten (C t1 t2) = flatten t1 . flatten t2
      flatten t         = (term2json t :)
term2json (Glue t1 t2) = makeObj [("glue1",term2json t1),("glue2", term2json t2)]
term2json (EPattType t) = makeObj [("patttype",term2json t)]
term2json (ELincat id t) = makeObj [("lincat",showJSON id), ("term",term2json t)]
term2json (ELin id t) = makeObj [("lin",showJSON id), ("term",term2json t)]
term2json (AdHocOverload ts) = makeObj [("overloaded",showJSON (map term2json ts))]
term2json (FV ts) = makeObj [("variants",showJSON (map term2json ts))]
term2json (Markup tag attrs children) = makeObj [ ("tag",showJSON tag)
                                                , ("attrs",showJSON (map (\(attr,val) -> (showJSON attr,term2json val)) attrs))
                                                , ("children",showJSON (map term2json children))
                                                ]
term2json (Reset ctl t) =
  let jctl = case ctl of
               All     -> showJSON "all"
               One     -> showJSON "one"
               Limit n -> showJSON n
               Coordination Nothing    conj cat -> makeObj [("conj",showJSON conj), ("cat",showJSON cat)]
               Coordination (Just mod) conj cat -> makeObj [("mod",showJSON mod), ("conj",showJSON conj), ("cat",showJSON cat)]
  in makeObj [("reset",jctl), ("term",term2json t)]
term2json (Alts def alts) = makeObj [("def",term2json def), ("alts",showJSON (map (\(t1,t2) -> (term2json t1, term2json t2)) alts))]
term2json (Strs ts) = makeObj [("strs",showJSON (map term2json ts))]

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


patt2json (PC id ps)     = makeObj [("pc",showJSON id),("args",showJSON (map patt2json ps))]
patt2json (PP (mn,id) ps) = makeObj [("mod",showJSON mn),("pc",showJSON id),("args",showJSON (map patt2json ps))]
patt2json (PV id)        = makeObj [("pv",showJSON id)]
patt2json PW             = makeObj [("wildcard",showJSON True)]
patt2json (PR lbls)      = makeObj (("record", showJSON True) : map toRow lbls)
                               where toRow (l,t) = (showLabel l, patt2json t)
patt2json (PString s)    = showJSON s
patt2json (PInt n)       = showJSON n
patt2json (PFloat d)     = showJSON d
patt2json (PT ty p)      = makeObj [("type", term2json ty), ("patt", patt2json p)]
patt2json (PAs id p)     = makeObj [("as", showJSON id), ("patt", patt2json p)]
patt2json (PImplArg p)   = makeObj [("implarg", patt2json p)]
patt2json (PTilde t)     = makeObj [("tilde", term2json t)]
patt2json (PNeg p)       = makeObj [("neg", patt2json p)]
patt2json (PAlt p1 p2)   = makeObj [("alt1", patt2json p1), ("alt2", patt2json p2)]
patt2json (PSeq min1 max1 p1 min2 max2 p2)
                           = makeObj [("min1",  showJSON min1)
                                     ,("max1",  showJSON max1)
                                     ,("patt1", patt2json p1)
                                     ,("min2",  showJSON min2)
                                     ,("max2",  showJSON max2)
                                     ,("patt2", patt2json p2)
                                     ]
patt2json (PRep min max p)=makeObj [("min",  showJSON min)
                                   ,("max",  showJSON max)
                                   ,("patt", patt2json p)
                                   ]
patt2json PChar          = makeObj [("char",showJSON True)]
patt2json (PChars cs)    = makeObj [("chars",showJSON cs)]
patt2json (PMacro id)    = makeObj [("macro",showJSON id)]
patt2json (PM (mn,id))   = makeObj [("mod",showJSON mn), ("macro",showJSON id)]

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
