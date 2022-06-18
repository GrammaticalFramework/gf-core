module GF.Server.SimpleEditor.JSON where

import Text.JSON

import GF.Server.SimpleEditor.Syntax


instance JSON Grammar where
  showJSON (Grammar name extends abstract concretes) =
    makeObj ["basename".=name, "extends".=extends,
             "abstract".=abstract, "concretes".=concretes]
  readJSON = error "Grammar.readJSON intentionally not defined"

instance JSON Abstract where
  showJSON (Abstract startcat cats funs) =
    makeObj ["startcat".=startcat, "cats".=cats, "funs".=funs]
  readJSON = error "Abstract.readJSON intentionally not defined"

instance JSON Fun where 
  showJSON (Fun   name typ) = signature  name typ
  readJSON = error "Fun.readJSON intentionally not defined"

instance JSON Param where 
  showJSON (Param name rhs) = definition name rhs
  readJSON = error "Param.readJSON intentionally not defined"

instance JSON Oper where 
  showJSON (Oper  name rhs) = definition name rhs
  readJSON = error "Oper.readJSON intentionally not defined"

signature  name typ = makeObj ["name".=name,"type".=typ]
definition name rhs = makeObj ["name".=name,"rhs".=rhs]

instance JSON Concrete where
  showJSON (Concrete langcode opens params lincats opers lins) =
    makeObj ["langcode".=langcode, "opens".=opens,
              "params".=params, "opers".=opers,
             "lincats".=lincats, "lins".=lins]
  readJSON = error "Concrete.readJSON intentionally not defined"

instance JSON Lincat where
  showJSON (Lincat cat lintype) = makeObj ["cat".=cat, "type".=lintype]
  readJSON = error "Lincat.readJSON intentionally not defined"

instance JSON Lin where
  showJSON (Lin fun args lin) = makeObj ["fun".=fun, "args".=args, "lin".=lin]
  readJSON = error "Lin.readJSON intentionally not defined"

infix 1 .=
name .= v = (name,showJSON v)
