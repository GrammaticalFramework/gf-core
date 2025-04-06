-- -*- haskell -*-
{
{-# OPTIONS -fno-warn-overlapping-patterns #-}
module GF.Grammar.Parser
         ( P, runP, Lang(..), runLangP, runPartial, Posn(..)
         , pModDef
         , pModHeader
         , pTerm
         , pTopDef
         , pBNFCRules
         , pEBNFRules
         , pNLG
         ) where

import GF.Infra.Ident
import GF.Infra.Option
import GF.Data.Operations
import GF.Grammar.Predef
import GF.Grammar.Grammar
import GF.Grammar.BNFC
import GF.Grammar.EBNF
import GF.Grammar.Macros
import GF.Grammar.Lexer
import GF.Compile.Update (buildAnyTree)
import Data.List(intersperse)
import Data.Char(isAlphaNum)
import qualified Data.Map as Map

}

%name pModDef ModDef
%name pTopDef TopDef
%partial pModHeader ModHeader
%partial pTerm Exp
%name pBNFCRules ListCFRule
%name pEBNFRules ListEBNFRule
%name pNLG NLG

-- no lexer declaration
%monad { P } { >>= } { return }
%lexer { lexer } { T_EOF }
%tokentype { Token }


%token 
 '!'          { T_exclmark  }
 '#'          { T_patt      }
 '$'          { T_int_label }
 '('          { T_oparen    }
 ')'          { T_cparen    }
 '~'          { T_tilde     }
 '*'          { T_star      }
 '**'         { T_starstar  }
 '+'          { T_plus      }
 '++'         { T_plusplus  }
 ','          { T_comma     }
 '-'          { T_minus     }
 '->'         { T_rarrow    }
 '.'          { T_dot       }
 '/'          { T_alt       }
 ':'          { T_colon     }
 ';'          { T_semicolon }
 '<'          { T_less      }
 '='          { T_equal     }
 '=>'         { T_big_rarrow}
 '>'          { T_great     }
 '?'          { T_questmark }
 '@'          { T_at        }
 '['          { T_obrack    }
 ']'          { T_cbrack    }
 '[:'         { T_reset     }
 '{'          { T_ocurly    }
 '}'          { T_ccurly    }
 '\\'         { T_lam       }
 '\\\\'       { T_lamlam    }
 '_'          { T_underscore}
 '|'          { T_bar       }
 '::='        { T_cfarrow   }
 'PType'      { T_PType     }
 'Str'        { T_Str       }
 'Strs'       { T_Strs      }
 'Tok'        { T_Tok       }
 'Type'       { T_Type      }
 'abstract'   { T_abstract  }
 'case'       { T_case      }
 'cat'        { T_cat       }
 'concrete'   { T_concrete  }
 'data'       { T_data      }
 'def'        { T_def       }
 'flags'      { T_flags     }
 'fun'        { T_fun       }
 'in'         { T_in        }
 'incomplete' { T_incomplete}
 'instance'   { T_instance  }
 'interface'  { T_interface }
 'let'        { T_let       }
 'lin'        { T_lin       }
 'lincat'     { T_lincat    }
 'lindef'     { T_lindef    }
 'linref'     { T_linref    }
 'of'         { T_of        }
 'open'       { T_open      }
 'oper'       { T_oper      }
 'option'     { T_option    }
 'param'      { T_param     }
 'pattern'    { T_pattern   }
 'pre'        { T_pre       }
 'printname'  { T_printname }
 'resource'   { T_resource  }
 'strs'       { T_strs      }
 'table'      { T_table     }
 'variants'   { T_variants  }
 'where'      { T_where     }
 'with'       { T_with      }
 'coercions'  { T_coercions }
 'terminator' { T_terminator }
 'separator'  { T_separator }
 'nonempty'   { T_nonempty  }

Integer       { (T_Integer $$) }
Double        { (T_Double  $$) }
String        { (T_String  $$) }
Ident         { (T_Ident   $$) }
'<tag'        { (T_open_tag  $$) }
'</tag'       { (T_close_tag $$) }


%%

ModDef :: { SourceModule }
ModDef
  : ComplMod ModType '=' ModBody {%
                                   do let mstat = $1
                                          (mtype,id) = $2
                                          (extends,with,content) = $4
                                          (opens,jments,opts) = case content of { Just c -> c; Nothing -> ([],[],noOptions) }
                                      jments <- mapM (checkInfoType mtype) jments
                                      defs <- buildAnyTree id jments
                                      return (id, ModInfo mtype mstat opts extends with opens [] "" Nothing defs)  }

ModHeader :: { SourceModule }
ModHeader
  : ComplMod ModType '=' ModHeaderBody { let { mstat = $1 ;
                                               (mtype,id) = $2 ;
                                               (extends,with,opens) = $4 }
                                         in (id, ModInfo mtype mstat noOptions extends with opens [] "" Nothing Map.empty) }

ComplMod :: { ModuleStatus }
ComplMod 
  : {- empty -}  { MSComplete   } 
  | 'incomplete' { MSIncomplete }

ModType :: { (ModuleType,ModuleName) }
ModType
  : 'abstract'  ModuleName                     { (MTAbstract,      $2) }
  | 'resource'  ModuleName                     { (MTResource,      $2) }
  | 'interface' ModuleName                     { (MTInterface,     $2) }
  | 'concrete'  ModuleName 'of' ModuleName     { (MTConcrete $4,   $2) }
  | 'instance'  ModuleName 'of' Included       { (MTInstance $4,   $2) }

ModHeaderBody :: { ( [(ModuleName,MInclude)]
                   , Maybe (ModuleName,MInclude,[(ModuleName,ModuleName)])
                   , [OpenSpec]
                   ) }
ModHeaderBody
  : ListIncluded '**' Included 'with' ListInst '**' ModOpen { ($1, Just (fst $3,snd $3,$5), $7) }
  | ListIncluded '**' Included 'with' ListInst              { ($1, Just (fst $3,snd $3,$5), []) }
  | ListIncluded                               '**' ModOpen { ($1, Nothing,                 $3) }
  | ListIncluded                                            { ($1, Nothing,                 []) }
  |                   Included 'with' ListInst '**' ModOpen { ([], Just (fst $1,snd $1,$3), $5) }
  |                   Included 'with' ListInst              { ([], Just (fst $1,snd $1,$3), []) }
  |                                                 ModOpen { ([], Nothing,                 $1) }

ModOpen :: { [OpenSpec] }
ModOpen
  :                 { [] }
  | 'open' ListOpen { $2 }

ModBody :: { ( [(ModuleName,MInclude)]
             , Maybe (ModuleName,MInclude,[(ModuleName,ModuleName)])
             , Maybe ([OpenSpec],[(Ident,Info)],Options)
             ) }
ModBody
  : ListIncluded '**' Included 'with' ListInst '**' ModContent  { ($1, Just (fst $3,snd $3,$5), Just $7) }
  | ListIncluded '**' Included 'with' ListInst                  { ($1, Just (fst $3,snd $3,$5), Nothing) }
  | ListIncluded                               '**' ModContent  { ($1, Nothing,                 Just $3) }
  | ListIncluded                                                { ($1, Nothing,                 Nothing) }
  |                   Included 'with' ListInst '**' ModContent  { ([], Just (fst $1,snd $1,$3), Just $5) }
  |                   Included 'with' ListInst                  { ([], Just (fst $1,snd $1,$3), Nothing) }
  |                                                 ModContent  { ([], Nothing,                 Just $1) }
  | ModBody ';'                                                 { $1                                     }

ModContent :: { ([OpenSpec],[(Ident,Info)],Options) }
ModContent
  :                      '{' ListTopDef '}' { ([],[d | Left ds <- $2, d <- ds],concatOptions [o | Right o <- $2]) }
  | 'open' ListOpen 'in' '{' ListTopDef '}' { ($2,[d | Left ds <- $5, d <- ds],concatOptions [o | Right o <- $5]) }

ListTopDef :: { [Either [(Ident,Info)] Options] }
ListTopDef
  : {- empty -}       { []      } 
  | TopDef ListTopDef { $1 : $2 }

ListOpen :: { [OpenSpec] }
ListOpen
  : Open              { [$1]    }
  | Open ',' ListOpen { $1 : $3 }

Open :: { OpenSpec }
Open
  : ModuleName                   { OSimple $1    }
  | '(' ModuleName '=' ModuleName ')' { OQualif $2 $4 }

ListInst :: { [(ModuleName,ModuleName)] }
ListInst
  : Inst              { [$1]    }
  | Inst ',' ListInst { $1 : $3 }

Inst :: { (ModuleName,ModuleName) }
Inst
  : '(' ModuleName '=' ModuleName ')' { ($2,$4) }

ListIncluded :: { [(ModuleName,MInclude)] }
ListIncluded
  : Included                  { [$1]    }
  | Included ',' ListIncluded { $1 : $3 }

Included :: { (ModuleName,MInclude) }
Included 
  : ModuleName                       { ($1,MIAll      ) }
  | ModuleName     '[' ListIdent ']' { ($1,MIOnly   $3) }
  | ModuleName '-' '[' ListIdent ']' { ($1,MIExcept $4) }

TopDef :: { Either [(Ident,Info)] Options }
TopDef
  : 'cat'             ListCatDef      { Left  $2 }
  | 'fun'             ListFunDef      { Left  $2 }
  | 'def'             ListDefDef      { Left  $2 }
  | 'data'            ListDataDef     { Left  $2 }
  | 'param'           ListParamDef    { Left  $2 }
  | 'oper'            ListOperDef     { Left  $2 }
  | 'lincat'          ListTermDef     { Left  [(f, CncCat (Just e) Nothing  Nothing  Nothing Nothing) | (f,e) <- $2] }
  | 'lindef'          ListTermDef     { Left  [(f, CncCat Nothing  (Just e) Nothing  Nothing Nothing) | (f,e) <- $2] }
  | 'linref'          ListTermDef     { Left  [(f, CncCat Nothing  Nothing  (Just e) Nothing Nothing) | (f,e) <- $2] }
  | 'lin'             ListLinDef      { Left  $2 }
  | 'printname' 'cat' ListTermDef     { Left  [(f, CncCat Nothing Nothing Nothing (Just e) Nothing) | (f,e) <- $3] }
  | 'printname' 'fun' ListTermDef     { Left  [(f, CncFun Nothing Nothing (Just e) Nothing) | (f,e) <- $3] }
  | 'flags'           ListFlagDef     { Right $2 }

CatDef :: { [(Ident,Info)] }
CatDef
  : Posn Ident ListDDecl                         Posn { [($2, AbsCat (Just (mkL $1 $4 $3)))]           }
  | Posn '[' Ident ListDDecl ']'                 Posn { listCatDef (mkL $1 $6 ($3,$4,0))               }
  | Posn '[' Ident ListDDecl ']' '{' Integer '}' Posn { listCatDef (mkL $1 $9 ($3,$4,fromIntegral $7)) }

FunDef :: { [(Ident,Info)] }
FunDef
  : Posn ListIdent ':' Exp Posn { [(fun, AbsFun (Just (mkL $1 $5 $4)) Nothing (Just []) (Just True)) | fun <- $2] } 

DefDef :: { [(Ident,Info)] }
DefDef
  : Posn LhsNames '=' Exp         Posn { [(f, AbsFun Nothing (Just 0)           (Just [mkL $1 $5 ([],$4)]) Nothing) | f <- $2] }
  | Posn LhsName ListPatt '=' Exp    Posn { [($2,AbsFun Nothing (Just (length $3)) (Just [mkL $1 $6 ($3,$5)]) Nothing)] }

DataDef :: { [(Ident,Info)] }
DataDef
  : Posn Ident '=' ListDataConstr Posn { ($2,   AbsCat Nothing) :
                                         [(fun, AbsFun Nothing Nothing Nothing  (Just True)) | fun <- $4] }
  | Posn ListIdent ':' Exp Posn        { -- (snd (valCat $4), AbsCat Nothing) :
                                         [(fun, AbsFun (Just (mkL $1 $5 $4)) Nothing Nothing (Just True)) | fun <- $2] }                                         

ParamDef :: { [(Ident,Info)] }
ParamDef
  : Posn LhsIdent '=' ListParConstr Posn { ($2, ResParam (Just (mkL $1 $5 [param | L loc param <- $4])) Nothing) :
                                           [(f, ResValue (L loc (mkProdSimple co (Cn $2))) 0) | L loc (f,co) <- $4] }
  | Posn LhsIdent                   Posn { [($2, ResParam Nothing Nothing)] }

OperDef :: { [(Ident,Info)] }
OperDef
  : Posn LhsNames ':' Exp ';'        Posn { [(i, info) | i <- $2,   info <- mkOverload (Just (mkL $1 $6 $4)) Nothing  ] }
  | Posn LhsNames '=' Markup         Posn { [(i, info) | i <- $2,   info <- mkOverload Nothing   (Just (mkL $1 $5 $4))] }
  | Posn LhsName ListArg '=' Markup  Posn { [(i, info) | i <- [$2], info <- mkOverload Nothing   (Just (mkL $1 $6 (mkAbs $3 $5)))] }
  | Posn LhsNames ':' Exp '=' Markup Posn { [(i, info) | i <- $2,   info <- mkOverload (Just (mkL $1 $7 $4)) (Just (mkL $1 $7 $6))] }

LinDef :: { [(Ident,Info)] }
LinDef
  : Posn LhsNames '=' Exp         Posn { [(f,  CncFun Nothing (Just (mkL $1 $5 $4)) Nothing Nothing) | f <- $2] }
  | Posn LhsName ListArg '=' Exp     Posn { [($2, CncFun Nothing (Just (mkL $1 $6 (mkAbs $3 $5))) Nothing Nothing)] }

TermDef :: { [(Ident,L Term)] }
TermDef
  : Posn LhsNames '=' Exp Posn { [(i,mkL $1 $5 $4) | i <- $2] }

FlagDef :: { Options }
FlagDef
  : Posn Ident '=' Ident Posn  {% case parseModuleOptions ["--" ++ showIdent $2 ++ "=" ++ showIdent $4] of
                                    Ok  x   -> return x
                                    Bad msg -> failLoc $1 msg                                           } 
  | Posn Ident '=' Double Posn {% case parseModuleOptions ["--" ++ showIdent $2 ++ "=" ++ show $4] of
                                    Ok  x   -> return x
                                    Bad msg -> failLoc $1 msg                                           } 

ListDataConstr :: { [Ident] }
ListDataConstr
  : Ident                    { [$1]    }
  | Ident '|' ListDataConstr { $1 : $3 }

ParConstr :: { L Param }
ParConstr
  : Posn Ident ListDDecl Posn { mkL $1 $4 ($2,$3) } 

ListLinDef :: { [(Ident,Info)] }
ListLinDef
  : LinDef ';'            { $1       } 
  | LinDef ';' ListLinDef { $1 ++ $3 }

ListDefDef :: { [(Ident,Info)] }
ListDefDef
  : DefDef ';'            { $1       } 
  | DefDef ';' ListDefDef { $1 ++ $3 }

ListOperDef :: { [(Ident,Info)] }
ListOperDef
  : OperDef             { $1       }
  | OperDef ListOperDef { $1 ++ $2 }

ListCatDef :: { [(Ident,Info)] }
ListCatDef
  : CatDef ';'            { $1       } 
  | CatDef ';' ListCatDef { $1 ++ $3 }

ListFunDef :: { [(Ident,Info)] }
ListFunDef
  : FunDef ';'            { $1       }
  | FunDef ';' ListFunDef { $1 ++ $3 }

ListDataDef :: { [(Ident,Info)] }
ListDataDef
  : DataDef ';'             { $1       } 
  | DataDef ';' ListDataDef { $1 ++ $3 }

ListParamDef :: { [(Ident,Info)] }
ListParamDef
  : ParamDef ';'              { $1       } 
  | ParamDef ';' ListParamDef { $1 ++ $3 }

ListTermDef :: { [(Ident,L Term)] }
ListTermDef
  : TermDef ';'             { $1       } 
  | TermDef ';' ListTermDef { $1 ++ $3 }

ListFlagDef :: { Options }
ListFlagDef
  : FlagDef ';'               { $1               } 
  | FlagDef ';' ListFlagDef   { addOptions $1 $3 }

ListParConstr :: { [L Param] }
ListParConstr
  : ParConstr                   { [$1]    }
  | ParConstr '|' ListParConstr { $1 : $3 }

ListIdent :: { [Ident] }
ListIdent
  : Ident               { [$1]    } 
  | Ident ',' ListIdent { $1 : $3 }

ListIdent2 :: { [Ident] }
ListIdent2 
  : Ident               { [$1]    } 
  | Ident ListIdent2    { $1 : $2 }

LhsIdent :: { Ident }
  : Ident     { $1 }
  | Posn Sort {% failLoc $1 (showIdent $2++ " is a predefined constant, it can not be redefined") }

LhsName :: { Ident }
LhsName
  : LhsIdent         { $1          }
  | '[' LhsIdent ']' { mkListId $2 }

LhsNames :: { [Ident] }
LhsNames
  : LhsName              { [$1]    }
  | LhsName ',' LhsNames { $1 : $3 }

LocDef :: { [(Ident, Maybe Type, Maybe Term)] }
LocDef
  : ListIdent ':' Exp         { [(lab,Just $3,Nothing) | lab <- $1] } 
  | ListIdent '=' Exp         { [(lab,Nothing,Just $3) | lab <- $1] }
  | ListIdent ':' Exp '=' Exp { [(lab,Just $3,Just $5) | lab <- $1] }

LocMarkupDef :: { [(Ident, Maybe Type, Maybe Term)] }
LocMarkupDef
  : ListIdent '=' Tag         { [(lab,Nothing,Just $3) | lab <- $1] }
  | ListIdent ':' Exp '=' Tag { [(lab,Just $3,Just $5) | lab <- $1] }

ListLocDef :: { [(Ident, Maybe Type, Maybe Term)] }
ListLocDef
  : {- empty -}             { []       }
  | LocDef                  { $1       }
  | LocMarkupDef ListLocDef { $1 ++ $2 }
  | LocDef ';'   ListLocDef { $1 ++ $3 }

Exp :: { Term }
Exp
  : Exp1 '|' Exp                      { case ($1,$3) of
                                          (FV xs,FV ys) -> FV (xs++ys )
                                          (FV xs,y    ) -> FV (xs++[y])
                                          (x,    FV ys) -> FV (x:ys)
                                          (x,    y    ) -> FV [x,y]
                                      }
  | '\\'   ListBind '->' Exp          { mkAbs $2 $4 }
  | '\\\\' ListBind '=>' Exp          { mkCTable $2 $4 }
  | Decl '->' Exp                     { mkProdSimple $1 $3 }
  | Exp3 '=>' Exp                     { Table $1 $3 }
  | 'let' '{' ListLocDef '}' 'in' Exp {%
                                        do defs <- mapM tryLoc $3
                                           return $ mkLet defs $6 }
  | 'let' ListLocDef 'in' Exp         {%
                                        do defs <- mapM tryLoc $2
                                           return $ mkLet defs $4 }
  | 'let' ListLocDef 'in' Tag         {%
                                        do defs <- mapM tryLoc $2
                                           return $ mkLet defs $4 }
  | Exp3 'where' '{' ListLocDef '}'   {%
                                        do defs <- mapM tryLoc $4
                                           return $ mkLet defs $1 }
  | 'in' Exp5 String                  { Example $2 $3 }
  | Exp1                              { $1 }

Exp1 :: { Term }
Exp1
  : Exp2 '++' Exp1 { C $1 $3 } 
  | Exp2           { $1      }

Exp2 :: { Term }
Exp2
  : Exp3 '+' Exp2 { Glue $1 $3 } 
  | Exp3          { $1         }

Exp3 :: { Term }
Exp3
  : Exp3 '!' Exp4                    { S $1 $3       } 
  | 'table' '{' ListCase '}'         { T TRaw $3     }
  | 'table' Exp6 '{' ListCase '}'    { T (TTyped $2) $4 }
  | 'table' Exp6 '[' ListExp ']'     { V $2 $4       }
  | Exp3 '*'  Exp4                   { case $1 of
                                         RecType xs -> RecType (xs ++ [(tupleLabel (length xs+1),$3)])
                                         t          -> RecType [(tupleLabel 1,$1), (tupleLabel 2,$3)]  }
  | Exp3 '**' Exp4                   { ExtR $1 $3    }
  | Exp4                             { $1            }

Exp4 :: { Term }
Exp4
  : Exp4 Exp5                        { App $1 $2 }
  | Exp4 '{' Exp '}'                 { App $1 (ImplArg $3) } 
  | 'option' Exp 'of' '{' ListOpt '}' { Opts $2 $5 }
  | 'case' Exp 'of' '{' ListCase '}' { let annot = case $2 of
                                             Typed _ t -> TTyped t
                                             _         -> TRaw
                                       in S (T annot $5) $2         }
  | 'variants' '{' ListExp '}'       { FV $3         }
  | 'pre' '{' ListCase '}'           {% mkAlts $3     }
  | 'pre' '{' String ';' ListAltern '}' { Alts (K $3) $5 }
  | 'pre' '{' Ident ';' ListAltern '}' { Alts (Vr $3) $5 }
  | 'strs' '{' ListExp '}'           { Strs $3       }
  | '#' Patt3                        { EPatt 0 Nothing $2 }
  | 'pattern' Exp5                   { EPattType $2  }
  | 'lincat' Ident Exp5              { ELincat $2 $3 }
  | 'lin' Ident Exp5                 { ELin $2 $3 }
  | Exp5                             { $1            }

Exp5 :: { Term }
Exp5
  : Exp5 '.' Label            { P  $1 $3 } 
  | Exp6                      { $1       }

Exp6 :: { Term }
Exp6 
  : Ident                 { Vr $1 } 
  | Sort                  { Sort $1 }
  | String                { K $1 }
  | Integer               { EInt $1 }
  | Double                { EFloat $1 }
  | '?'                   { Meta 0 }
  | '[' ']'               { Empty }
  | '[' Ident Exps ']'    { foldl App (Vr (mkListId $2)) $3 }
  | '[' String ']'        { K $2 }
  | '{' ListLocDef '}'    {% mkR $2 }
  | '<' ListTupleComp '>' { R (tuple2record $2) }
  | '<' Exp ':' Exp '>'   { Typed $2 $4      }
  | '[:' Control '|' Tag ']'      { Reset $2 $4 }
  | '[:' Control '|' Exp ']'      { Reset $2 $4 }
  | '(' Exp ')'           { $2 }

ListExp :: { [Term] }
ListExp
  : {- empty -}     { []      } 
  | Exp             { [$1]    }
  | Exp ';' ListExp { $1 : $3 }

Exps :: { [Term] }
Exps
  : {- empty -}     { []      } 
  | Exp6 Exps       { $1 : $2 }

Patt :: { Patt }
Patt
  : Patt '|' Patt1            { PAlt $1 $3 } 
  | Patt '+' Patt1            { PSeq 0 Nothing $1 0 Nothing $3 }
  | Patt1                     { $1         }

Patt1 :: { Patt }
Patt1
  : Ident ListPatt            { PC $1 $2 } 
  | ModuleName '.' Ident ListPatt  { PP ($1,$3) $4 }
  | Patt3 '*'                 { PRep 0 Nothing $1 }
  | Patt2                     { $1 }

Patt2 :: { Patt }
Patt2
  : Ident '@' Patt3           { PAs $1 $3 }
  | '-' Patt3                 { PNeg $2 }
  | '~' Exp6                  { PTilde $2 }
  | Patt3                     { $1 } 

Patt3 :: { Patt }
Patt3
  : '?'                       { PChar } 
  | '[' String ']'            { PChars $2 }
  | '#' Ident                 { PMacro $2 }
  | '#' ModuleName '.' Ident  { PM ($2,$4) }
  | '_'                       { PW }
  | Ident                     { PV $1 }
  | ModuleName '.' Ident      { PP ($1,$3) [] }
  | Integer                   { PInt $1 }
  | Double                    { PFloat  $1 }
  | String                    { PString $1 }
  | '{' ListPattAss '}'       { PR $2 }
  | '<' ListPattTupleComp '>' { (PR . tuple2recordPatt) $2 }
  | '(' Patt ')'              { $2 }

PattAss :: { [(Label,Patt)] }
PattAss
  : ListIdent '=' Patt { [(LIdent (ident2raw i),$3) | i <- $1] } 

Label :: { Label }
Label
  : Ident       { LIdent (ident2raw $1)   } 
  | '$' Integer { LVar (fromIntegral $2) }

Sort :: { Ident }
Sort
  : 'Type'  { cType  } 
  | 'PType' { cPType }
  | 'Tok'   { cTok   }
  | 'Str'   { cStr   }
  | 'Strs'  { cStrs  }

ListPattAss :: { [(Label,Patt)] }
ListPattAss
  : {- empty -}             { []       }
  | PattAss                 { $1       }
  | PattAss ';' ListPattAss { $1 ++ $3 }

ListPatt :: { [Patt] }
ListPatt
  : PattArg          { [$1]    } 
  | PattArg ListPatt { $1 : $2 }

PattArg :: { Patt }
  : Patt2         { $1                }
  | '{' Patt '}'  { PImplArg $2       }

Arg :: { [(BindType,Ident)] }
Arg 
  : Ident               { [(Explicit,$1    )]      }
  | '_'                 { [(Explicit,identW)]      }
  | '{' ListIdent2 '}'  { [(Implicit,v) | v <- $2] }
  
ListArg :: { [(BindType,Ident)] }
ListArg
  : Arg                 { $1       } 
  | Arg ListArg         { $1 ++ $2 }

Bind :: { [(BindType,Ident)] }
Bind
  : Ident               { [(Explicit,$1    )]      } 
  | '_'                 { [(Explicit,identW)]      }
  | '{' ListIdent '}'   { [(Implicit,v) | v <- $2] }

ListBind :: { [(BindType,Ident)] }
ListBind
  : Bind                { $1       }
  | Bind ',' ListBind   { $1 ++ $3 }

Decl :: { [Hypo] }
Decl
  : '(' ListBind ':' Exp ')' { [(b,x,$4) | (b,x) <- $2] } 
  | Exp3                     { [mkHypo $1]              }

ListTupleComp :: { [Term] }
ListTupleComp
  : {- empty -}           { []      } 
  | Exp                   { [$1]    }
  | Exp ',' ListTupleComp { $1 : $3 }

ListPattTupleComp :: { [Patt] }
ListPattTupleComp
  : {- empty -}                { []      } 
  | Patt                       { [$1]    }
  | Patt ',' ListPattTupleComp { $1 : $3 }

Opt :: { Option }
Opt
  : '(' Exp ')' '=>' Exp { ($2,$5) }

ListOpt :: { [Option] }
ListOpt
  : Opt             { [$1]    }
  | Opt ';' ListOpt { $1 : $3 }

Case :: { Case }
Case
  : Patt '=>' Exp { ($1,$3) } 

ListCase :: { [Case] }
ListCase
  : Case              { [$1]    } 
  | Case ';' ListCase { $1 : $3 }

Altern :: { (Term,Term) }
Altern
  : Exp '/' Exp { ($1,$3) } 

ListAltern :: { [(Term,Term)] }
ListAltern
  : Altern                { [$1]    }
  | Altern ';' ListAltern { $1 : $3 }

DDecl :: { [Hypo] }
DDecl
  : '(' ListBind ':' Exp ')' { [(b,x,$4) | (b,x) <- $2] } 
  | Exp6                     { [mkHypo $1]              }

ListDDecl :: { [Hypo] }
ListDDecl
  : {- empty -}     { []       } 
  | DDecl ListDDecl { $1 ++ $2 }

ListCFRule :: { [BNFCRule] }
ListCFRule
  : CFRule            { $1       }
  | CFRule ListCFRule { $1 ++ $2 }

CFRule :: { [BNFCRule] }
CFRule
  : Ident '.' Ident '::=' ListCFSymbol ';' { [BNFCRule (showIdent $3) $5 (CFObj (showIdent $1) [])]
                                           }
  | Ident '::=' ListCFRHS ';'              { let { cat = showIdent $1;
                                                   mkFun cat its =
                                                     case its of {
                                                       [] -> cat ++ "_";
                                                       _  -> concat $ intersperse "_" (cat : filter (not . null) (map clean its)) -- CLE style
                                                     };
                                                   clean sym = 
                                                     case sym of {
                                                       Terminal    c     -> filter isAlphaNum c;
                                                       NonTerminal (t,_) -> t
                                                     }
                                             } in map (\rhs -> BNFCRule cat rhs (CFObj (mkFun cat rhs) [])) $3
                                           }
  | 'coercions' Ident Integer ';'          { [BNFCCoercions (showIdent $2) $3]}
  | 'terminator' NonEmpty Ident String ';' { [BNFCTerminator $2 (showIdent $3) $4] }
  | 'separator'  NonEmpty Ident String ';' { [BNFCSeparator $2 (showIdent $3) $4] }

ListCFRHS :: { [[BNFCSymbol]] }
ListCFRHS
  : ListCFSymbol                { [$1]    }
  | ListCFSymbol '|' ListCFRHS  { $1 : $3 }

ListCFSymbol :: { [BNFCSymbol] }
ListCFSymbol
  : {- empty -}           { []      } 
  | CFSymbol ListCFSymbol { $1 : $2 }

CFSymbol :: { BNFCSymbol }
  : String                { Terminal $1 }
  | Ident                 { NonTerminal (showIdent $1, False) }
  | '[' Ident ']'         { NonTerminal (showIdent $2, True)  }

NonEmpty :: { Bool }
NonEmpty : 'nonempty' { True }
        | {-empty-}  { False }


ListEBNFRule :: { [ERule] }
ListEBNFRule
  : EBNFRule              { [$1]    }
  | EBNFRule ListEBNFRule { $1 : $2 }

EBNFRule :: { ERule }
  : Ident '::=' ERHS0 ';' { ((showIdent $1,[]),$3) }

ERHS0 :: { ERHS }
  : ERHS1                 { $1         }
  | ERHS1 '|' ERHS0       { EAlt $1 $3 }

ERHS1 :: { ERHS }
  : ERHS2                 { $1         }
  | ERHS2 ERHS1           { ESeq $1 $2 }

ERHS2 :: { ERHS }
  : ERHS3 '*'             { EStar $1 }
  | ERHS3 '+'             { EPlus $1 }
  | ERHS3 '?'             { EOpt $1  }
  | ERHS3                 { $1       }

ERHS3 :: { ERHS }
  : String                { ETerm $1 }
  | Ident                 { ENonTerm (showIdent $1,[]) }
  | '(' ERHS0 ')'         { $2         }

NLG :: { Map.Map Ident Info }
  : ListNLGDef     { Map.fromList $1 }
  | Posn Tag Posn  { Map.singleton (identS "main") (ResOper Nothing (Just (mkL $1 $3 $2))) }
  | Posn Exp Posn  { Map.singleton (identS "main") (ResOper Nothing (Just (mkL $1 $3 $2))) }

ListNLGDef :: { [(Ident,Info)] }
ListNLGDef
  : {- empty -}               { []       }
  | 'oper' OperDef ListNLGDef { $2 ++ $3 }

Markup :: { Term }
Markup
  : Tag     { $1 }
  | Exp ';' { $1 }

Tag :: { Term }
Tag
  : '<tag' Attributes '>' ListMarkup '</tag' '>'  {% if $1 == $5
                                                       then return (Markup $1 $2 $4)
                                                       else fail ("Unmatched closing tag " ++ showIdent $1) }
  | '<tag' Attributes '/' '>'                     { Markup $1 $2 [] }

ListMarkup :: { [Term] }
  :                    { []      }
  | Exp                { [$1]    }
  | Markup ListMarkup  { $1 : $2 }

Control :: { Control }
  :            { All                     }
  | Integer    { Limit (fromIntegral $1) }
  | Ident      { Coordination Nothing $1 identW }

Attributes :: { [(Ident,Term)] }
Attributes
  :                       { []    }
  | Attribute Attributes  { $1:$2 }

Attribute :: { (Ident,Term) }
Attribute
  : Ident '=' Exp6   { ($1,$3) }

ModuleName :: { ModuleName }
  : Ident           { MN $1 }

Posn :: { Posn }
Posn
  : {- empty -}     {% getPosn } 


{

happyError :: P a
happyError = fail "syntax error"

mkListId,mkConsId,mkBaseId  :: Ident -> Ident
mkListId = prefixIdent "List"
mkConsId = prefixIdent "Cons"
mkBaseId = prefixIdent "Base"

listCatDef :: L (Ident, Context, Int) -> [(Ident,Info)]
listCatDef (L loc (id,cont,size)) = [catd,nilfund,consfund]
  where
    listId = mkListId id
    baseId = mkBaseId id
    consId = mkConsId id

    catd     = (listId, AbsCat (Just (L loc cont')))
    nilfund  = (baseId, AbsFun (Just (L loc niltyp))  Nothing Nothing (Just True))
    consfund = (consId, AbsFun (Just (L loc constyp)) Nothing Nothing (Just True))

    cont' = [(b,mkId x i,ty) | (i,(b,x,ty)) <- zip [0..] cont]
    xs = map (\(b,x,t) -> Vr x) cont'
    cd = mkHypo (mkApp (Vr id) xs)
    lc = mkApp (Vr listId) xs

    niltyp  = mkProdSimple (cont' ++ replicate size cd) lc
    constyp = mkProdSimple (cont' ++ [cd, mkHypo lc]) lc

    mkId x i = if x == identW then (varX i) else x

tryLoc (c,mty,Just e) = return (c,(mty,e))
tryLoc (c,_  ,_     ) = fail ("local definition of" +++ showIdent c +++ "without value")

mkR []       = return $ RecType [] --- empty record always interpreted as record type
mkR fs@(f:_) =
  case f of
    (lab,Just ty,Nothing) -> mapM tryRT fs >>= return . RecType
    _                     -> mapM tryR  fs >>= return . R
  where
    tryRT (lab,Just ty,Nothing) = return (ident2label lab,ty)
    tryRT (lab,_      ,_      ) = fail $ "illegal record type field" +++ showIdent lab --- manifest fields ?!

    tryR (lab,mty,Just t) = return (ident2label lab,(mty,t))
    tryR (lab,_  ,_     ) = fail $ "illegal record field" +++ showIdent lab

mkOverload pdt pdf@(Just (L loc df)) =
  case appForm df of
    (keyw, ts@(_:_)) | isOverloading keyw -> 
       case last ts of
         R fs -> [ResOverload [MN m | Vr m <- ts] [(L loc ty,L loc fu) | (_,(Just ty,fu)) <- fs]]
         _    -> [ResOper pdt pdf]
    _         -> [ResOper pdt pdf]

     -- to enable separare type signature --- not type-checked
mkOverload pdt@(Just (L _ df)) pdf =
  case appForm df of
    (keyw, ts@(_:_)) | isOverloading keyw ->
       case last ts of
         RecType _ -> [] 
         _         -> [ResOper pdt pdf]
    _              -> [ResOper pdt pdf]
mkOverload pdt pdf = [ResOper pdt pdf]

isOverloading t =
  case t of
    Vr keyw | showIdent keyw == "overload" -> True      -- overload is a "soft keyword"
    _                                      -> False

checkInfoType mt jment@(id,info) =
  case info of
    AbsCat pcont         -> ifAbstract mt (locPerh pcont)
    AbsFun pty _ pde _   -> ifAbstract mt (locPerh pty ++ maybe [] locAll pde)
    CncCat pty pd pr ppn _->ifConcrete mt (locPerh pty ++ locPerh pd ++ locPerh pr ++ locPerh ppn)
    CncFun _   pd ppn _  -> ifConcrete mt (locPerh pd ++ locPerh ppn)
    ResParam pparam _    -> ifResource mt (locPerh pparam)
    ResValue ty _        -> ifResource mt (locL ty)
    ResOper  pty pt      -> ifOper mt pty pt
    ResOverload _ xs     -> ifResource mt (concat [[loc1,loc2] | (L loc1 _,L loc2 _) <- xs])
  where
    locPerh = maybe [] locL
    locAll xs = [loc | L loc x <- xs]
    locL (L loc x) = [loc]
    
    illegal (Local s e:_) = failLoc (Pn s 0) "illegal definition"
    illegal _             = return jment

    ifAbstract MTAbstract     locs = return jment
    ifAbstract _              locs = illegal locs

    ifConcrete (MTConcrete _) locs = return jment
    ifConcrete _              locs = illegal locs

    ifResource (MTConcrete _) locs = return jment
    ifResource (MTInstance _) locs = return jment
    ifResource MTInterface    locs = return jment
    ifResource MTResource     locs = return jment
    ifResource _              locs = illegal locs
    
    ifOper MTAbstract pty pt = return (id,AbsFun pty (fmap (const 0) pt) (Just (maybe [] (\(L l t) -> [L l ([],t)]) pt)) (Just False))
    ifOper _          pty pt = return jment

mkAlts cs = case cs of
  _:_ -> do
    def  <- mkDef (last cs)
    alts <- mapM mkAlt (init cs)
    return (Alts def alts)
  _ -> fail "empty alts"
 where
   mkDef (_,t) = return t
   mkAlt (p,t) = do
     ss <- mkStrs p
     return (t,ss)

mkL :: Posn -> Posn -> x -> L x
mkL (Pn l1 _) (Pn l2 _) x = L (Local l1 l2) x

}
