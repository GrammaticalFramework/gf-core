{-# LANGUAGE BangPatterns #-}
-------------------------------------------------
-- |
-- Module      : PGF
-- Maintainer  : Krasimir Angelov
-- Stability   : stable
-- Portability : portable
--
-- This module is an Application Programming Interface to
-- load and interpret grammars compiled in Portable Grammar Format (PGF).
-- The PGF format is produced as a final output from the GF compiler.
-- The API is meant to be used for embedding GF grammars in Haskell
-- programs
-------------------------------------------------

module PGF(
           -- * PGF
           PGF,
           readPGF,

           -- * Identifiers
           CId, mkCId, wildCId,
           showCId, readCId,
           -- extra
           ppCId, PGF2.pIdent,

           -- * Languages
           Language, 
           showLanguage, readLanguage,
           languages, abstractName, languageCode,

           -- * Types
           Type, Hypo, BindType(..),
           PGF2.showType, PGF2.readType,
           mkType, PGF2.mkHypo, mkDepHypo, mkImplHypo,
           unType,
           categories, categoryContext, PGF2.startCat,

           -- * Functions
           PGF2.functions, PGF2.functionsByCat, PGF2.functionType,

           -- * Expressions & Trees
           -- ** Tree
           Tree,

           -- ** Expr
           Expr,
           PGF2.showExpr, PGF2.readExpr, PGF2.pExpr,
           mkAbs,         unAbs,
           mkApp,         unApp, PGF2.unapply,
           PGF2.mkStr,    PGF2.unStr,
           PGF2.mkInt,    PGF2.unInt,
           PGF2.mkDouble, PGF2.unDouble,
           PGF2.mkFloat,  PGF2.unFloat,
           PGF2.mkMeta,   PGF2.unMeta,
           -- extra
           PGF2.exprSize, PGF2.exprFunctions,

           -- * Operations
           -- ** Linearization
           linearize, linearizeAllLang, linearizeAll, bracketedLinearize, {-bracketedLinearizeAll,-} tabularLinearizes,
           showPrintName,

           BracketedString(..), FId, LIndex, Token,
           showBracketedString,flattenBracketedString,

           -- ** Parsing
           parse, parseAllLang, parseAll, complete,

           -- ** Evaluation
           {- PGF.compute, paraphrase,-}

           -- ** Type Checking
           -- | The type checker in PGF does both type checking and renaming
           -- i.e. it verifies that all identifiers are declared and it
           -- distinguishes between global function or type indentifiers and
           -- variable names. The type checker should always be applied on
           -- expressions entered by the user i.e. those produced via functions
           -- like 'readType' and 'readExpr' because otherwise unexpected results
           -- could appear. All typechecking functions returns updated versions
           -- of the input types or expressions because the typechecking could
           -- also lead to metavariables instantiations.
           PGF2.checkType, PGF2.checkExpr, PGF2.inferExpr,

           -- ** Generation
           -- | The PGF interpreter allows automatic generation of
           -- abstract syntax expressions of a given type. Since the
           -- type system of GF allows dependent types, the generation
           -- is in general undecidable. In fact, the set of all type
           -- signatures in the grammar is equivalent to a Turing-complete language (Prolog).
           --
           -- There are several generation methods which mainly differ in:
           --
           --     * whether the expressions are sequentially or randomly generated?
           --
           --     * are they generated from a template? The template is an expression
           --     containing meta variables which the generator will fill in.
           --
           --     * is there a limit of the depth of the expression?
           --     The depth can be used to limit the search space, which 
           --     in some cases is the only way to make the search decidable.
           generateAll,         generateAllDepth,
           {-generateFrom,        generateFromDepth,-}
           generateRandom,      generateRandomDepth,
           {-generateRandomFrom,  generateRandomFromDepth,-}

           -- ** Morphological Analysis
           Lemma, Analysis, Morpho,
           lookupMorpho, buildMorpho, fullFormLexicon,

           -- ** Visualizations
           graphvizAbstractTree,
           graphvizParseTree,
           graphvizParseTreeDep,
           graphvizDependencyTree,
           graphvizBracketedString,
           graphvizAlignment,
           gizaAlignment,
           GraphvizOptions(..),
           PGF2.graphvizDefaults,
           -- extra:
           Labels, getDepLabels,
           CncLabels, getCncDepLabels,

          ) where

import Prelude hiding ((<>))
import PGF2 (PGF, GraphvizOptions(..), FId, Expr(..), Type(..), Hypo, BindType(..))
import qualified PGF2
import qualified Data.Map as Map
import Control.Monad
import Data.Char
import Data.Maybe (fromMaybe)
import Data.List (nub,intersperse,groupBy,sortBy,partition)
import Data.Ord (comparing)
import qualified Text.ParserCombinators.ReadP as RP
import Text.PrettyPrint
import System.Random

---------------------------------------------------
-- Interface
---------------------------------------------------

newtype CId = CId String deriving (Eq,Ord)

mkCId = CId
wildCId = CId "_"

-- | Reads an identifier from 'String'. The function returns 'Nothing' if the string is not valid identifier.
readCId :: String -> Maybe CId
readCId s = case [x | (x,cs) <- RP.readP_to_S pCId s, all isSpace cs] of
              [x] -> Just x
              _   -> Nothing

-- | Renders the identifier as 'String'
showCId :: CId -> String
showCId (CId raw) = PGF2.showIdent raw

instance Show CId where
    showsPrec _ = showString . showCId

instance Read CId where
    readsPrec _ = RP.readP_to_S pCId

pCId :: RP.ReadP CId
pCId = do s <- PGF2.pIdent
          if s == "_"
            then RP.pfail
            else return (mkCId s)

ppCId :: CId -> Doc
ppCId = text . showCId

type Language = CId

readLanguage lang = CId lang
showLanguage (CId lang) = lang

-- | creates a type from list of hypothesises, category and
-- list of arguments for the category. The operation
-- @mkType [h_1,...,h_n] C [e_1,...,e_m]@ will create
-- @h_1 -> ... -> h_n -> C e_1 ... e_m@
mkType :: [Hypo] -> CId -> [Expr] -> Type
mkType hyps (CId cat) args = PGF2.mkType hyps cat args

-- | creates hypothesis for dependent type i.e. (x : A)
mkDepHypo :: CId -> Type -> Hypo
mkDepHypo (CId x) ty = PGF2.mkDepHypo x ty

-- | creates hypothesis for dependent type with implicit argument i.e. ({x} : A)
mkImplHypo :: CId -> Type -> Hypo
mkImplHypo (CId x) ty = PGF2.mkImplHypo x ty

unType :: Type -> ([Hypo], CId, [Expr])
unType (DTyp hyps cat es) = (hyps, CId cat, es)

type Tree = Expr

mkAbs :: BindType -> CId -> Expr -> Expr
mkAbs bt (CId var) e = PGF2.mkAbs bt var e

unAbs :: Expr -> Maybe (BindType, CId, Expr)
unAbs e =
  case PGF2.unAbs e of
    Just (bt,var,e) -> Just (bt,CId var,e)
    Nothing         -> Nothing

mkApp :: CId -> [Expr] -> Expr
mkApp (CId fun) es = PGF2.mkApp fun es

unApp :: Expr -> Maybe (CId, [Expr])
unApp e =
  case PGF2.unApp e of
    Just (fun,es) -> Just (CId fun,es)
    Nothing       -> Nothing

-- | Reads file in Portable Grammar Format and produces
-- 'PGF' structure. The file is usually produced with:
--
-- > $ gf -make <grammar file name>
readPGF  :: FilePath -> IO PGF
readPGF = PGF2.readPGF

-- | Tries to parse the given string in the specified language
-- and to produce abstract syntax expression.
parse :: PGF -> Language -> Type -> String -> [Tree]
parse gr (CId lang) cat sent = 
  case Map.lookup lang (PGF2.languages gr) of
    Just cnc -> case PGF2.parse cnc cat sent of
                  PGF2.ParseOk ts -> map fst ts
                  _               -> []
    Nothing  -> error ("Unknown language: " ++ lang)

-- | The same as 'parseAllLang' but does not return
-- the language.
parseAll     :: PGF -> Type -> String -> [[Tree]]
parseAll gr cat sent = 
  [map fst ts | (lang,cnc) <- Map.toList (PGF2.languages gr)
              , PGF2.ParseOk ts <- [PGF2.parse cnc cat sent]]

-- | Tries to parse the given string with all available languages.
-- The returned list contains pairs of language
-- and list of abstract syntax expressions 
-- (this is a list, since grammars can be ambiguous). 
-- Only those languages
-- for which at least one parsing is possible are listed.
parseAllLang :: PGF -> Type -> String -> [(Language,[Tree])]
parseAllLang gr cat sent = 
  [(CId lang,map fst ts)
              | (lang,cnc) <- Map.toList (PGF2.languages gr)
              , PGF2.ParseOk ts <- [PGF2.parse cnc cat sent]]

complete :: PGF -> Language -> Type -> String -> String -> (BracketedString,String,Map.Map Token [CId])
complete pgf (CId lang) typ input prefix =
  case Map.lookup lang (PGF2.languages pgf) of
    Just cnc -> case PGF2.complete cnc typ input prefix of
                  PGF2.ParseOk res -> (noBS, input++" "++prefix, Map.fromListWith (++) [(w,[CId fun]) | (w,fun,cat,_) <- res])
                  _                -> (noBS, input++" "++prefix, Map.empty)
    Nothing  -> error ("Unknown language: " ++ lang)
  where
    noBS = error "TODO: The bracketed string is not computed"

linearize :: PGF -> Language -> Tree -> String
linearize pgf (CId lang) t =
  case Map.lookup lang (PGF2.languages pgf) of
    Just cnc -> PGF2.linearize cnc t
    Nothing  -> error ("Unknown language: " ++ lang)

-- | The same as 'linearizeAllLang' but does not return
-- the language.
linearizeAll :: PGF -> Tree -> [String]
linearizeAll pgf = map snd . linearizeAllLang pgf

-- | Linearizes given expression as string in all languages
-- available in the grammar.
linearizeAllLang :: PGF -> Tree -> [(Language,String)]
linearizeAllLang pgf t = [(CId lang,PGF2.linearize cnc t) | (lang,cnc) <- Map.toList (PGF2.languages pgf)]

-- | Linearizes given expression as a bracketed string in the language
bracketedLinearize :: PGF -> Language -> Tree -> [BracketedString]
bracketedLinearize pgf (CId lang) t =
  case Map.lookup lang (PGF2.languages pgf) of
    Just cnc -> map bs2bs (PGF2.bracketedLinearize cnc t)
    Nothing  -> error ("Unknown language: " ++ lang)

-- | Creates a table from feature name to linearization. 
-- The outher list encodes the variations
tabularLinearizes :: PGF -> Language -> Expr -> [[(String,String)]]
tabularLinearizes pgf (CId lang) t =
  case Map.lookup lang (PGF2.languages pgf) of
    Just cnc -> [PGF2.tabularLinearize cnc t]
    Nothing  -> error ("Unknown language: " ++ lang)

showPrintName :: PGF -> Language -> CId -> String
showPrintName gr (CId lang) (CId name) =
  case Map.lookup lang (PGF2.languages gr) of
    Just cnc -> fromMaybe name (PGF2.printName cnc name)
    Nothing  -> error ("Unknown language: " ++ lang)

-- | List of all languages available in the given grammar.
languages    :: PGF -> [Language]
languages gr = [CId lang | (lang,_) <- Map.toList (PGF2.languages gr)]

-- | Gets the RFC 4646 language tag 
-- of the language which the given concrete syntax implements,
-- if this is listed in the source grammar.
-- Example language tags include @\"en\"@ for English,
-- and @\"en-UK\"@ for British English.
languageCode :: PGF -> Language -> Maybe String
languageCode gr (CId lang) =
  case Map.lookup lang (PGF2.languages gr) of
    Just cnc -> PGF2.languageCode cnc
    _        -> Nothing

-- | The abstract language name is the name of the top-level
-- abstract module
abstractName :: PGF -> Language
abstractName gr = CId (PGF2.abstractName gr)

-- | List of all categories defined in the given grammar.
-- The categories are defined in the abstract syntax
-- with the \'cat\' keyword.
categories :: PGF -> [CId]
categories gr = map CId (PGF2.categories gr)

categoryContext :: PGF -> CId -> Maybe [Hypo]
categoryContext gr (CId cat) = PGF2.categoryContext gr cat

-- | List of all functions defined in the abstract syntax
functions :: PGF -> [CId]
functions gr = map CId (PGF2.functions gr)

-- | List of all functions defined for a given category
functionsByCat :: PGF -> CId -> [CId]
functionsByCat gr (CId fun) = map CId (PGF2.functionsByCat gr fun)

-- | The type of a given function
functionType :: PGF -> CId -> Maybe Type
functionType gr (CId fun) = PGF2.functionType gr fun

type LIndex= String
type Token = String

-- | BracketedString represents a sentence that is linearized
-- as usual but we also want to retain the ''brackets'' that
-- mark the beginning and the end of each constituent.
data BracketedString
  = Leaf Token                                                                -- ^ this is the leaf i.e. a single token
  | Bracket CId {-# UNPACK #-} !FId {-# UNPACK #-} !FId LIndex CId [Expr] [BracketedString]
                                                                               -- ^ this is a bracket. The 'CId' is the category of
                                                                               -- the phrase. The 'FId' is an unique identifier for
                                                                               -- every phrase in the sentence. For context-free grammars
                                                                               -- i.e. without discontinuous constituents this identifier
                                                                               -- is also unique for every bracket. When there are discontinuous 
                                                                               -- phrases then the identifiers are unique for every phrase but
                                                                               -- not for every bracket since the bracket represents a constituent.
                                                                               -- The different constituents could still be distinguished by using
                                                                               -- the constituent index i.e. 'LIndex'. If the grammar is reduplicating
                                                                               -- then the constituent indices will be the same for all brackets
                                                                               -- that represents the same constituent.

bs2bs (PGF2.Leaf token) = Leaf token
bs2bs PGF2.BIND         = Leaf "&+"
bs2bs (PGF2.Bracket cat fid lbl fun bs) = Bracket (CId cat) fid fid lbl (CId fun) [] (map bs2bs bs)

-- | Renders the bracketed string as string where 
-- the brackets are shown as @(S ...)@ where
-- @S@ is the category.
showBracketedString :: BracketedString -> String
showBracketedString = render . ppBracketedString

ppBracketedString (Leaf t) = text t
ppBracketedString (Bracket cat fid fid' index _ _ bss) = parens (ppCId cat <> colon <> int fid <+> hsep (map ppBracketedString bss))

flattenBracketedString :: BracketedString -> [String]
flattenBracketedString (Leaf w)                  = [w]
flattenBracketedString (Bracket _ _ _ _ _ _ bss) = concatMap flattenBracketedString bss  

-- | Renders abstract syntax tree in Graphviz format.
-- The pair of 'Bool' @(funs,cats)@ lets you control whether function names and
-- category names are included in the rendered tree
graphvizAbstractTree :: PGF -> (Bool,Bool) -> Tree -> String
graphvizAbstractTree gr (funs,cats) = PGF2.graphvizAbstractTree gr PGF2.graphvizDefaults{noFun=not funs,noCat=not cats}

graphvizParseTree :: PGF -> Language -> GraphvizOptions -> Tree -> String
graphvizParseTree gr (CId lang) opts t =
   case Map.lookup lang (PGF2.languages gr) of
    Just cnc -> PGF2.graphvizParseTree cnc opts t
    Nothing  -> error ("Unknown language: " ++ lang)

type Labels = Map.Map CId [String]
type CncLabels = [CncLabel]

data CncLabel =
    CncSyncat (String, String -> Maybe (String -> String,String,String))
    -- (fun, word/lemma -> (pos,label,target))
    -- the pos can remain unchanged, as in the current notation in the article
  | CncMorpho (String,[String])
    -- (category, features in ascending order)
  | CncForm (String,(String,String))
    -- (wordform, (lemma,features))

-- | Prepare lines obtained from a configuration file for labels for
-- use with 'graphvizDependencyTree'. Format per line /fun/ /label/@*@.
--- ignore other gf-ud annotatations than #fun and #cat at this point 
getDepLabels :: String -> Labels
getDepLabels s = Map.fromList [(mkCId f,ls) | f:ls <- map (words . rmcomments) (lines s), not (head f == '#')]

getCncDepLabels :: String -> CncLabels
getCncDepLabels s = wlabels ws ++ flabels fs
 where
  wlabels =
    map CncSyncat .
    map merge .
    groupBy (\ (x,_) (a,_) -> x == a) .
    sortBy (comparing fst) .
    concatMap analyse .
    filter chooseW
    
  flabels =
    map CncMorpho .
    map collectTags .
    map words

  (fs,ws) = partition chooseF $ map uncomment $ lines s

  --- choose is for compatibility with the general notation
  chooseW line = notElem '(' line &&
                 elem '{' line
                   --- ignoring non-local (with "(") and abstract (without "{") rules
                   ---- TODO: this means that "(" cannot be a token

  chooseF line = take 1 line == "@"  --- feature assignments have the form e.g. @N SgNom SgGen ; no spaces inside tags

  uncomment line = case line of
    '-':'-':_ -> ""
    c:cs -> c : uncomment cs
    _ -> line

  analyse line = case break (=='{') line of
    (beg,_:ws) -> case break (=='}') ws of
      (toks,_:target) -> case (getToks beg, words target) of
        (funs,[    label,j]) -> [(fun, (tok, (id,       label,j))) | fun <- funs, tok <- getToks toks]
        (funs,[pos,label,j]) -> [(fun, (tok, (const pos,label,j))) | fun <- funs, tok <- getToks toks]
        _ -> []
      _ -> []
    _ -> []
  merge rules@((fun,_):_) = (fun, \tok ->
    case lookup tok (map snd rules) of
      Just new -> return new
      _ -> lookup "*"  (map snd rules)
    )
  getToks = map unquote . filter (/=",") . toks
  toks s = case lex s of [(t,"")] -> [t] ; [(t,cc)] -> t:toks cc ; _ -> []
  unquote s = case s of '"':cc@(_:_) | last cc == '"' -> init cc ; _ -> s

  collectTags (w:ws) = (tail w,ws)

-- auxiliaries for UD conversion  PK 15/12/2018 
rmcomments :: String -> String
rmcomments s = case s of
  '-':'-':_ -> []
  '#':'f':'u':'n':rest -> rmcomments rest -- the new gf-ud format
  '#':'c':'a':'t':rest -> rmcomments rest
  x:xs -> x : rmcomments xs
  _ -> []

-- | Visualize word dependency tree.
graphvizDependencyTree
  :: String -- ^ Output format: @"latex"@, @"conll"@, @"malt_tab"@, @"malt_input"@ or @"dot"@
  -> Bool -- ^ Include extra information (debug)
  -> Maybe Labels -- ^ abstract label information obtained with 'getDepLabels'
  -> Maybe CncLabels -- ^ concrete label information obtained with ' ' (was: unused (was: @Maybe String@))
  -> PGF
  -> CId -- ^ The language of analysis
  -> Tree
  -> String -- ^ Rendered output in the specified format
graphvizDependencyTree format debug mb_labels mb_cnclabels gr (CId lang) t =
  error "TODO: graphvizDependencyTree"

graphvizParseTreeDep :: Maybe Labels -> PGF -> Language -> GraphvizOptions -> Tree -> String
graphvizParseTreeDep mbl pgf lang opts tree = graphvizBracketedString opts mbl tree $ bracketedLinearize pgf lang tree

graphvizBracketedString :: GraphvizOptions -> Maybe Labels -> Tree -> [BracketedString] -> String
graphvizBracketedString opts mbl tree bss = render graphviz_code
    where
      graphviz_code
          = text "graph {" $$
            text node_style $$
            vcat internal_nodes $$
            (if noLeaves opts then empty
             else text leaf_style $$
                  leaf_nodes
            ) $$ text "}"

      leaf_style = mkOption "edge" "style" (leafEdgeStyle opts) ++
                   mkOption "edge" "color" (leafColor opts) ++
                   mkOption "node" "fontcolor" (leafColor opts) ++
                   mkOption "node" "fontname" (leafFont opts) ++
                   mkOption "node" "shape" "plaintext"

      node_style = mkOption "edge" "style" (nodeEdgeStyle opts) ++
                   mkOption "edge" "color" (nodeColor opts) ++
                   mkOption "node" "fontcolor" (nodeColor opts) ++
                   mkOption "node" "fontname" (nodeFont opts) ++
                   mkOption "node" "shape" nodeshape
          where nodeshape | noFun opts && noCat opts = "point"
                          | otherwise = "plaintext"

      mkOption object optname optvalue
          | null optvalue  = ""
          | otherwise      = object ++ "[" ++ optname ++ "=\"" ++ optvalue ++ "\"]; "

      mkNode fun cat
          | noFun opts = showCId cat
          | noCat opts = showCId fun
          | otherwise  = showCId fun ++ " : " ++ showCId cat

      nil = -1
      internal_nodes = [mkLevel internals |
                        internals <- getInternals (map ((,) nil) bss),
                        not (null internals)]
      leaf_nodes = mkLevel [(parent, id, mkLeafNode cat word) |
                            (id, (parent, (cat,word))) <- zip [100000..] (concatMap (getLeaves (mkCId "?") nil) bss)]

      getInternals []    = []
      getInternals nodes
          = nub [(parent, fid, mkNode fun cat) |
                 (parent, Bracket cat fid _ _ fun _ _) <- nodes]
            : getInternals [(fid, child) |
                            (_, Bracket _ fid _ _ _ _ children) <- nodes,
                            child <- children]

      getLeaves cat parent (Leaf word) = [(parent, (cat, word))] -- the lowest cat before the word
      getLeaves _ parent (Bracket cat fid _ i _ _ children)
          = concatMap (getLeaves cat fid) children

      mkLevel nodes
          = text "subgraph {rank=same;" $$
            nest 2 (-- the following gives the name of the node and its label:
                    vcat [tag id <> text (mkOption "" "label" lbl) | (_, id, lbl) <- nodes] $$
                    -- the following is for fixing the order between the children:
                    (if length nodes > 1 then
                         text (mkOption "edge" "style" "invis") $$
                         hsep (intersperse (text " -- ") [tag id | (_, id, _) <- nodes]) <+> semi
                     else empty)
                   ) $$
            text "}" $$
            -- the following is for the edges between parent and children:
            vcat [tag pid <> text " -- " <> tag id <> text (depLabel node) | node@(pid, id, _) <- nodes, pid /= nil] $$
            space

      depLabel node@(parent,id,lbl) 
        | noDep opts = ";"
        | otherwise = case getArg id of
            Just (fun,arg) -> mkOption "" "label" (lookLabel fun arg) 
            _ -> ";"
      getArg i = getArgumentPlace i (expr2numtree tree) Nothing

      labels = maybe Map.empty id mbl

      lookLabel fun arg = case Map.lookup fun labels of
        Just xx | length xx > arg -> case xx !! arg of
          "head" -> ""
          l -> l
        _ -> argLabel fun arg
      argLabel fun arg = if arg==0 then "" else "dep#" ++ show arg --showCId fun ++ "#" ++ show arg
                         -- assuming the arg is head, if no configuration is given; always true for 1-arg funs
      mkLeafNode cat word
       | noDep opts = word        --- || not (noCat opts) -- show POS only if intermediate nodes hidden
       | otherwise  = posCat cat ++ "\n" ++ word         -- show POS in dependency tree

      posCat cat = case Map.lookup cat labels of
        Just [p] -> p
        _ -> showCId cat

---- to restore the argument place from bracketed linearization
data NumTree = NumTree Int CId [NumTree]

getArgumentPlace :: Int -> NumTree -> Maybe (CId,Int) -> Maybe (CId,Int)
getArgumentPlace i tree@(NumTree int fun ts) mfi
 | i == int  = mfi
 | otherwise = case [fj | (t,x) <- zip ts [0..], Just fj <- [getArgumentPlace i t (Just (fun,x))]] of
     fj:_ -> Just fj
     _ -> Nothing

expr2numtree :: Expr -> NumTree
expr2numtree = fst . renumber 0 . flatten where
  flatten e = case e of
    EApp f a -> case flatten f of
      NumTree _ g ts -> NumTree 0 g (ts ++ [flatten a])
    EFun f -> NumTree 0 (CId f) []
  renumber i t@(NumTree _ f ts) = case renumbers i ts of
    (ts',j) -> (NumTree j f ts', j+1)
  renumbers i ts = case ts of
    t:tt -> case renumber i t of
      (t',j) -> case renumbers j tt of (tt',k) -> (t':tt',k)
    _ -> ([],i)
----- end this terrible stuff AR 4/11/2015

-- alignment in the Graphviz format from the intermediate structure
-- same effect as the old direct function
graphvizAlignment :: PGF -> [Language] -> Expr -> String
graphvizAlignment pgf langs exp =
  let cncs = [cnc | (l,cnc) <- Map.toList (PGF2.languages pgf)
                  , CId l `elem` langs]
  in PGF2.graphvizWordAlignment cncs PGF2.graphvizDefaults exp

gizaAlignment :: PGF -> (Language,Language) -> Expr -> (String,String,String)
gizaAlignment = error "TODO: gizaAlignment"


tag i
  | i < 0     = char 'r' <> int (negate i)
  | otherwise = char 'n' <> int i

-- | Generates an exhaustive possibly infinite list of
-- abstract syntax expressions.
generateAll :: PGF -> Type -> [Expr]
generateAll pgf ty = map fst (PGF2.generateAll pgf ty)

-- | A variant of 'generateAll' which also takes as argument
-- the upper limit of the depth of the generated expression.
generateAllDepth :: PGF -> Type -> Maybe Int -> [Expr]
generateAllDepth pgf ty mb_dp = map fst (PGF2.generateAllDepth pgf ty (fromMaybe maxBound mb_dp))

-- | Generates an infinite list of random abstract syntax expressions.
-- This is usefull for tree bank generation which after that can be used
-- for grammar testing.
generateRandom :: RandomGen g => g -> PGF -> Type -> [Expr]
generateRandom g pgf ty = map fst (PGF2.generateRandom g pgf ty)

-- | A variant of 'generateRandom' which also takes as argument
-- the upper limit of the depth of the generated expression.
generateRandomDepth :: RandomGen g => g -> PGF -> Type -> Maybe Int -> [Expr]
generateRandomDepth g pgf ty mb_dp = map fst (PGF2.generateRandomDepth g pgf ty (fromMaybe maxBound mb_dp))

type Lemma = CId
type Analysis = String

newtype Morpho = Morpho PGF2.Concr

buildMorpho :: PGF -> Language -> Morpho
buildMorpho pgf (CId lang) = Morpho $
  case Map.lookup lang (PGF2.languages pgf) of
    Just cnc -> cnc
    Nothing  -> error ("Unknown language: " ++ lang)

lookupMorpho :: Morpho -> String -> [(Lemma,Analysis)]
lookupMorpho (Morpho cnc) s =
  [(CId fun,an) | (fun,an,_) <- PGF2.lookupMorpho cnc s]

fullFormLexicon :: Morpho -> [(String,[(Lemma,Analysis)])]
fullFormLexicon (Morpho cnc) =
  [(w,[(CId fun,an) | (fun,an,_) <- ans]) | (w,ans) <- PGF2.fullFormLexicon cnc]
