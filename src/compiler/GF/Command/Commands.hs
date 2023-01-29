{-# LANGUAGE FlexibleInstances, UndecidableInstances, CPP #-}
module GF.Command.Commands (
  HasPGF(..),pgfCommands,
  options,flags,
  ) where
import Prelude hiding (putStrLn,(<>)) -- GHC 8.4.1 clash with Text.PrettyPrint
import System.Info(os)

import PGF2

import GF.Compile.Export
import GF.Compile.ToAPI
import GF.Compile.ExampleBased
import GF.Infra.Option (noOptions, readOutputFormat, outputFormatsExpl)
import GF.Infra.UseIO(writeUTF8File)
import GF.Infra.SIO
import GF.Command.Abstract
import GF.Command.CommandInfo
import GF.Command.CommonCommands
import GF.Text.Clitics
import GF.Quiz

import GF.Command.TreeOperations ---- temporary place for typecheck and compute

import GF.Data.Operations

import Data.Char
import Data.List(intersperse,nub)
import Data.Maybe
import qualified Data.Map as Map
import GF.Text.Pretty
import Data.List (sort)
import Control.Monad(mplus)
import qualified Control.Monad.Fail as Fail

class (Functor m,Monad m,MonadSIO m) => HasPGF m where getPGF :: m (Maybe PGF)

instance (Monad m,HasPGF m,Fail.MonadFail m) => TypeCheckArg m where
  typeCheckArg e = do mb_pgf <- getPGF
                      case mb_pgf of
                        Just pgf -> either fail
                                           (return . fst)
                                           (inferExpr pgf e)
                        Nothing  -> fail "Import a grammar before using this command"

pgfCommands :: (HasPGF m, Fail.MonadFail m) => Map.Map String (CommandInfo m)
pgfCommands = Map.fromList [
  ("aw", emptyCommandInfo {
     longname = "align_words",
     synopsis = "show word alignments between languages graphically",
     explanation = unlines [
       "Prints a set of strings in the .dot format (the graphviz format).",
       "The graph can be saved in a file by the wf command as usual.",
       "If the -view flag is defined, the graph is saved in a temporary file",
       "which is processed by 'dot' (graphviz) and displayed by the program indicated",
       "by the view flag. The target format is png, unless overridden by the",
       "flag -format. Results from multiple trees are combined to pdf with convert (ImageMagick)."
       ],
     exec = needPGF $ \ opts arg pgf -> do
         let es = toExprs arg
         let langs = optLangs pgf opts
         if isOpt "giza" opts
           then do
             let giz = map (gizaAlignment pgf (head $ langs, head $ tail $ langs)) es
             let lsrc = unlines $ map (\(x,_,_) -> x) giz
             let ltrg = unlines $ map (\(_,x,_) -> x) giz
             let align = unlines $ map (\(_,_,x) -> x) giz
             let grph = if null es then [] else lsrc ++ "\n--end_source--\n\n"++ltrg++"\n-end_target--\n\n"++align
             return $ fromString grph
           else do
             let grphs = map (graphvizWordAlignment langs graphvizDefaults) es
             if isFlag "view" opts || isFlag "format" opts
               then do
                 let view = optViewGraph opts
                 let format = optViewFormat opts
                 viewGraphviz view format "_grpha_" grphs
               else return $ fromString $ unlines grphs,
     examples = [
       ("gr | aw"                         , "generate a tree and show word alignment as graph script"),
       ("gr | aw -view=\"open\""          , "generate a tree and display alignment on Mac"),
       ("gr | aw -view=\"eog\""           , "generate a tree and display alignment on Ubuntu"),
       ("gt | aw -giza | wf -file=aligns" , "generate trees, send giza alignments to file")
       ],
     options = [
       ("giza",  "show alignments in the Giza format; the first two languages")
       ],
     flags = [
       ("format","format of the visualization file (default \"png\")"),
       ("lang",  "alignments for this list of languages (default: all)"),
       ("view",  "program to open the resulting file")
       ]
    }),

  ("ca", emptyCommandInfo {
     longname = "clitic_analyse",
     synopsis = "print the analyses of all words into stems and clitics",
     explanation = unlines [
       "Analyses all words into all possible combinations of stem + clitics.",
       "The analysis is returned in the format stem &+ clitic1 &+ clitic2 ...",
       "which is hence the inverse of 'pt -bind'. The list of clitics is give",
       "by the flag '-clitics'. The list of stems is given as the list of words",
       "of the language given by the '-lang' flag."
       ],
     exec  = needPGF $ \opts ts pgf -> do
               concr <- optLang pgf opts
               case opts of
                 _ | isOpt "raw" opts ->
                       return . fromString .
                       unlines . map (unwords . map (concat . intersperse "+")) .
                       map (getClitics (not . null . lookupMorpho concr) (optClitics opts)) .
                       concatMap words $ toStrings ts
                 _ -> return . fromStrings .
                      getCliticsText (not . null . lookupMorpho concr) (optClitics opts) .
                      concatMap words $ toStrings ts,
     flags = [
       ("clitics","the list of possible clitics (comma-separated, no spaces)"),
       ("lang",   "the language of analysis")
       ],
     options = [
       ("raw", "analyse each word separately (not suitable input for parser)")
       ],
     examples = [
       mkEx "ca -lang=Fin -clitics=ko,ni \"nukkuuko minun vaimoni\" | p  -- to parse Finnish"
       ]
     }),

  ("eb", emptyCommandInfo {
     longname = "example_based",
     syntax = "eb (-probs=FILE | -lang=LANG)* -file=FILE.gfe",
     synopsis = "converts .gfe files to .gf files by parsing examples to trees",
     explanation = unlines [
       "Reads FILE.gfe and writes FILE.gf. Each expression of form",
       "'%ex CAT QUOTEDSTRING' in FILE.gfe is replaced by a syntax tree.",
       "This tree is the first one returned by the parser; a biased ranking",
       "can be used to regulate the order. If there are more than one parses",
       "the rest are shown in comments, with probabilities if the order is biased.",
       "The probabilities flag and configuration file is similar to the commands",
       "gr and rt. Notice that the command doesn't change the environment,",
       "but the resulting .gf file must be imported separately."
       ],
     options = [
       ("api","convert trees to overloaded API expressions (using Syntax not Lang)")
       ],
     flags = [
       ("file","the file to be converted (suffix .gfe must be given)"),
       ("lang","the language in which to parse")
       ],
     exec = needPGF $ \opts _ pgf -> do
       let file = optFile opts
       let printer = if (isOpt "api" opts) then exprToAPI else (showExpr [])
       concr <- optLang pgf opts
       let conf = configureExBased pgf concr printer
       (file',ws) <- restricted $ parseExamplesInGrammar conf file
       if null ws then return () else putStrLn ("unknown words: " ++ unwords ws)
       return (fromString ("wrote " ++ file')),
     needsTypeCheck = False
     }),

  ("gr", emptyCommandInfo {
     longname = "generate_random",
     synopsis = "generate random trees in the current abstract syntax",
     syntax = "gr [-cat=CAT] [-number=INT]",
     examples = [
       mkEx "gr                     -- one tree in the startcat of the current grammar",
       mkEx "gr -cat=NP -number=16  -- 16 trees in the category NP",
       mkEx "gr -lang=LangHin,LangTha -cat=Cl  -- Cl, both in LangHin and LangTha",
       mkEx "gr -probs=FILE         -- generate with bias",
       mkEx "gr (AdjCN ? (UseN ?))  -- generate trees of form (AdjCN ? (UseN ?))"
       ],
     explanation = unlines [
       "Generates a list of random trees, by default one tree.",
       "If a tree argument is given, the command completes the Tree with values to",
       "all metavariables in the tree. The generation can be biased by probabilities",
       "if the grammar was compiled with option -probs"
       ],
     options = [
       ("show_probs", "show the probability of each result")
       ],
     flags = [
       ("cat","generation category"),
       ("lang","uses only functions that have linearizations in all these languages"),
       ("number","number of trees generated")
       ],
     exec = needPGF $ \opts arg pgf -> do
       gen <- newStdGen
       let ts  = case mexp (toExprs arg) of
                   Just ex -> generateRandomFrom gen pgf ex
                   Nothing -> generateRandom     gen pgf (optType pgf opts)
       returnFromExprs (isOpt "show_probs" opts) $ take (optNum opts) ts
     }),

  ("gt", emptyCommandInfo {
     longname = "generate_trees",
     synopsis = "generates a list of trees, by default exhaustive",
     explanation = unlines [
       "Generates all trees of a given category.",
       "If a Tree argument is given, the command completes the Tree with values",
       "to all metavariables in the tree."
       ],
     options = [
       ("show_probs", "show the probability of each result")
       ],
     flags = [
       ("cat","the generation category"),
       ("lang","excludes functions that have no linearization in this language"),
       ("number","the number of trees generated")
       ],
     examples = [
       mkEx "gt                     -- all trees in the startcat",
       mkEx "gt -cat=NP -number=16  -- 16 trees in the category NP",
       mkEx "gt (AdjCN ? (UseN ?))  -- trees of form (AdjCN ? (UseN ?))"
       ],
     exec = needPGF $ \opts arg pgf -> do
       let es = case mexp (toExprs arg) of
                  Just ex -> generateAllFrom pgf ex
                  Nothing -> generateAll     pgf (optType pgf opts)
       returnFromExprs (isOpt "show_probs" opts) $ takeOptNum opts es
     }),

  ("i", emptyCommandInfo {
     longname = "import",
     synopsis = "import a grammar from source code or compiled .pgf file",
     explanation = unlines [
       "Reads a grammar from File and compiles it into a GF runtime grammar.",
       "If its abstract is different from current state, old modules are discarded.",
       "If its abstract is the same and a concrete with the same name is already in the state",
       "it is overwritten - but only if compilation succeeds.",
       "The grammar parser depends on the file name suffix:",
       "  .cf    context-free (labelled BNF) source",
       "  .ebnf  extended BNF source",
       "  .gfm   multi-module GF source",
       "  .gf    normal GF source",
       "  .gfo   compiled GF source",
       "  .pgf   precompiled grammar in Portable Grammar Format"
       ],
     flags = [
       ("probs","file with biased probabilities for generation")
       ],
     options = [
       ("retain","retain operations (used for cc command)"),
       ("src",   "force compilation from source"),
       ("v",     "be verbose - show intermediate status information")
       ],
     needsTypeCheck = False
     }),

  ("l", emptyCommandInfo {
     longname = "linearize",
     synopsis = "convert an abstract syntax expression to string",
     explanation = unlines [
       "Shows the linearization of a tree by the grammars in scope.",
       "The -lang flag can be used to restrict this to fewer languages.",
       "A sequence of string operations (see command ps) can be given",
       "as options, and works then like a pipe to the ps command, except",
       "that it only affect the strings, not e.g. the table labels."
       ],
     examples = [
       mkEx "l -lang=LangSwe,LangNor no_Utt   -- linearize tree to LangSwe and LangNor",
       mkEx "gr -lang=LangHin -cat=Cl | l -table -to_devanagari -- hindi table"
       ],
     exec = needPGF $ \ opts ts pgf -> return . fromStrings . optLins pgf opts $ toExprs ts,
     options = [
       ("all",    "show all forms and variants, one by line (cf. l -list)"),
       ("bracket","show tree structure with brackets and paths to nodes"),
       ("groups", "all languages, grouped by lang, remove duplicate strings"),
       ("list","show all forms and variants, comma-separated on one line (cf. l -all)"),
       ("multi","linearize to all languages (default)"),
       ("table","show all forms labelled by parameters"),
       ("treebank","show the tree and tag linearizations with language names")
       ] ++ stringOpOptions,
     flags = [
       ("lang","the languages of linearization (comma-separated, no spaces)")
       ]
     }),

  ("ma", emptyCommandInfo {
     longname = "morpho_analyse",
     synopsis = "print the morphological analyses of words in the string",
     explanation = unlines [
       "Prints all the analyses of words in the input string.",
       "By default it assumes that the input consists of a single lexical expression,",
       "but if one of the options bellow is used then the command tries to",
       "separate the text into units. Some of the units may be multi-word expressions,",
       "others punctuations, or morphemes not separated by spaces."
       ],
     exec  = needPGF $ \opts ts pgf -> do
               concr <- optLang pgf opts
               case opts of
                 _ | isOpt "all" opts ->
                      return . fromString . unlines .
                      map prCohortAnalysis . concatMap (morphoCohorts id concr) $
                      toStrings ts
                 _ | isOpt "longest" opts ->
                      return . fromString . unlines .
                      map prCohortAnalysis . concatMap (morphoCohorts filterLongest concr) $
                      toStrings ts
                 _ | isOpt "best" opts ->
                      return . fromString . unlines .
                      map prCohortAnalysis . concatMap (morphoCohorts filterBest concr) $
                      toStrings ts
                 _ | isOpt "known" opts ->
                      return . fromString . unwords .
                      concatMap (morphoKnown concr) $
                      toStrings ts
                 _ | isOpt "missing" opts ->
                      return . fromString . unwords .
                      concatMap (morphoMissing concr) $
                      toStrings ts
                 _ -> return . fromString . unlines .
                      map prMorphoAnalysis . concatMap (morphos pgf opts) $
                      toStrings ts,
     flags = [
       ("lang","the languages of analysis (comma-separated, no spaces)")
       ],
     options = [
       ("all",    "scan the text for all words, not just a single one"),
       ("longest","scan the text for all words, and apply longest match filtering"),
       ("best",   "scan the text for all words, and apply global best match filtering"),
       ("known",  "list all known words, in order of appearance"),
       ("missing","list all missing words, in order of appearance")
       ]
     }),

  ("mq", emptyCommandInfo {
     longname = "morpho_quiz",
     synopsis = "start a morphology quiz",
     syntax   = "mq (-cat=CAT)? (-probs=FILE)? TREE?",
     exec = needPGF $ \ opts arg pgf -> do
         lang <- optLang pgf opts
         let typ  = optType pgf opts
         let mt = mexp (toExprs arg)
         restricted $ morphologyQuiz mt pgf lang typ
         return void,
     flags = [
       ("lang","language of the quiz"),
       ("cat","category of the quiz"),
       ("number","maximum number of questions")
       ]
     }),

  ("p", emptyCommandInfo {
     longname = "parse",
     synopsis = "parse a string to abstract syntax expression",
     explanation = unlines [
       "Shows all trees returned by parsing a string in the grammars in scope.",
       "The -lang flag can be used to restrict this to fewer languages.",
       "The default start category can be overridden by the -cat flag.",
       "See also the ps command for lexing and character encoding."
       ],
     exec = needPGF $ \opts ts pgf ->
              return $
                foldr (joinPiped . fromParse1 opts) void 
                  (concat [
                              [(s,parse concr (optType pgf opts) s) |
                                            concr <- optLangs pgf opts] 
                           | s <- toStrings ts]),
     options = [
       ("show_probs", "show the probability of each result")
       ],
     flags = [
       ("cat","target category of parsing"),
       ("lang","the languages of parsing (comma-separated, no spaces)"),
       ("number","limit the results to the top N trees")
       ]
     }),

  ("pg", emptyCommandInfo { -----
     longname = "print_grammar",
     synopsis = "print the actual grammar with the given printer",
     explanation = unlines [
       "Prints the actual grammar, with all involved languages.",
       "In some printers, this can be restricted to a subset of languages",
       "with the -lang=X,Y flag (comma-separated, no spaces).",
       "The -printer=P flag sets the format in which the grammar is printed.",
       "N.B.1 Since grammars are compiled when imported, this command",
       "generally shows a grammar that looks rather different from the source.",
       "N.B.2 Another way to produce different formats is to use 'gf -make',",
       "the batch compiler. The following values are available both for",
       "the batch compiler (flag -output-format) and the print_grammar",
       "command (flag -printer):",
       ""
       ] ++ unlines (sort [
        " " ++ opt ++ "\t\t" ++ expl |
           ((opt,_),expl) <- outputFormatsExpl, take 1 expl /= "*"
       ]),
     exec  = needPGF $ \opts _ pgf -> prGrammar pgf opts,
     flags = [
       ("file",   "set the file name when printing with -pgf option"),
       ("lang",   "select languages for some options (default all languages)"),
       ("printer","select the printing format (see flag values above)")
       ],
     options = [
       ("cats",   "show just the names of abstract syntax categories"),
       ("fullform", "print the fullform lexicon"),
       ("funs",   "show just the names and types of abstract syntax functions"),
       ("langs",  "show just the names of top concrete syntax modules"),
       ("lexc", "print the lexicon in Xerox LEXC format"),
       ("missing","show just the names of functions that have no linearization"),
       ("opt",    "optimize the generated pgf"),
       ("pgf",    "write the current pgf image in a file"),
       ("words", "print the list of words")
       ],
     examples = [
       mkEx ("pg -funs | ? grep \" S ;\"  -- show functions with value cat S")
       ]
     }),

  ("pt", emptyCommandInfo {
     longname = "put_tree",
     syntax = "pt OPT? TREE",
     synopsis = "return a tree, possibly processed with a function",
     explanation = unlines [
       "Returns a tree obtained from its argument tree by applying",
       "tree processing functions in the order given in the command line",
       "option list. Thus 'pt -f -g s' returns g (f s). Typical tree processors",
       "are type checking and semantic computation."
       ],
     examples = [
       mkEx "pt -compute (plus one two)                               -- compute value"
       ],
     exec = needPGF $ \opts arg pgf ->
            returnFromExprs False .  takeOptNum opts . map (flip (,) 0) . treeOps pgf opts $ toExprs arg,
     options = treeOpOptions undefined{-pgf-},
     flags = [("number","take at most this many trees")] ++ treeOpFlags undefined{-pgf-}
     }),

  ("rf",  emptyCommandInfo {
     longname = "read_file",
     synopsis = "read string or tree input from a file",
     explanation = unlines [
       "Reads input from file. The filename must be in double quotes.",
       "The input is interpreted as a string by default, and can hence be",
       "piped e.g. to the parse command. The option -tree interprets the",
       "input as a tree, which can be given e.g. to the linearize command.",
       "The option -lines will result in a list of strings or trees, one by line."
       ],
     options = [
       ("lines","return the list of lines, instead of the singleton of all contents"),
       ("tree","convert strings into trees")
       ],
     exec = needPGF $ \ opts _ pgf -> do
       let file = valStrOpts "file" "_gftmp" opts
       let exprs []         = ([],empty)
           exprs ((n,s):ls) | null s
                            = exprs ls
           exprs ((n,s):ls) = case readExpr s of
                                Just e  -> let (es,err) = exprs ls
                                           in case inferExpr pgf e of
                                                Right (e,t) -> (e:es,err)
                                                Left err    -> (es,"on line" <+> n <> ':' $$ nest 2 err $$ err)
                                Nothing -> let (es,err) = exprs ls
                                           in (es,"on line" <+> n <> ':' <+> "parse error" $$ err)
           returnFromLines ls = case exprs ls of
                                  (es, err) | null es   -> return $ pipeMessage $ render (err $$ "no trees found")
                                            | otherwise -> return $ pipeWithMessage (map (flip (,) 0) es) (render err)

       s <- restricted $ readFile file
       case opts of
         _ | isOpt "lines" opts && isOpt "tree" opts ->
               returnFromLines (zip [1::Int ..] (lines s))
         _ | isOpt "tree" opts ->
               returnFromLines [(1::Int,s)]
         _ | isOpt "lines" opts -> return (fromStrings $ lines s)
         _ -> return (fromString s),
     flags = [("file","the input file name")]
     }),

  ("tq", emptyCommandInfo {
     longname = "translation_quiz",
     syntax   = "tq -from=LANG -to=LANG (-cat=CAT)? (-probs=FILE)? TREE?",
     synopsis = "start a translation quiz",
     exec = needPGF $ \ opts arg pgf -> do
         from <- optLangFlag "from" pgf opts
         to   <- optLangFlag "to"   pgf opts
         let typ  = optType pgf opts
         let mt   = mexp (toExprs arg)
         restricted $ translationQuiz mt pgf from to typ
         return void,
     flags = [
       ("from","translate from this language"),
       ("to","translate to this language"),
       ("cat","translate in this category"),
       ("number","the maximum number of questions")
       ],
     examples = [
       mkEx ("tq -from=Eng -to=Swe                               -- any trees in startcat"),
       mkEx ("tq -from=Eng -to=Swe (AdjCN (PositA ?2) (UseN ?))  -- only trees of this form")
       ]
     }),

  ("vd", emptyCommandInfo {
     longname = "visualize_dependency",
     synopsis = "show word dependency tree graphically",
     explanation = unlines [
       "Prints a dependency tree in the .dot format (the graphviz format, default)",
       "or LaTeX (flag -output=latex)",
       "or the CoNLL/MaltParser format (flag -output=conll for training, malt_input",
       "for unanalysed input).",
       "By default, the last argument is the head of every abstract syntax",
       "function; moreover, the head depends on the head of the function above.",
       "The graph can be saved in a file by the wf command as usual.",
       "If the -view flag is defined, the graph is saved in a temporary file",
       "which is processed by dot (graphviz) and displayed by the program indicated",
       "by the view flag. The target format is png, unless overridden by the",
       "flag -format. Results from multiple trees are combined to pdf with convert (ImageMagick).",
       "See also 'vp -showdep' for another visualization of dependencies." 
       ],
     exec = needPGF $ \ opts arg pgf -> do
         let absname = abstractName pgf
         let es = toExprs arg
         let debug = isOpt "v" opts
         let abslabels = valStrOpts "abslabels" (valStrOpts "file" "" opts) opts
         let cnclabels = valStrOpts "cnclabels" "" opts
         let outp = valStrOpts "output" "dot" opts
         mlab <- case abslabels of
           "" -> return Nothing
           _  -> (Just . getDepLabels) `fmap` restricted (readFile abslabels)
         mclab <- case cnclabels of
           "" -> return Nothing
           _  -> (Just . getCncDepLabels) `fmap` restricted (readFile cnclabels)
         concr <- optLang pgf opts
         let grphs = map (graphvizDependencyTree outp debug mlab mclab concr) es
         if isOpt "conll2latex" opts
           then return $ fromString $ conlls2latexDoc $ stanzas $ unlines $ toStrings arg
           else if isFlag "view" opts && valStrOpts "output" "" opts == "latex"
             then do
               let view = optViewGraph opts
               viewLatex view "_grphd_" grphs
             else if isFlag "view" opts || isFlag "format" opts
               then do
                 let view = optViewGraph opts
                 let format = optViewFormat opts
                 viewGraphviz view format "_grphd_" grphs
               else return $ fromString $ unlines $ intersperse "" grphs,
     examples = [
       mkEx "gr | vd              -- generate a tree and show dependency tree in .dot",
       mkEx "gr | vd -view=open   -- generate a tree and display dependency tree on with Mac's 'open'",
       mkEx "gr | vd -view=open -output=latex   -- generate a tree and display latex dependency tree with Mac's 'open'",
       mkEx "gr -number=1000 | vd -abslabels=Lang.labels -cnclabels=LangSwe.labels -output=conll  -- generate a random treebank",
       mkEx "rf -file=ex.conll | vd -conll2latex | wf -file=ex.tex   -- convert conll file to latex"
       ],
     options = [
       ("v","show extra information"),
       ("conll2latex", "convert conll to latex")
       ],
     flags = [
       ("abslabels","abstract configuration file for labels, format per line 'fun label*'"),
       ("cnclabels","concrete configuration file for labels, format per line 'fun {words|*} pos label head'"),
       ("file",     "same as abslabels (abstract configuration file)"),
       ("format",   "format of the visualization file using dot (default \"png\")"),
       ("output",   "output format of graph source (latex, conll, dot (default but deprecated))"),
       ("view",     "program to open the resulting graph file (default \"open\")"),
       ("lang",     "the language of analysis")
       ]
    }),

  ("vp", emptyCommandInfo {
     longname = "visualize_parse",
     synopsis = "show parse tree graphically",
     explanation = unlines [
       "Prints a parse tree in the .dot format (the graphviz format).",
       "The graph can be saved in a file by the wf command as usual.",
       "If the -view flag is defined, the graph is saved in a temporary file",
       "which is processed by dot (graphviz) and displayed by the program indicated",
       "by the view flag. The target format is png, unless overridden by the",
       "flag -format. Results from multiple trees are combined to pdf with convert (ImageMagick)."
       ],
     exec = needPGF $ \opts arg pgf -> do
         let es        = toExprs arg
         let gvOptions = GraphvizOptions {noLeaves = isOpt "noleaves" opts && not (isOpt "showleaves" opts),
                                          noFun = isOpt "nofun" opts || not (isOpt "showfun" opts),
                                          noCat = isOpt "nocat" opts && not (isOpt "showcat" opts),
                                          noDep = not (isOpt "showdep" opts),
                                          nodeFont = valStrOpts "nodefont" "" opts,
                                          leafFont = valStrOpts "leaffont" "" opts,
                                          nodeColor = valStrOpts "nodecolor" "" opts,
                                          leafColor = valStrOpts "leafcolor" "" opts,
                                          nodeEdgeStyle = valStrOpts "nodeedgestyle" "solid" opts,
                                          leafEdgeStyle = valStrOpts "leafedgestyle" "dashed" opts
                                         }
         concr  <- optLang pgf opts
         let grphs = map (graphvizParseTree concr gvOptions) es
         if isFlag "view" opts || isFlag "format" opts
           then do
             let view = optViewGraph opts
             let format = optViewFormat opts
             viewGraphviz view format "_grphp_" grphs
           else return $ fromString $ unlines grphs,
     examples = [
       mkEx "p \"John walks\" | vp  -- generate a tree and show parse tree as .dot script",
       mkEx "gr | vp -view=open -- generate a tree and display parse tree on a Mac",
       mkEx "p \"she loves us\" | vp -view=open -showdep -file=uddeps.labels -nocat  -- show a visual variant of a dependency tree"
       ],
     options = [
       ("showcat","show categories in the tree nodes (default)"),
       ("nocat","don't show categories"),
       ("showdep","show dependency labels"),
       ("showfun","show function names in the tree nodes"),
       ("nofun","don't show function names (default)"),
       ("showleaves","show the leaves of the tree (default)"),
       ("noleaves","don't show the leaves of the tree (i.e., only the abstract tree)")
       ],
     flags = [
       ("lang","the language to visualize"),
       ("file","configuration file for dependency labels with -deps, format per line 'fun label*'"),
       ("format","format of the visualization file (default \"png\")"),
       ("view","program to open the resulting file (default \"open\")"),
       ("nodefont","font for tree nodes (default: Times -- graphviz standard font)"),
       ("leaffont","font for tree leaves (default: nodefont)"),
       ("nodecolor","color for tree nodes (default: black -- graphviz standard color)"),
       ("leafcolor","color for tree leaves (default: nodecolor)"),
       ("nodeedgestyle","edge style between tree nodes (solid/dashed/dotted/bold, default: solid)"),
       ("leafedgestyle","edge style for links to leaves (solid/dashed/dotted/bold, default: dashed)")
       ]
    }),

  ("vt", emptyCommandInfo {
     longname = "visualize_tree",
     synopsis = "show a set of trees graphically",
     explanation = unlines [
       "Prints a set of trees in the .dot format (the graphviz format).",
       "The graph can be saved in a file by the wf command as usual.",
       "If the -view flag is defined, the graph is saved in a temporary file",
       "which is processed by dot (graphviz) and displayed by the command indicated",
       "by the view flag. The target format is postscript, unless overridden by the",
       "flag -format. Results from multiple trees are combined to pdf with convert (ImageMagick).",
       "With option -mk, use for showing library style function names of form 'mkC'."
       ],
     exec = needPGF $ \opts arg pgf ->
      let es = toExprs arg in
       if isOpt "mk" opts
       then return $ fromString $ unlines $ map (tree2mk pgf) es
       else if isOpt "api" opts
       then do
         let ss = map exprToAPI es
         mapM_ putStrLn ss
         return void
       else do
         let funs = isOpt "nofun" opts
         let cats = isOpt "nocat" opts
         let grphs = map (graphvizAbstractTree pgf (graphvizDefaults{noFun=funs,noCat=cats})) es
         if isFlag "view" opts || isFlag "format" opts
           then do
             let view = optViewGraph opts
             let format = optViewFormat opts
             viewGraphviz view format "_grpht_" grphs
           else return $ fromString $ unlines grphs,
     examples = [
       mkEx "p \"hello\" | vt              -- parse a string and show trees as graph script",
       mkEx "p \"hello\" | vt -view=\"open\" -- parse a string and display trees on a Mac"
       ],
     options = [
       ("api", "show the tree with function names converted to 'mkC' with value cats C"),
       ("mk",  "similar to -api, deprecated"),
       ("nofun","don't show functions but only categories"),
       ("nocat","don't show categories but only functions")
       ],
     flags = [
       ("format","format of the visualization file (default \"png\")"),
       ("view","program to open the resulting file (default \"open\")")
       ]
     }),

  ("ai", emptyCommandInfo {
     longname = "abstract_info",
     syntax = "ai IDENTIFIER  or  ai EXPR",
     synopsis = "Provides an information about a function, an expression or a category from the abstract syntax",
     explanation = unlines [
       "The command has one argument which is either a function, an expression or",
       "a category defined in the abstract syntax of the current grammar.",
       "If the argument is a function then its type is printed out.",
       "If it is a category then the category definition is printed.",
       "If a whole expression is given, then it prints the expression with refined",
       "metavariables as well as the type of the expression."
       ],
     exec = needPGF $ \opts arg pgf -> do
       case toExprs arg of
         [e] -> case unApp e of
                  Just (id, []) -> case functionType pgf id of
                                     Just ty -> do putStrLn (showFun pgf id ty)
                                                   putStrLn ("Probability: "++show (exprProbability pgf e))
                                                   return void
                                     Nothing -> case categoryContext pgf id of
                                                  Just hypos -> do putStrLn ("cat "++id++if null hypos then "" else ' ':showContext [] hypos)
                                                                   let ls = [showFun pgf fn ty | fn <- functionsByCat pgf id, Just ty <- [functionType pgf fn]]
                                                                   if null ls
                                                                     then return ()
                                                                     else putStrLn (unlines ("":ls))
                                                                   putStrLn ("Probability: "++show (categoryProbability pgf id))
                                                                   return void
                                                  Nothing    -> do putStrLn ("unknown category of function identifier "++show id)
                                                                   return void
                  _             -> case inferExpr pgf e of
                                     Left err     -> errorWithoutStackTrace err
                                     Right (e,ty) -> do putStrLn ("Expression:  "++showExpr [] e)
                                                        putStrLn ("Type:        "++showType [] ty)
                                                        putStrLn ("Probability: "++show (exprProbability pgf e))
                                                        return void
         _           -> do putStrLn "a single identifier or expression is expected from the command"
                           return void,
     needsTypeCheck = False
     }),
  ("c", emptyCommandInfo {
     longname = "create",
     syntax = "create fun f = ..; create cat c = ..; create concrete l; create lin c = ..; or create lincat c = ..",
     synopsis = "Dynamically adds new functions, categories and languages to the current grammar.",
     explanation = unlines [
       "After the command you can write fun, data, cat, concrete, lin or a lincat definition.",
       "The syntax is the same as if the definition was in a module. If you want to use",
       "any operations inside lin and lincat, you should import them",
       "by using the command `i -resource <file path>`."
       ],
     flags = [
       ("lang","the language to which to add a lin or a lincat"),
       ("prob","the probability for a new abstract function")
       ],
     needsTypeCheck = False
     }),
  ("d", emptyCommandInfo {
     longname = "drop",
     syntax = "drop fun f; drop cat c; drop concrete l; drop lin c; or drop lincat c",
     synopsis = "Dynamically removes functions, categories and languages from the current grammar.",
     explanation = unlines [
       "After the command you must specify whether you want to remove",
       "fun, data, cat, concrete, lin or a lincat definition.",
       "Note that if you are removing an abstract function or category,",
       "then all corresponding linearizations will be dropped as well."
       ],
     flags = [
       ("lang","the language from which to remove the lin or the lincat")
       ],
     needsTypeCheck = False
     }),
  ("t", emptyCommandInfo {
     longname = "transaction",
     syntax = "transaction (start|commit|rollback)",
     synopsis = "Starts, commits or rollbacks a transaction",
     explanation = unlines [
       "If there is no active transaction, each create and drop command",
       "starts its own transaction. Start it manually",
       "if you want to perform several operations in one transaction.",
       "This also makes batch operations a lot faster."
       ],
     flags = [],
     needsTypeCheck = False
     })
  ]
 where
   needPGF exec opts ts = do
     mb_pgf <- getPGF
     case mb_pgf of
       Just pgf -> do liftSIO $ exec opts ts pgf
       _ -> fail "Import a grammar before using this command"

   joinPiped (Piped (es1,ms1)) (Piped (es2,ms2)) = Piped (jA es1 es2,ms1+++-ms2)
     where
       jA (Exprs es1) (Exprs es2) = Exprs (es1++es2)

   fromParse1 opts (s,po) =
     case po of
       ParseOk ts      -> fromExprs (isOpt "show_probs" opts) (takeOptNum opts ts)
       ParseFailed i t -> pipeMessage $ "The parser failed at token "
                                        ++ show i ++": "
                                        ++ show t
       ParseIncomplete -> pipeMessage "The sentence is not complete"

   optLins pgf opts ts = concatMap (optLin pgf opts) ts
   optLin pgf opts t =
     case opts of
       _ | isOpt "treebank" opts ->
         (abstractName pgf ++ ": " ++ showExpr [] t) :
         [concreteName concr ++ ": " ++ s | concr <- optLangs pgf opts, s<-linear opts concr t]
       _ -> [s | concr <- optLangs pgf opts, s <- linear opts concr t]

   linear :: [Option] -> Concr -> Expr -> [String]
   linear opts concr = case opts of
       _ | isOpt "list"    opts &&
           isOpt "all"     opts -> map (commaList . map snd) . tabularLinearizeAll concr
       _ | isOpt "list"    opts -> (:[]) . commaList .
                                   map snd . tabularLinearize concr
       _ | isOpt "table"   opts &&
           isOpt "all"     opts -> map (\(p,v) -> p+++":"+++v) . concat . tabularLinearizeAll concr
       _ | isOpt "table"   opts -> map (\(p,v) -> p+++":"+++v) . tabularLinearize concr
       _ | isOpt "bracket" opts &&
           isOpt "all"     opts -> map (unwords . map showBracketedString) . bracketedLinearizeAll concr
       _ | isOpt "bracket" opts -> (:[]) . unwords . map showBracketedString . bracketedLinearize concr
       _ | isOpt "all"     opts -> linearizeAll concr
       _                        -> (:[]) . linearize concr

   -- replace each non-atomic constructor with mkC, where C is the val cat
   tree2mk pgf = showExpr [] . t2m where
     t2m t = case unApp t of
               Just (cid,ts@(_:_)) -> mkApp (mk cid) (map t2m ts)
               _                   -> t
     mk f = case functionType pgf f of
              Just ty -> let (_,cat,_) = unType ty
                         in "mk" ++ cat
              Nothing -> f

   commaList [] = []
   commaList ws = concat $ head ws : map (", " ++) (tail ws)

   optLang  = optLangFlag "lang"
   optLangs = optLangsFlag "lang"

   optLangFlag flag pgf opts =
     case optLangsFlag flag pgf opts of
       []     -> fail "no language specified"
       (l:ls) -> return l

   optLangsFlag flag pgf opts =
     case valStrOpts flag "" opts of
       ""  -> Map.elems langs
       str -> mapMaybe (completeLang pgf) (chunks ',' str)
     where
       langs = languages pgf

       completeLang pgf la =
         mplus (Map.lookup la                       langs)
               (Map.lookup (abstractName pgf ++ la) langs)

   optFile opts = valStrOpts "file" "_gftmp" opts

   optType pgf opts =
     let readOpt str = case readType str of
                         Just ty -> case checkType pgf ty of
                                      Left err -> error err
                                      Right ty -> ty
                         Nothing -> error ("Can't parse '"++str++"' as a type")
     in maybeStrOpts "cat" (startCat pgf) readOpt opts
   optViewFormat opts = valStrOpts "format" "png" opts
   optViewGraph opts = valStrOpts "view" open_cmd opts
   optNum opts = valIntOpts "number" 1 opts
   optNumInf opts = valIntOpts "number" 1000000000 opts ---- 10^9
   takeOptNum opts = take (optNumInf opts)

   open_cmd | os == "linux"   = "xdg-open"
            | os == "mingw32" = "start"
            | otherwise       = "open"

   returnFromExprs show_p es =
     return $ 
       case es of
         [] -> pipeMessage "no trees found"
         _  -> fromExprs show_p es

   prGrammar pgf opts
     | isOpt "pgf"      opts = do
          let outfile = valStrOpts "file" (abstractName pgf ++ ".pgf") opts
          restricted $ writePGF outfile pgf (Just (map concreteName (optLangs pgf opts)))
          putStrLn $ "wrote file " ++ outfile
          return void
     | isOpt "cats"     opts = return $ fromString $ unwords $ categories pgf
     | isOpt "funs"     opts = return $ fromString $ unlines [showFun pgf f ty | f <- functions pgf, Just ty <- [functionType pgf f]]
     | isOpt "fullform" opts = return $ fromString $ concatMap prFullFormLexicon $ optLangs pgf opts
     | isOpt "langs"    opts = return $ fromString $ unwords $ Map.keys $ languages pgf

     | isOpt "lexc"     opts = return $ fromString $ concatMap prLexcLexicon $ optLangs pgf opts
     | isOpt "missing"  opts = return $ fromString $ unlines $ [unwords (concreteName concr:":":[f | f <- functions pgf, not (hasLinearization concr f)]) |
                                                                  concr <- optLangs pgf opts]
     | isOpt "words" opts = return $ fromString $ concatMap prAllWords $ optLangs pgf opts
     | otherwise             = do fmt <- readOutputFormat (valStrOpts "printer" "pgf_pretty" opts)
                                  return $ fromString $ concatMap snd $ exportPGF noOptions fmt pgf

   showFun pgf id ty = kwd++" "++ id ++ " : " ++ showType [] ty
                       where
                         kwd | functionIsConstructor pgf id = "data"
                             | otherwise                    = "fun"

   morphos pgf opts s =
     [(s,lookupMorpho concr s) | concr <- optLangs pgf opts]

   morphoCohorts f concr s = f (lookupCohorts concr s)

   morphoKnown = morphoClassify True

   morphoMissing = morphoClassify False

   morphoClassify k concr s =
     [w | (_,w,ans,_) <- lookupCohorts concr s, k /= null ans, notLiteral w]
     where
       notLiteral w = not (all isDigit w)

   optClitics opts = case valStrOpts "clitics" "" opts of
     "" -> []
     cs -> map reverse $ chunks ',' cs

   mexp xs = case xs of
     t:_ -> Just t
     _   -> Nothing

   -- ps -f -g s returns g (f s)
   treeOps pgf opts s = foldr app s (reverse opts) where
     app (OOpt  op)          | Just (Left  f) <- treeOp pgf op = f
     app (OFlag op (LStr x)) | Just (Right f) <- treeOp pgf op = f x
     app _                                                     = id

treeOpOptions pgf = [(op,expl) | (op,(expl,Left  _)) <- allTreeOps pgf]
treeOpFlags   pgf = [(op,expl) | (op,(expl,Right _)) <- allTreeOps pgf]

translationQuiz :: Maybe Expr -> PGF -> Concr -> Concr -> Type -> IO ()
translationQuiz mex pgf ig og typ = do
  tts <- translationList mex pgf ig og typ infinity
  mkQuiz "Welcome to GF Translation Quiz." tts

morphologyQuiz :: Maybe Expr -> PGF -> Concr -> Type -> IO ()
morphologyQuiz mex pgf ig typ = do
  tts <- morphologyList mex pgf ig typ infinity
  mkQuiz "Welcome to GF Morphology Quiz." tts

-- | the maximal number of precompiled quiz problems
infinity :: Int
infinity = 256

prLexcLexicon :: Concr -> String
prLexcLexicon concr =
  unlines $ "Multichar_Symbols":multichars:"":"LEXICON Root" : [prLexc l p ++ ":" ++ w  ++ " # ;" | (w,lps) <- morpho, (l,p,_) <- lps] ++ ["END"]
 where
  morpho = fullFormLexicon concr
  prLexc l p = l ++ concat (mkTags (words p))
  mkTags p = case p of
    "s":ws -> mkTags ws   --- remove record field
    ws -> map ('+':) ws

  multichars = unwords $ nub $ concat [mkTags (words p) | (w,lps) <- morpho, (l,p,_) <- lps]

prFullFormLexicon :: Concr -> String
prFullFormLexicon concr =
  unlines (map prMorphoAnalysis (fullFormLexicon concr))

prAllWords :: Concr -> String
prAllWords concr =
  unwords [w | (w,_) <- fullFormLexicon concr]

prMorphoAnalysis (w,lps) =
  unlines (w:[l ++ " : " ++ p ++ " " ++ show prob | (l,p,prob) <- lps])

prCohortAnalysis (i,w,lps,j) =
  unlines ((show i++"-"++show j++" "++w):[l ++ " : " ++ p ++ " " ++ show prob | (l,p,prob) <- lps])

viewGraphviz :: String -> String -> String -> [String] -> SIO CommandOutput
viewGraphviz view format name grphs = do
           let file i s = name ++ i ++ "." ++ s
           mapM_ (\ (grph,i) -> restricted $ writeUTF8File (file (show i) "dot") grph) (zip grphs [1..])
           mapM_ (\i -> restrictedSystem $ "dot -T" ++ format ++ " " ++ file (show i) "dot" ++ " > " ++ file (show i) format) [1..length grphs]
           if length grphs > 1
             then do
               let files = unwords [file (show i) format | i <- [1..length grphs]]
               restrictedSystem $ "convert " ++ files ++ " " ++ file "all" "pdf"
               restrictedSystem $ view ++ " " ++ file "all" "pdf"
             else restrictedSystem $ view ++ " " ++ file "1" format
---           restrictedSystem $ "rm " ++ file "*" format  --- removing temporary files
---           restrictedSystem $ "rm " ++ file "*" "dot"
---           restrictedSystem $ "rm " ++ file "all" "pdf"
           return void

viewLatex :: String -> String -> [String] -> SIO CommandOutput
viewLatex view name grphs = do
  let texfile = name ++ ".tex"
  let pdffile = name ++ ".pdf"
  restricted $ writeUTF8File texfile (latexDoc grphs)
  restrictedSystem $ "pdflatex " ++ texfile
  restrictedSystem $ view ++ " " ++ pdffile
  return void
  
---- copied from VisualizeTree ; not sure about proper place AR Nov 2015
latexDoc :: [String] -> String
latexDoc body = unlines $
    "\\batchmode"
  : "\\documentclass{article}"
  : "\\usepackage[utf8]{inputenc}"  
  : "\\begin{document}"
  : spaces body
  ++ ["\\end{document}"]
 where
   spaces = intersperse "\\vspace{6mm}"
   ---- also reduce the size for long sentences

stanzas :: String -> [String]
stanzas = map unlines . chop . lines where
  chop ls = case break (=="") ls of
    (ls1,[])  -> [ls1]
    (ls1,_:ls2) -> ls1 : chop ls2

#if !(MIN_VERSION_base(4,9,0))
errorWithoutStackTrace = error
#endif
