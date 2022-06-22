{-# LANGUAGE CPP #-}
module PGFService(cgiMain,cgiMain',getPath,
                  logFile,stderrToFile,
                  Caches,pgfCache,newPGFCache,flushPGFCache,listPGFCache) where

import PGF2
import PGF2.Transactions
import Cache
import Network.CGI(CGI,readInput,getInput,getVarWithDefault,
                   CGIResult,handleErrors,setHeader,
                   Accept(..),Language(..),negotiate,liftIO)
import CGIUtils(outputJSONP,outputPlain,
                outputBinary,outputBinary',
                handleCGIErrors,throwCGIError,stderrToFile)
import URLEncoding

import Data.Time.Format(formatTime)
import Data.Time.Format(defaultTimeLocale,rfc822DateFormat)
import Text.JSON
import qualified Codec.Binary.UTF8.String as UTF8 (decodeString)
import qualified Data.ByteString.Lazy as BS

import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.Catch(bracket_)
import Data.Char
import qualified Data.Map as Map
import Data.Maybe
import System.Process
import System.Exit
import System.IO
import System.IO.Error(isDoesNotExistError)
import System.FilePath(takeExtension)
import System.Mem(performGC)

catchIOE :: IO a -> (E.IOException -> IO a) -> IO a
catchIOE = E.catch

withQSem qsem = bracket_ (liftIO $ waitQSem qsem) (liftIO $ signalQSem qsem)

logFile :: FilePath
logFile = "pgf-error.log"

data Caches = Caches { qsem :: QSem,
                       pgfCache :: Cache PGF,
                       labelsCache :: Cache Labels }

newPGFCache jobs = do let n = maybe 4 id jobs
                      qsem <- newQSem n
                      pgfCache <- newCache' readGrammar
                      lblCache <- newCache' (const (fmap getDepLabels . readFile))
                      return $ Caches qsem pgfCache lblCache
flushPGFCache c = do flushCache (pgfCache c)
                     flushCache (labelsCache c)
listPGFCache c = listCache (pgfCache c)

readGrammar mb_pgf path =
  case takeExtension path of
    ".pgf" -> readPGF path
    ".ngf" -> case mb_pgf of
                Nothing -> readNGF path
                Just gr -> checkoutPGF gr
    _      -> error "Extension must be .pgf or .ngf"

newCache' rd = do c <- newCache rd
                  forkIO $ forever $ clean c
                  return c
  where
    clean c = do threadDelay 2000000000 -- 2000 seconds, i.e. ~33 minutes
                 expireCache (24*60*60) c -- 24 hours

getPath =
    do path <- getVarWithDefault "PATH_TRANSLATED" "" -- apache mod_fastcgi
       if null path
          then getVarWithDefault "SCRIPT_FILENAME" "" -- lighttpd
          else return path

cgiMain :: Caches -> CGI CGIResult
cgiMain cache = handleErrors . handleCGIErrors $
                  cgiMain' cache =<< getPath

cgiMain' :: Caches -> FilePath -> CGI CGIResult
cgiMain' cache path =
    do command <- liftM (maybe "grammar" (urlDecodeUnicode . UTF8.decodeString))
                        (getInput "command")
       case command of
         "download" -> outputBinary =<< getFile BS.readFile path
         _ -> do tpgf <- getFile (readCache' (pgfCache cache)) path
                 pgfMain (qsem cache) command tpgf

getFile get path =
   either failed return =<< liftIO (E.try (get path))
  where
    failed e = if isDoesNotExistError e
               then notFound path
               else liftIO $ ioError e


pgfMain qsem command (t,pgf) =
  case command of
    "parse"         -> withQSem qsem $
                       out t=<< join (parse # input % cat % start % limit % treeopts)
--    "parseToChart"  -> withQSem qsem $
--                       out t=<< join (parseToChart # input % cat % limit)
    "linearize"     -> out t=<< lin # tree % to
    "bracketedLinearize"
                    -> out t=<< bracketedLin # tree % to
    "linearizeAll"  -> out t=<< linAll # tree % to
    "translate"     -> withQSem qsem $
                       out t=<<join(trans # input % cat % to % start % limit%treeopts)
    "lookupmorpho"  -> out t=<< morpho # from % textInput
    "lookupcohorts" -> out t=<< cohorts # from % getInput "filter" % textInput
    "flush"         -> out t=<< flush
    "grammar"       -> out t grammar
    "abstrtree"     -> outputGraphviz=<< graphvizAbstractTree pgf graphvizDefaults # tree
    "parsetree"     -> outputGraphviz=<< (\cnc -> graphvizParseTree cnc graphvizDefaults) . snd # from %tree
    "wordforword"   -> out t =<< wordforword # input % cat % to
    _               -> badRequest "Unknown command" command
  where
    flush = liftIO $ do --modifyMVar_ pc $ const $ return Map.empty
                        performGC
                        return $ showJSON ()

    cat :: CGI Type
    cat =
       do mcat  <- getInput1 "cat"
          case mcat of
            Nothing -> return (startCat pgf)
            Just cat -> case readType cat of
                          Nothing  -> badRequest "Bad category" cat
                          Just typ -> return typ

    langs = languages pgf

    grammar = showJSON $ makeObj
                 ["name".=abstractName pgf,
                  "lastmodified".=show t,
                  "startcat".=showType [] (startCat pgf),
                  "languages".=languages]
      where
        languages = [makeObj ["name".= l] | (l,_)<-Map.toList langs]

    parse input@((from,_),_) cat start mlimit (trie,json) =
        do r <- parse' cat start mlimit input
           return $ showJSON [makeObj ("from".=from:jsonParseResult json r)]

    jsonParseResult json = either bad good
      where
        bad err = ["parseFailed".=err]
        good trees = "trees".=map tp trees :[]  -- :addTrie trie trees
        tp (tree,prob) = makeObj ["tree".=tree
                                 ,"prob".=prob
                                 ]

    -- Without caching parse results:
    parse' cat start mlimit ((from,concr),input) =
        case parseWithHeuristics concr cat input (-1) callbacks of
          ParseOk ts        -> return (Right (maybe id take mlimit (drop start ts)))
          ParseFailed _ tok -> return (Left tok)
          ParseIncomplete   -> return (Left "")
        where
          callbacks = undefined

    parseToChart ((from,concr),input) cat mlimit = undefined {-
      do r <- case C.parseToChart concr cat input (-1) callbacks (fromMaybe 5 mlimit) of
                ParseOk chart     -> return (good chart)
                ParseFailed _ tok -> return (bad tok)
                ParseIncomplete   -> return (bad "")
         return $ showJSON [makeObj ("from".=from:r)]
      where
        callbacks = maybe [] cb $ lookup (C.abstractName pgf) C.literalCallbacks
        cb fs = [(cat,f pgf (from,concr) input)|(cat,f)<-fs]

        bad  err           = ["parseFailed".=err]
        good (roots,chart) = ["roots".=showJSON roots,
                              "chart".=makeObj [show fid .= mkChartObj inf | (fid,inf)<-Map.toList chart]]

        mkChartObj (brackets,prods,cat) =
          makeObj ["brackets".=map mkChartBracket brackets
                  ,"prods"   .=map mkChartProd prods
                  ,"cat"     .=cat
                  ]

        mkChartBracket (s,e,ann) =
          makeObj ["start".=s,"end".=e,"ann".=ann]

        mkChartProd (expr,args,prob) =
          makeObj ["tree".=expr,"args".=map mkChartPArg args,"prob".=prob]

        mkChartPArg (PArg _ fid) = showJSON fid
-}
    linAll tree to = showJSON (linAll' tree to)
    linAll' tree tos =
        [makeObj ["to".=to,
                  "texts".=linearizeAll c tree]|(to,c)<-tos]

    lin tree to = showJSON (lin' tree to)
    lin' tree tos =
        [makeObj ["to".=to,"text".=linearize c tree]|(to,c)<-tos]

    bracketedLin tree to = showJSON (bracketedLin' tree to)
    bracketedLin' tree tos =
        [makeObj ["to".=to,"brackets".=showJSON (bracketedLinearize c tree)]|(to,c)<-tos]

    trans input@((from,_),_) cat to start mlimit (trie,jsontree) =
      do parses <- parse' cat start mlimit input
         return $
           showJSON [ makeObj ["from".=from,
                               "translations".= jsonParses parses]]
      where
        jsonParses = either bad good
          where
            bad err = [makeObj ["error".=err]]
            good parses = [makeObj ["tree".=tree
                                   ,"prob".=prob
                                   ,"linearizations".=lin' tree to]
                                    | (tree,prob) <- parses]

    morpho (from,concr) input =
        showJSON [makeObj ["lemma".=l
                          ,"analysis".=a
                          ,"prob".=p]
                     | (l,a,p)<-lookupMorpho concr input]

    cohorts (from,concr) filter input =
      showJSON [makeObj ["start" .=showJSON s
                        ,"word"  .=showJSON w
                        ,"morpho".=showJSON [makeObj ["lemma".=l
                                                     ,"analysis".=a
                                                     ,"prob".=p] 
                                                | (l,a,p)<-ms]
                        ,"end"   .=showJSON e
                        ]
                   | (s,w,ms,e) <- (case filter of
                                      Just "longest" -> filterLongest
                                      Just "best"    -> filterBest
                                      _              -> id)
                                     (lookupCohorts concr input)]

    wordforword input@((from,_),_) cat = jsonWFW from . wordforword' input cat
      where
         jsonWFW from rs =
           showJSON
             [makeObj
               ["from".=from,
                "translations".=[makeObj ["linearizations".=
                                             [makeObj["to".=to,"text".=text]
                                                | (to,text)<-rs]]]]]

    wordforword' inp@((from,concr),input) cat tos =
        [(to,unwords $ map (lin_word' c) pws)
         |let pws=map parse_word' (words input),(to,c)<-tos]
      where
        lin_word' c = either id (lin1 c)

        lin1 c = dropq . linearize c
          where
            dropq (q:' ':s) | q `elem` "+*" = s
            dropq s = s

        parse_word' w = if all (\c->isSpace c||isPunctuation c) w
                        then Left w
                        else parse_word w


        parse_word w =
            maybe (Left ("["++w++"]")) Right $
            msum [parse1 w,parse1 ow,morph w,morph ow]
          where
            ow = case w of
                   c:cs | isLower c -> toUpper c : cs
                        | isUpper c -> toLower c : cs
                   s                -> s

            parse1 s = case PGF2.parse concr cat s of
                         ParseOk ((t,_):ts) -> Just t
                         _                  -> Nothing
            morph w = listToMaybe
                        [t | (f,a,p)<-lookupMorpho concr w,
                             t<-maybeToList (readExpr f)]

    ---

    input = (,) # from % textInput

    from = maybe (missing "from") return =<< getLang "from"

    to = getLangs "to"

    getLangs i = mapM readLang . maybe [] words =<< getInput i

    getLang i = do
      mlang <- getInput i
      case mlang of
        Just lang@(_:_) -> Just # readLang lang
        _               -> return Nothing

    readLang :: String -> CGI (String,Concr)
    readLang lang =
      case Map.lookup lang langs of
        Nothing -> badRequest "Bad language" lang
        Just c -> return (lang,c)

    tree = do s <- maybe (missing "tree") return =<< getInput1 "tree"
              maybe (badRequest "bad tree" s) return (readExpr s)


out t r = do let fmt = formatTime defaultTimeLocale rfc822DateFormat t
             setHeader "Last-Modified" fmt
             outputJSONP r

getInput1 x = nonEmpty # getInput x
nonEmpty (Just "") = Nothing
nonEmpty r = r

textInput :: CGI String
textInput = liftM (maybe "" (urlDecodeUnicode . UTF8.decodeString)) $ getInput "input"

limit, depth :: CGI (Maybe Int)
limit = readInput "limit"
depth = readInput "depth"

start :: CGI Int
start = maybe 0 id # readInput "start"

treeopts :: CGI TreeOpts
treeopts = (,) # getBool "trie" % getBool "jsontree"

getBool x = maybe False toBool # getInput x
toBool s = s `elem` ["","yes","true","True"]

missing = badRequest "Missing parameter"
errorMissingId = badRequest "Missing identifier" ""

notFound = throw 404 "Not found"
badRequest = throw 400

throw code msg extra =
    throwCGIError code msg [msg ++(if null extra then "" else ": "++extra)]

format def = maybe def id # getInput "format"

type From = (Maybe Concr,String)
type TreeOpts = (Bool,Bool) -- (trie,jsontree)

outputGraphviz code =
  do fmt <- format "png"
     case fmt of
       "gv" -> outputPlain code
       _ -> outputFPS' fmt =<< liftIO (pipeIt2graphviz fmt code)
  where
    outputFPS' = outputBinary' . mimeType

    mimeType fmt =
      case fmt of
        "png" -> "image/png"
        "gif" -> "image/gif"
        "svg" -> "image/svg+xml"
    -- ...
        _     -> "application/binary"

pipeIt2graphviz :: String -> String -> IO BS.ByteString
pipeIt2graphviz fmt code = do
    (Just inh, Just outh, _, pid) <-
        createProcess (proc "dot" ["-T",fmt])
                      { std_in  = CreatePipe,
                        std_out = CreatePipe,
                        std_err = Inherit }

    hSetBinaryMode outh True
    hSetEncoding inh  utf8

    -- fork off a thread to start consuming the output
    output  <- BS.hGetContents outh
    outMVar <- newEmptyMVar
    _ <- forkIO $ E.evaluate (BS.length output) >> putMVar outMVar ()

    -- now write and flush any input
    hPutStr inh code
    hFlush inh
    hClose inh -- done with stdin

    -- wait on the output
    takeMVar outMVar
    hClose outh

    -- wait on the process
    ex <- waitForProcess pid

    case ex of
     ExitSuccess   -> return output
     ExitFailure r -> fail ("pipeIt2graphviz: (exit " ++ show r ++ ")")

instance JSON Expr where
    readJSON x = readJSON x >>= maybe (fail "Bad expression.") return . readExpr
    showJSON = showJSON . showExpr []

instance JSON BracketedString where
    readJSON x = return (Leaf "")
    showJSON (Bracket cat fid index fun bs) =
        makeObj ["cat".=cat, "fid".=fid, "index".=index, "fun".=fun, "children".=bs]
    showJSON BIND     = makeObj ["bind".=True]
    showJSON (Leaf s) = makeObj ["token".=s]

-- * PGF utilities

selectLanguage :: PGF -> Maybe (Accept Language) -> Concr
selectLanguage pgf macc = case acceptable of
                            []  -> case Map.elems (languages pgf) of
                                     []  -> error "No concrete syntaxes in PGF grammar."
                                     l:_ -> l
                            Language c:_ -> fromJust (langCodeLanguage pgf c)
  where langCodes = mapMaybe languageCode (Map.elems (languages pgf))
        acceptable = negotiate (map Language langCodes) macc

langCodeLanguage :: PGF -> String -> Maybe Concr
langCodeLanguage pgf code = listToMaybe [concr | concr <- Map.elems (languages pgf), languageCode concr == Just code]

-- * General utilities

infixl 2 #,%

f .= v = (f,showJSON v)
f # x = fmap f x
f % x = ap f x
