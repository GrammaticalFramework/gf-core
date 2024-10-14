{-# LANGUAGE CPP, ScopedTypeVariables #-}
module GF.Server.PGFService(pgfMain,
                            Caches,newPGFCache,readCachedPGF,readCachedNGF,
                            flushPGFCache,listPGFCache) where

import PGF2
import PGF2.Transactions
import GF.Server.Cache

import Data.Time.Format(formatTime)
import Data.Time.Format(defaultTimeLocale,rfc822DateFormat)
import Text.JSON

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Char
import qualified Data.Map as Map
import Data.Maybe
import Data.Time (UTCTime)
import System.Process
import System.Exit
import System.IO
import System.IO.Error(isDoesNotExistError)
import System.FilePath(takeExtension)
import System.Mem(performGC)
import Network.HTTP
import Numeric(showHex)


data Caches = Caches { qsem :: QSem,
                       pgfCache :: Cache PGF,
                       ngfCache :: Cache PGF,
                       labelsCache :: Cache Labels }

newPGFCache root jobs = do
  let n = maybe 4 id jobs
  qsem <- newQSem n
  pgfCache <- newCache' root (const readPGF)
  ngfCache <- newCache' root (maybe readNGF (const . checkoutPGF))
  lblCache <- newCache' root (const (fmap getDepLabels . readFile))
  return $ Caches qsem pgfCache ngfCache lblCache

readCachedPGF :: Caches -> FilePath -> IO PGF
readCachedPGF = readCache . pgfCache

readCachedNGF :: Caches -> FilePath -> IO PGF
readCachedNGF = readCache . ngfCache

flushPGFCache c = do flushCache (pgfCache c)
                     flushCache (ngfCache c)
                     flushCache (labelsCache c)

listPGFCache c = liftM2 (++) (listCache (pgfCache c)) (listCache (ngfCache c))

newCache' root rd = do 
  c <- newCache root rd
  forkIO $ forever $ clean c
  return c
  where
    clean c = do threadDelay 2000000000 -- 2000 seconds, i.e. ~33 minutes
                 expireCache (24*60*60) c -- 24 hours

pgfMain :: (String -> IO ()) -> Connection -> Caches -> FilePath -> Request -> IO ()
pgfMain logLn conn cache path rq =
  case fromMaybe "grammar" (lookup "command" query) of
    "download"
      | ext == ".pgf" -> do tpgf <- getFile (readCache' (pgfCache cache)) path
                            pgfDownload conn query tpgf
      | ext == ".ngf" -> do tpgf <- getFile (readCache' (ngfCache cache)) path
                            pgfDownload conn query tpgf
    command
      | ext == ".pgf" -> do tpgf <- getFile (readCache' (pgfCache cache)) path
                            handleErrors logLn (pgfCommand (qsem cache) command query tpgf) >>= respondHTTP conn
      | ext == ".ngf" -> do tpgf <- getFile (readCache' (ngfCache cache)) path
                            handleErrors logLn (pgfCommand (qsem cache) command query tpgf) >>= respondHTTP conn
    _                 -> respondHTTP conn (Response 415 "Bad Request" [] "Extension must be .pgf or .ngf")
    where
      ext  = takeExtension path
                           
      query = rqQuery rq

      getFile get path =
        (get path)
        `catch`
        (\e ->
            if isDoesNotExistError e
              then notFound path
              else ioError e)

pgfCommand qsem command q (t,pgf) =
  case command of
    "parse"         -> withQSem qsem $
                       out q t=<< join (parse # input % cat % start % limit % treeopts)
--    "parseToChart"  -> withQSem qsem $
--                       out q t=<< join (parseToChart # input % cat % limit)
    "linearize"     -> out q t=<< lin # tree % to
    "bracketedLinearize"
                    -> out q t=<< bracketedLin # tree % to
    "linearizeAll"  -> out q t=<< linAll # tree % to
    "translate"     -> withQSem qsem $
                       out q t=<<join(trans # input % cat % to % start % limit%treeopts)
    "lookupmorpho"  -> out q t=<< morpho # from % textInput
    "lookupcohorts" -> out q t=<< cohorts # from % filter % textInput
    "flush"         -> out q t=<< flush
    "grammar"       -> out q t grammar
    "abstrtree"     -> outputGraphviz=<< graphvizAbstractTree pgf graphvizDefaults # tree
    "parsetree"     -> outputGraphviz=<< (\cnc -> graphvizParseTree cnc graphvizDefaults) . snd # from % tree
    "wordforword"   -> out q t =<< wordforword # input % cat % to
    _               -> badRequest "Unknown command" command
  where
    flush = do performGC
               return $ showJSON ()

    cat :: IO Type
    cat =
       case lookup "cat" q of
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
        case PGF2.parse concr cat (init input) of
          ParseOk ts        -> return (Right (maybe id take mlimit (drop start ts)))
          ParseFailed _ tok -> return (Left tok)
          ParseIncomplete   -> return (Left "")

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
        [makeObj ["to".=to,"brackets".=bracketedLinearize c tree]|(to,c)<-tos]

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
      showJSON [makeObj ["start" .=s
                        ,"word"  .=w
                        ,"morpho".=[makeObj ["lemma".=l
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

    getLangs i = mapM readLang (maybe [] words (lookup i q))

    getLang i =
      case lookup i q of
        Just lang -> fmap Just (readLang lang)
        _         -> return Nothing

    readLang :: String -> IO (String,Concr)
    readLang lang =
      case Map.lookup lang langs of
        Nothing -> badRequest "Bad language" lang
        Just c  -> return (lang,c)

    tree = do s <- maybe (missing "tree") return (lookup "tree" q)
              maybe (badRequest "bad tree" s) return (readExpr s)

    textInput = maybe (missing "input") return (lookup "input" q)
    
    filter = return (lookup "filter" q)
    
    start = 
      case lookup "start" q of
        Just s  -> case reads s of
                     [(val,"")] -> return val
                     _          -> badRequest "bad start" s
        Nothing -> return 0

    limit = 
      case lookup "limit" q of
        Just s  -> case reads s of
                     [(val,"")] -> return (Just val)
                     _          -> badRequest "bad limit" s
        Nothing -> return Nothing

    treeopts = (,) # getBool "trie" % getBool "jsontree"

    getBool x = return (maybe False toBool (lookup x q))
      where
        toBool s = s `elem` ["","yes","true","True"]

    outputGraphviz code
      | fmt == "dot" = outputGraph code
      | otherwise    = outputGraph =<< pipeIt2graphviz fmt code
      where
        fmt = fromMaybe "png" (lookup "format" q)

        outputGraph body =
          return (Response
            { rspCode = 200
            , rspReason = "OK"
            , rspHeaders = [Header HdrContentType mimeType]
            , rspBody = body
            })

        mimeType =
          case fmt of
            "png" -> "image/png"
            "gif" -> "image/gif"
            "svg" -> "image/svg+xml"
            "dot" -> "text/x-graphviz; charset=UTF8"
            _     -> "application/binary"

pgfDownload conn query (t,pgf) = do
  let fmt      = formatTime defaultTimeLocale rfc822DateFormat t
      mb_langs = fmap words (lookup "lang" query)
  writeHeaders conn (Response
                        { rspCode = 200
                        , rspReason = "OK"
                        , rspHeaders = [ Header HdrContentType "application/pgf"
                                       , Header HdrContentDisposition ("attachment; filename=\""++abstractName pgf++".pgf\"")
                                       , Header HdrTransferEncoding "chunked"
                                       , Header HdrDate fmt
                                       ]
                        , rspBody = ""
                        })
  writePGF_ (writeChunk conn) pgf mb_langs
  writeAscii conn "0\r\n\r\n"
  where
    writeChunk conn ptr len =
      (do writeAscii conn (showHex len "\r\n")
          writeBytes conn ptr len
          writeAscii conn "\r\n"
          return len)
      `catch`
      (\(e :: SomeException) -> return (-1))

out :: JSON a => Query -> UTCTime -> a -> IO Response
out q t r = do
  let (ty,str) = case lookup "jsonp" q of
                   Nothing -> ("json",encode r)
                   Just c  -> ("javascript",c ++ "(" ++ encode r ++ ")")
      fmt = formatTime defaultTimeLocale rfc822DateFormat t
  return (Response
            { rspCode = 200
            , rspReason = "OK"
            , rspHeaders = [Header HdrContentType ("application/"++ty++"; charset=utf-8")
                           ,Header HdrLastModified fmt
                           ]
            , rspBody = str
            })
{-
getInput1 x = nonEmpty # getInput x
nonEmpty (Just "") = Nothing
nonEmpty r = r

limit, depth :: CGI (Maybe Int)
limit = readInput "limit"
depth = readInput "depth"
-}
missing = badRequest "Missing parameter"
errorMissingId = badRequest "Missing identifier" ""

notFound = httpError 404 "Not found"
badRequest = httpError 400

pipeIt2graphviz :: String -> String -> IO String
pipeIt2graphviz fmt code = do
    (Just inh, Just outh, _, pid) <-
        createProcess (proc "dot" ["-T",fmt])
                      { std_in  = CreatePipe,
                        std_out = CreatePipe,
                        std_err = Inherit }

    hSetBinaryMode outh True
    hSetEncoding inh utf8

    -- fork off a thread to start consuming the output
    output  <- hGetContents outh
    outMVar <- newEmptyMVar
    _ <- forkIO $ evaluate (length output) >> putMVar outMVar ()

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
{-
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
-}

-- * General utilities

infixl 2 #,%

f .= v = (f,showJSON v)
f # x = fmap f x
f % x = ap f x


withQSem qsem = bracket_ (waitQSem qsem) (signalQSem qsem)
