{-# LANGUAGE CPP, ScopedTypeVariables, FlexibleInstances #-}
-- | GF interactive mode
module GF.Interactive (mainGFI,mainRunGFI,mainServerGFI) where

import Prelude hiding (putStrLn,print)
import qualified Prelude as P(putStrLn)
import GF.Command.Interpreter(CommandEnv(..),mkCommandEnv,interpretCommandLine)
import GF.Command.Commands(HasPGF(..),pgfCommands)
import GF.Command.CommonCommands(commonCommands,extend)
import GF.Command.SourceCommands
import GF.Command.CommandInfo
import GF.Command.Help(helpCommand)
import GF.Command.Abstract
import GF.Command.Parse(readCommandLine,pCommand,readTransactionCommand)
import GF.Compile.Rename(renameSourceTerm)
import GF.Compile.TypeCheck.Concrete(inferLType)
import GF.Compile.TypeCheck.Primitives(predefMod)
import GF.Compile.GeneratePMCFG(pmcfgForm,type2fields)
import GF.Data.Operations (Err(..))
import GF.Data.Utilities(whenM,repeatM)
import GF.Grammar hiding (Ident,isPrefixOf)
import GF.Infra.UseIO(ioErrorText,putStrLnE)
import GF.Infra.SIO
import GF.Infra.Option
import GF.Infra.CheckM
import qualified System.Console.Haskeline as Haskeline

import PGF2
import PGF2.Transactions hiding (modifyPGF,checkoutPGF,
                                 startTransaction,
                                 commitTransaction,rollbackTransaction,
                                 inTransaction)

import Data.Char
import Data.List(isPrefixOf,sortOn)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Text.ParserCombinators.ReadP as RP
import System.Directory(getAppUserDataDirectory)
import Control.Exception(SomeException,fromException,evaluate,try)
import Control.Monad.State hiding (void)
import qualified GF.System.Signal as IO(runInterruptibly)
import GF.Command.Messages(welcome)
#ifdef SERVER_MODE
import GF.Server(server)
#endif

type ReadNGF = FilePath -> IO PGF

-- | Run the GF Shell in quiet mode (@gf -run@).
mainRunGFI :: Options -> [FilePath] -> IO ()
mainRunGFI opts files = shell (beQuiet opts) files

beQuiet = addOptions (modifyFlags (\f -> f{optVerbosity=Quiet}))

-- | Run the interactive GF Shell
mainGFI :: Options -> [FilePath] -> IO ()
mainGFI opts files = do
  P.putStrLn welcome
  shell opts files

shell opts files =
  flip evalStateT (emptyGFEnv opts) $
  do mapStateT runSIO $ importInEnv readNGF opts files
     modify $ \ gfenv0 -> gfenv0 {history = [unwords ("i":files)]}
     repeatM (mapStateT runSIO . execute1 readNGF =<< readCommand)


#ifdef SERVER_MODE
-- | Run the GF Server (@gf -server@).
-- The 'Int' argument is the port number for the HTTP service.
mainServerGFI opts0 port files =
  server jobs port root init execute
  where
    root = flag optDocumentRoot opts
    opts = beQuiet opts0
    jobs = join (flag optJobs opts)

    init readNGF = do
      (_, gfenv) <- runSIO (runStateT (importInEnv readNGF opts files) (emptyGFEnv opts))
      return gfenv

    execute readNGF gfenv0 cmd = do
      (continue,gfenv) <- runStateT (execute1 readNGF cmd) gfenv0
      return $ if continue then Just gfenv else Nothing

#else
mainServerGFI opts port files =
  fail "GF has not been compiled with server mode support"
#endif

-- | Read a command
readCommand :: StateT GFEnv IO String
readCommand =
  do opts <- gets startOpts
     case flag optMode opts of
       ModeRun -> lift tryGetLine
       _       -> do gfenv <- get
                     s <- lift (fetchCommand gfenv)
                     put $ gfenv {history = s : history gfenv}
                     return s

timeIt act =
  do t1 <- liftSIO $ getCPUTime
     a <-  act
     t2 <- liftSIO $ getCPUTime
     return (t2-t1,a)

-- | Optionally show how much CPU time was used to run an IO action
optionallyShowCPUTime :: (Monad m,MonadSIO m) => Options -> m a -> m a
optionallyShowCPUTime opts act
  | not (verbAtLeast opts Normal) = act
  | otherwise = do (dt,r) <- timeIt act
                   liftSIO $ putStrLnFlush $ show (dt `div` 1000000000) ++ " msec"
                   return r


type ShellM = StateT GFEnv SIO

-- | Execute a given command line, returning 'True' to continue execution,
-- | 'False' when it is time to quit
execute1 :: ReadNGF -> String -> ShellM Bool
execute1 readNGF s0 =
  do opts <- gets startOpts
     interruptible $ optionallyShowCPUTime opts $
       case pwords s0 of
      -- cc, sd, so, ss and dg are now in GF.Commands.SourceCommands
      -- special commands
         "q" :_   -> quit
         "!" :ws  -> system_command ws
         "eh":ws  -> execute_history ws
         "i" :ws  -> do import_ readNGF ws; continue
         "r" :_   -> do gfenv0 <- get
                        let imports = [(s,ws) | s <- history gfenv0, ("i":ws) <- [pwords s]]
                        case imports of
                          (s,ws):_ -> do
                            putStrLnE $ "repeating latest import: " ++ s
                            import_ readNGF ws
                            continue
                          _ -> do putStrLnE $ "no import in history"
                                  continue
         (w  :ws) | elem w ["c","a","d"] -> do
                        case readTransactionCommand s0 of
                          Just cmd -> do checkout
                                         env <- gets pgfenv
                                         case env of
                                           (_,Just pgf,mb_txnid) -> transactionCommand cmd pgf mb_txnid
                                           _                     -> fail "Import a grammar before using this command"
                          Nothing  -> putStrLnE $ "command not parsed: "++s0
                        continue
                  | w == "t" -> do
                        env <- gets pgfenv
                        case env of
                          (gr,Just pgf,mb_txnid) ->
                              case ws of
                                ["start"]     ->
                                    case mb_txnid of
                                      Just _  -> fail "You have already started a transaction"
                                      Nothing -> do txnid <- lift $ startTransaction pgf
                                                    modify (\gfenv -> gfenv{pgfenv=(gr,Just pgf,Just txnid)})
                                ["commit"]    ->
                                    case mb_txnid of
                                      Just id -> do lift $ commitTransaction id
                                                    modify (\gfenv -> gfenv{pgfenv=(gr,Just pgf,Nothing)})
                                      Nothing -> fail "There is no active transaction"
                                ["rollback"]  ->
                                    case mb_txnid of
                                      Just id -> do lift $ rollbackTransaction id
                                                    modify (\gfenv -> gfenv{pgfenv=(gr,Just pgf,Nothing)})
                                      Nothing -> fail "There is no active transaction"
                                []            -> fail "The transaction command expects start, commit or rollback as an argument"
                                _             -> fail "The only arguments to the transaction command are start, commit and rollback"
                          _                   -> fail "Import a grammar before using this command"
                        continue
                                           
      -- other special commands, working on GFEnv
         "dc":ws  -> define_command ws
         "dt":ws  -> define_tree ws
      -- ordinary commands
         _        -> do env <- gets commandenv
                        checkout
                        interpretCommandLine env s0
                        continue
  where
    continue,stop :: ShellM Bool
    continue = return True
    stop = return False

    checkout = do
      gfenv <- get
      case pgfenv gfenv of
        (gr,Just pgf,Nothing) -> do pgf <- lift $ checkoutPGF pgf
                                    put (gfenv{pgfenv = (gr,Just pgf,Nothing)})
        _                     -> return ()

    interruptible :: ShellM Bool -> ShellM Bool
    interruptible act =
      do gfenv <- get
         mapStateT (
           either (\e -> printException e >> return (True,gfenv)) return
             <=< runInterruptibly) act

  -- Special commands:

    quit = do
      env <- gets pgfenv
      case env of
        (_,_,Just _) -> fail "Commit or rollback the transaction first!"
        _            -> do opts <- gets startOpts
                           when (verbAtLeast opts Normal) $ putStrLnE "See you."
                           stop

    system_command ws = do lift $ restrictedSystem $ unwords ws ; continue

    execute_history [w] =
      do execute . lines =<< lift (restricted (readFile w))
         continue
      where
        execute []           = return ()
        execute (line:lines) = whenM (execute1 readNGF line) (execute lines)

    execute_history _   =
       do putStrLnE "eh command not parsed"
          continue

    define_command (f:ws) =
        case readCommandLine (unwords ws) of
           Just comm ->
             do modify $
                  \ gfenv ->
                    let env = commandenv gfenv
                    in gfenv {
                         commandenv = env {
                           commandmacros = Map.insert f comm (commandmacros env)
                         }
                       }
                continue
           _ -> dc_not_parsed
    define_command _ = dc_not_parsed

    dc_not_parsed = putStrLnE "command definition not parsed" >> continue

    define_tree (f:ws) =
        case readExpr (unwords ws) of
          Just exp ->
           do modify $
                \ gfenv ->
                  let env = commandenv gfenv
                  in gfenv { commandenv = env {
                               expmacros = Map.insert f exp (expmacros env) } }
              continue
          _ -> dt_not_parsed
    define_tree _ = dt_not_parsed

    dt_not_parsed = putStrLnE "value definition not parsed" >> continue

pwords s = case words s of
             w:ws -> getCommandOp w :ws
             ws -> ws

import_ readNGF args =
  do case parseOptions args of
       Ok (opts',files) -> do
         opts <- gets startOpts
         curr_dir <- lift getCurrentDirectory
         lib_dir  <- lift $ getLibraryDirectory (addOptions opts opts')
         importInEnv readNGF (addOptions opts (fixRelativeLibPaths curr_dir lib_dir opts')) files
       Bad err -> putStrLnE $ "Command parse error: " ++ err

transactionCommand :: TransactionCommand -> PGF -> Maybe TxnID -> ShellM ()
transactionCommand (CreateFun opts f ty) pgf mb_txnid = do
  let prob = realToFrac (valFltOpts "prob" (1/0) opts)
  case checkType pgf ty of
    Left msg -> putStrLnE msg
    Right ty -> do lift $ updatePGF pgf mb_txnid (createFunction f ty 0 [] prob >> return ())
                   return ()
transactionCommand (CreateCat opts c ctxt) pgf mb_txnid = do
  let prob = realToFrac (valFltOpts "prob" (1/0) opts)
  case checkContext pgf ctxt of
    Left msg -> putStrLnE msg
    Right ty -> do lift $ updatePGF pgf mb_txnid (createCategory c ctxt prob)
                   return ()
transactionCommand (CreateConcrete opts name) pgf mb_txnid = do
  lift $ updatePGF pgf mb_txnid (createConcrete name (return ()))
  return ()
transactionCommand (CreateLin opts f t is_alter) pgf mb_txnid = do
  sgr0 <- getGrammar
  let (sgr,mo) = case greatestResource sgr0 of
                   Nothing -> (mGrammar [predefMod], fst predefMod)
                   Just mo -> (sgr0,mo)
  lang <- optLang pgf opts
  lift $ updatePGF pgf mb_txnid $ do
    mb_ty <- getFunctionType f
    case mb_ty of
      Just ty@(DTyp _ cat _) -> 
         alterConcrete lang $ do
           mb_fields <- getCategoryFields cat
           case mb_fields of
             Just fields -> case runCheck (compileLinTerm sgr mo t (type2term mo ty)) of
                              Ok ((prods,seqtbl,fields'),_)
                                   | fields == fields' -> do
                                       (if is_alter then alterLin else createLin) f prods seqtbl
                                       return ()
                                   | otherwise -> fail "The linearization categories in the resource and the compiled grammar does not match"
                              Bad msg      -> fail msg
             Nothing     -> fail ("Category "++cat++" is not in the concrete syntax")
      _ -> fail ("Function "++f++" is not in the abstract syntax")
  where
    type2term mo (DTyp hypos cat _) =
      foldr (\(b,x,ty1) ty2 -> Prod b (identS x) (type2term mo ty1) ty2)
            (Vr (identS cat))
            hypos

    compileLinTerm sgr mo t ty = do
      t  <- renameSourceTerm sgr mo (Typed t ty)
      (t,ty) <- inferLType sgr [] t
      let (ctxt,res_ty) = typeFormCnc ty
      (prods,seqs) <- pmcfgForm sgr t ctxt res_ty Map.empty
      return (prods,mapToSequence seqs,type2fields sgr res_ty)
      where
        mapToSequence m = Seq.fromList (map (Left . fst) (sortOn snd (Map.toList m)))

transactionCommand (CreateLincat opts c t) pgf mb_txnid = do
  sgr0 <- getGrammar
  let (sgr,mo) = case greatestResource sgr0 of
                   Nothing -> (mGrammar [predefMod], fst predefMod)
                   Just mo -> (sgr0,mo)
  lang <- optLang pgf opts
  case runCheck (compileLincatTerm sgr mo t) of
    Ok (fields,_)-> do lift $ updatePGF pgf mb_txnid (alterConcrete lang (createLincat c fields [] [] Seq.empty >> return ()))
                       return ()
    Bad msg      -> fail msg
  where
    compileLincatTerm sgr mo t = do
      t <- renameSourceTerm sgr mo t
      (t,_) <- inferLType sgr [] t
      return (type2fields sgr t)
transactionCommand (DropFun opts f) pgf mb_txnid = do
  lift $ updatePGF pgf mb_txnid (dropFunction f)
  return ()
transactionCommand (DropCat opts c) pgf mb_txnid = do
  lift $ updatePGF pgf mb_txnid (dropCategory c)
  return ()
transactionCommand (DropConcrete opts name) pgf mb_txnid = do
  lift $ updatePGF pgf mb_txnid (dropConcrete name)
  return ()
transactionCommand (DropLin opts f) pgf mb_txnid = do
  lang <- optLang pgf opts
  lift $ updatePGF pgf mb_txnid (alterConcrete lang (dropLin f))
  return ()
transactionCommand (DropLincat opts c) pgf mb_txnid = do
  lang <- optLang pgf opts
  lift $ updatePGF pgf mb_txnid (alterConcrete lang (dropLincat c))
  return ()

updatePGF pgf mb_txnid f = do
  maybe (modifyPGF pgf f >> return ())
                                 (\txnid -> inTransaction txnid f)
                                 mb_txnid

optLang pgf opts =
  case Map.keys (languages pgf) of
    [lang] -> completeLang (valStrOpts "lang" lang opts)
    _      -> case valStrOpts "lang" "" opts of
                ""   -> fail "Specify a language to change"
                lang -> completeLang lang
  where
    langs = languages pgf

    completeLang la
      | Map.member la  langs = return la
      | Map.member la' langs = return la'
      | otherwise            = fail "Unknown language"
      where
        la' = abstractName pgf ++ la


-- | Commands that work on 'GFEnv'
moreCommands = [
  ("e",  emptyCommandInfo {
     longname = "empty",
     synopsis = "empty the environment (except the command history)",
     exec = \ _ _ ->
            do modify $ \ gfenv -> (emptyGFEnv (startOpts gfenv))
                                     { history=history gfenv }
               return void
     }),
  ("ph", emptyCommandInfo {
     longname = "print_history",
     synopsis = "print command history",
     explanation = unlines [
       "Prints the commands issued during the GF session.",
       "The result is readable by the eh command.",
       "The result can be used as a script when starting GF."
       ],
     examples = [
      mkEx "ph | wf -file=foo.gfs  -- save the history into a file"
      ],
     exec = \ _ _ ->
            fmap (fromString . unlines . reverse . drop 1 . history) get
     }),
  ("r",  emptyCommandInfo {
     longname = "reload",
     synopsis = "repeat the latest import command"
     })
  ]


printException e = maybe (print e) (putStrLn . ioErrorText) (fromException e)

fetchCommand :: GFEnv -> IO String
fetchCommand gfenv = do
  path <- getAppUserDataDirectory "gf_history"
  let settings =
        Haskeline.Settings {
          Haskeline.complete = wordCompletion gfenv,
          Haskeline.historyFile = Just path,
          Haskeline.autoAddHistory = True
        }
  res <- IO.runInterruptibly $ Haskeline.runInputT settings (Haskeline.getInputLine (prompt gfenv))
  case res of
    Left  _        -> return ""
    Right Nothing  -> return "q"
    Right (Just s) -> return s

importInEnv :: ReadNGF -> Options -> [FilePath] -> ShellM ()
importInEnv readNGF opts files =
  do (_,pgf0,mb_txnid) <- gets pgfenv
     case (flag optRetainResource opts,mb_txnid) of
       (RetainAll,Nothing)      -> do src <- lift $ importSource opts Nothing files
                                      pgf <- lift $ link opts pgf0 src
                                      modify $ \gfenv -> gfenv{pgfenv = (snd src,Just pgf,Nothing)}
       (RetainSource,mb_txn)    -> do src <- lift $ importSource opts pgf0 files
                                      modify $ \gfenv -> gfenv{pgfenv = (snd src,pgf0,mb_txn)}
       (RetainCompiled,Nothing) -> do pgf <- lift $ importPGF pgf0
                                      modify $ \gfenv -> gfenv{pgfenv = (emptyGrammar,pgf,Nothing)}
       _ -> fail "You must commit/rollback the transaction before loading a new grammar"
  where
    importPGF pgf0 =
      do let opts' = addOptions (setOptimization OptCSE False) opts
         pgf1 <- importGrammar readNGF pgf0 opts' files
         if (verbAtLeast opts Normal)
           then case pgf1 of
                  Just pgf -> putStrLnFlush $ unwords $ "\nLanguages:" : Map.keys (languages pgf)
                  Nothing  -> return ()
           else return ()
         return pgf1

tryGetLine = do
  res <- try getLine
  case res of
   Left (e :: SomeException) -> return "q"
   Right l -> return l

prompt env = 
  case pgfenv env of
    (_,mb_pgf,mb_tr) -> 
         maybe "" abstractName mb_pgf ++ 
         maybe "" (const " (transaction)") mb_tr ++
         "> "

type CmdEnv = (Grammar,Maybe PGF,Maybe TxnID)

data GFEnv = GFEnv {
    startOpts :: Options,
    pgfenv :: CmdEnv,
    commandenv :: CommandEnv ShellM,
    history    :: [String]
  }

emptyGFEnv opts = GFEnv opts emptyCmdEnv emptyCommandEnv []

emptyCmdEnv = (emptyGrammar,Nothing,Nothing)

emptyCommandEnv = mkCommandEnv allCommands

allCommands =
  extend pgfCommands (helpCommand allCommands:moreCommands)
  `Map.union` sourceCommands
  `Map.union` commonCommands

instance HasGrammar ShellM where
  getGrammar = gets $ \gfenv ->
    case pgfenv gfenv of
      (gr,_,_) -> gr

instance HasPGF ShellM where
  getPGF = gets $ \gfenv ->
    case pgfenv gfenv of
      (_,mb_pgf,_) -> mb_pgf

wordCompletion gfenv (left,right) = do
  case wc_type (reverse left) of
    CmplCmd pref
      -> ret (length pref) [Haskeline.simpleCompletion name | name <- Map.keys (commands cmdEnv), isPrefixOf pref name]
    CmplStr (Just (Command _ opts _)) s0
      -> case pgfenv gfenv of
           (_,Just pgf,_) ->
                       let langs = languages pgf
                           optLang opts = case valStrOpts "lang" "" opts of
                                            ""   -> case Map.minView langs of
                                                      Nothing        -> Nothing
                                                      Just (concr,_) -> Just concr
                                            lang -> mplus (Map.lookup lang                       langs)
                                                          (Map.lookup (abstractName pgf ++ lang) langs)
                           optType opts = let readOpt str = case readType str of
                                                              Just ty -> case checkType pgf ty of
                                                                           Left _   -> Nothing
                                                                           Right ty -> Just ty
                                                              Nothing -> Nothing
                                          in maybeStrOpts "cat" (Just (startCat pgf)) readOpt opts
                           (rprefix,rs) = break isSpace (reverse s0)
                           s            = reverse rs
                           prefix       = reverse rprefix
                       in case (optLang opts, optType opts) of
                            (Just lang,Just cat) -> let compls = [t | ParseOk res <- [complete lang cat s prefix], (t,_,_,_) <- res]
                                                    in ret (length prefix) (map Haskeline.simpleCompletion compls)
                            _                    -> ret 0 []
           _        -> ret 0 []
    CmplOpt (Just (Command n _ _)) pref
      -> case Map.lookup n (commands cmdEnv) of
           Just inf -> do let flg_compls = [Haskeline.Completion ('-':flg++"=") ('-':flg) False | (flg,_) <- flags   inf, isPrefixOf pref flg]
                              opt_compls = [Haskeline.Completion ('-':opt)      ('-':opt) True | (opt,_) <- options inf, isPrefixOf pref opt]
                          ret (length pref+1)
                              (flg_compls++opt_compls)
           Nothing  -> ret (length pref) []
    CmplIdent (Just (Command "i" _ _)) _        -- HACK: file name completion for command i
      -> Haskeline.completeFilename (left,right)
    CmplIdent _ pref
      -> case pgfenv gfenv of
           (_,Just pgf,_) -> ret (length pref) [Haskeline.simpleCompletion name | name <- functionsByPrefix pgf pref]
           _              -> ret (length pref) []
    _ -> ret 0 []
  where
    cmdEnv = commandenv gfenv

    loop ps []     = Just ps
    loop ps (t:ts) = case error "nextState ps (simpleParseInput t)" of
                       Left  es -> Nothing
                       Right ps -> loop ps ts

    ret len xs  = return (drop len left,xs)


data CompletionType
  = CmplCmd                   Ident
  | CmplStr   (Maybe Command) String
  | CmplOpt   (Maybe Command) Ident
  | CmplIdent (Maybe Command) Ident
  deriving Show

wc_type :: String -> CompletionType
wc_type = cmd_name
  where
    cmd_name cs =
      let cs1 = dropWhile isSpace cs
      in go cs1 cs1
      where
        go x []       = CmplCmd x
        go x (c:cs)
          | isIdent c = go x cs
          | otherwise = cmd x cs

    cmd x []       = ret CmplIdent x "" 0
    cmd _ ('|':cs) = cmd_name cs
    cmd _ (';':cs) = cmd_name cs
    cmd x ('"':cs) = str x cs cs
    cmd x ('-':cs) = option x cs cs
    cmd x (c  :cs)
      | isIdent c  = ident x (c:cs) cs
      | otherwise  = cmd x cs

    option x y []       = ret CmplOpt x y 1
    option x y ('=':cs) = optValue x y cs
    option x y (c  :cs)
      | isIdent c       = option x y cs
      | otherwise       = cmd x cs

    optValue x y ('"':cs) = str x y cs
    optValue x y cs       = cmd x cs

    ident x y []     = ret CmplIdent x y 0
    ident x y (c:cs)
      | isIdent c    = ident x y cs
      | otherwise    = cmd x cs

    str x y []          = ret CmplStr x y 1
    str x y ('\"':cs)   = cmd x cs
    str x y ('\\':c:cs) = str x y cs
    str x y (c:cs)      = str x y cs

    ret f x y d = f cmd y
      where
        x1 = take (length x - length y - d) x
        x2 = takeWhile (\c -> isIdent c || isSpace c || c == '-' || c == '=' || c == '"') x1

        cmd = case [x | (x,cs) <- RP.readP_to_S pCommand x2, all isSpace cs] of
                [x] -> Just x
                _   -> Nothing

    isIdent c = c == '_' || c == '\'' || isAlphaNum c
