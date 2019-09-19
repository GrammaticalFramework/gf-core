module GF.Compile.ExampleBased (
  parseExamplesInGrammar,
  configureExBased
  ) where

import PGF2
import Data.List

parseExamplesInGrammar :: ExConfiguration -> FilePath -> IO (FilePath,[String])
parseExamplesInGrammar conf file = do
  src <- readFile file                             -- .gfe
  let file' = take (length file - 3) file ++ "gf"  -- .gf
  ws <- convertFile conf src file'
  return (file',ws)

convertFile :: ExConfiguration -> String -> FilePath -> IO [String]
convertFile conf src file = do
  writeFile file "" -- "-- created by example-based grammar writing in GF\n"
  conv [] src
 where
  conv ws s = do
    (cex,end) <- findExample s
    if null end then return (nub (sort ws)) else do
      ws2 <- convEx cex
      conv (ws2 ++ ws) end
  findExample s = case s of
    '%':'e':'x':cs -> return $ getExample cs
    c:cs -> appf [c] >> findExample cs
    _ -> return (undefined,s)
  getExample s = 
    let 
      (cat,exend) = break (=='"') s
      (ex,   end) = break (=='"') (tail exend)
    in ((unwords (words cat),ex), tail end)  -- quotes ignored
  pgf = resource_pgf conf
  lang = language conf 
  convEx (cat,ex) = do
    appn "("
    let typ = maybe (error "no valid cat") id $ readType cat
    ws <- case parse lang typ ex of
      ParseFailed _ _ -> do
        appv ("WARNING: cannot parse example " ++ ex) 
        return []
      ParseIncomplete ->
        return []
      ParseOk ts ->
        case ts of
          (t:tt) -> do
            if null tt 
              then return ()
              else appv ("WARNING: ambiguous example " ++ ex) 
            appn (printExp conf (fst t))
            mapM_ (appn . ("  --- " ++) . printExp conf . fst) tt
            appn ")" 
            return [] 
    return ws
  appf = appendFile file
  appn s = appf s >> appf "\n"
  appv s = appn ("--- " ++ s) >> putStrLn s

data ExConfiguration = ExConf {
  resource_pgf :: PGF,
  verbose  :: Bool,
  language :: Concr,
  printExp :: Expr -> String
  }

configureExBased :: PGF -> Concr -> (Expr -> String) -> ExConfiguration
configureExBased pgf concr pr = ExConf pgf False concr pr

