import PGF2
import Data.Char (toUpper)
import GF.Scribe
import GF.Infra.Ident
import GF.Infra.CheckM
import GF.Infra.Option
import GF.Grammar.Grammar
import GF.Grammar.Parser
import GF.Grammar.Printer
import GF.Grammar.Macros
import qualified GF.Data.Operations as O
import GF.Compile.Rename
import GF.Compile.Compute.Concrete
import GF.Compile
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS
import System.Environment(getArgs)
import System.Console.Haskeline
import Network.HTTP
import Control.Monad
import Control.Monad.IO.Class
import Text.JSON
import Text.PrettyPrint

main = do
  (qid:lang:args) <- getArgs
  gr <- readNGF "/usr/local/share/x86_64-linux-ghc-8.8.4/gf-4.0.0/www/robust/Parse.ngf"
  let Just cnc = Map.lookup (toCnc lang) (languages gr)
  rsp <- simpleHTTP (getRequest ("https://www.wikidata.org/wiki/Special:EntityData/"++qid++".json"))
  case decode (rspBody rsp) >>= valFromObj "entities" >>= valFromObj qid >>= valFromObj "claims" of
    Ok obj    -> do do (_,(mo,sgr)) <- batchCompile noOptions ["RGL.gf"]
                       interactive (item2term obj) () mo sgr gr cnc qid
    Error msg -> fail msg
  where
    toCnc (c:cs) = "Parse"++(toUpper c:cs)
    toCnc s      = s

item2term obj = 
  R [assign (LIdent (rawIdentS prop))
            (mkFV [term | value <- values,
                          Ok term <- [claim2term value]])
                | (prop,values) <- fromJSObject obj]
  where
    mkFV [t] = t
    mkFV ts  = FV ts

    claim2term value = do
      t1 <- valFromObj "mainsnak"    value >>= snak2term
      t2 <- (valFromObj "qualifiers" value >>= mods2term)
            `mplus`
            return []
      t3 <- (valFromObj "references" value >>= mods2term)
            `mplus`
            return []
      return (R (t1++t2++t3))

    mods2term obj =
      return [assign (LIdent (rawIdentS prop))
                     (mkFV [R term | value <- values,
                                   Ok term <- [snak2term value]])
                         | (prop,values) <- fromJSObject obj]

    snak2term value = valFromObj "datavalue" value >>= datavalue2term

    datavalue2term dv =
      do s <- valFromObj "value" dv                -- string
         return [assign (LIdent (rawIdentS "s")) (K s)]
      `mplus`
      do value <- valFromObj "value" dv            -- wikibase-entityid
         id <- valFromObj "id" value
         return [assign (LIdent (rawIdentS "id")) (K id)]
      `mplus`
      do value     <- valFromObj "value" dv        -- globecoordinate
         latitude  <- valFromObj "latitude" value
         longitude <- valFromObj "longitude" value
         precision <- valFromObj "precision" value
         return [assign (LIdent (rawIdentS "latitude")) (EFloat latitude)
                ,assign (LIdent (rawIdentS "longitude")) (EFloat longitude)
                ,assign (LIdent (rawIdentS "longitude")) (EFloat precision)]
      `mplus`
      do value <- valFromObj "value" dv            -- quantity
         amount <- valFromObj "amount" value >>= decimal
         unit <- fmap dropURL (valFromObj "unit" value)
         return [assign (LIdent (rawIdentS "amount")) amount
                ,assign (LIdent (rawIdentS "unit")) (K unit)]
      `mplus`
      do value <- valFromObj "value" dv            -- time
         time  <- valFromObj "time" value
         model <- fmap dropURL (valFromObj "calendarmodel" value)
         precision <- valFromObj "precision" value
         return [assign (LIdent (rawIdentS "time")) (K time)
                ,assign (LIdent (rawIdentS "calendarmodel")) (K model)
                ,assign (LIdent (rawIdentS "precision")) (EInt precision)]
      `mplus`
      do value    <- valFromObj "value" dv
         text     <- valFromObj "text" value
         language <- valFromObj "language" value
         return [assign (LIdent (rawIdentS "text")) (K text)
                ,assign (LIdent (rawIdentS "language")) (K language)]
      `mplus`
      Error "Cannot parse a datavalue"

    dropURL s = match "http://www.wikidata.org/entity/" s
      where
        match []     ys     = ys
        match (x:xs) (y:ys)
         | x == y           = match xs ys
        match _      _      = s

    decimal ('+':s) = decimal s
    decimal s       =
      case reads s of
        [(n,"")] -> return (EInt n)
        _        -> case reads s of
                      [(d,"")] -> return (EFloat d)
                      _        -> Error "Not a decimal"

interactive entity db mo sgr gr cnc qid = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine (qid++"> ")
      case minput of
        Nothing    -> return ()
        Just input -> case runP pTerm (BS.pack input) of
                        Right term     -> do case runCheck (checkComputeTerm term) of
                                               O.Ok (terms,msg) -> do outputStr msg
                                                                      mapM_ (\term -> outputStrLn (render (ppTerm Unqualified 0 term))) terms
                                               O.Bad msg       -> do outputStrLn msg
                                             loop
                        Left (pos,msg) -> do outputStrLn (show pos ++ msg)
                                             loop
                                             
    checkComputeTerm term = do
      term <- renameSourceTerm sgr mo term
      runEvalM sgr $ do
        tnk <- newThunk [] entity
        val <- eval [(identS "entity",tnk)] term []
        value2term [] val
