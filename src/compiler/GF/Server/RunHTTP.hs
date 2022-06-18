module GF.Server.RunHTTP(runHTTP,Options(..),cgiHandler) where

import GF.Server.CGI(ContentType(..),
                     CGIResult(..),CGIRequest(..),Input(..),
                     Headers,HeaderName(..),
                     runCGIT)
import GF.Server.URLEncoding(decodeQuery)
import Network.URI(uriPath,uriQuery)
import Network.Shed.Httpd(initServer,Request(..),Response(..))
import qualified Data.ByteString.Lazy.Char8 as BS(pack,unpack,empty)
import qualified Data.Map as M(fromList)

data Options = Options { documentRoot :: String, port :: Int } deriving Show

runHTTP (Options root port) = initServer port . cgiHandler root

cgiHandler root h = fmap httpResp . runCGIT h . cgiReq root

httpResp :: (Headers,CGIResult) -> Response
httpResp (hdrs,r) = Response code (map name hdrs) (body r)
  where
    code = maybe 200 (read.head.words) (lookup (HeaderName "Status") hdrs)
    body CGINothing = ""
    body (CGIOutput s) = BS.unpack s

    name (HeaderName n,v) = (n,v)

cgiReq :: String -> Request -> CGIRequest
cgiReq root (Request method uri hdrs body)
  | method == "POST" = CGIRequest vars (map input (decodeQuery body)) BS.empty
  | otherwise        = CGIRequest vars (map input (decodeQuery qs  )) BS.empty -- assumes method=="GET"
  where
    vars = M.fromList [("REQUEST_METHOD",method),
                       ("REQUEST_URI",show uri),
                       ("SCRIPT_FILENAME",root++uriPath uri),
                       ("QUERY_STRING",qs),
                       ("HTTP_ACCEPT_LANGUAGE",al)]
    qs = case uriQuery uri of
           '?':'&':s -> s -- httpd-shed bug workaround
           '?':s -> s
           s -> s
    al = maybe "" id $ lookup "Accept-Language" hdrs

    input (name,val) = (name,Input (BS.pack val) Nothing plaintext)
    plaintext = ContentType "text" "plain" []
