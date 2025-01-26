import GHC.IO.Encoding (setLocaleEncoding, utf8)

import System.Environment (getArgs)
import GF.Compile.Repl (getReplOpts, runRepl)

main :: IO ()
main = do
  setLocaleEncoding utf8
  args <- getArgs
  case getReplOpts args of
    Left errs  -> mapM_ print errs
    Right opts -> runRepl opts
