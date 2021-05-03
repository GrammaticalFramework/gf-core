import PGF2
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map as M

main :: IO ()
main = do
  pgf <- readPGF "/Users/john/repositories/GF/contrib/foods/Foods.pgf"
  let
    Just concr = M.lookup "FoodsEng" (languages pgf)
    loop = do
      putStr "> "
      input <- getLine
      let
        (sent,pfx) =
          if C.isSpace (last input)
          then (input, "")
          else let toks = words input in (unwords (init toks), last toks)
      let pr = complete concr (startCat pgf) sent pfx Nothing
      case pr of
        ParseOk x -> print x
        ParseFailed x s -> putStrLn $ "parse failed at " ++ show x ++ " " ++ s
        ParseIncomplete -> putStrLn "input incomplete"
      loop
  loop
