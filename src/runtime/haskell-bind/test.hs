import PGF2
import qualified Data.Map as M

main :: IO ()
main = do
  pgf <- readPGF "/Users/john/repositories/GF/contrib/foods/Foods.pgf"
  let Just concr = M.lookup "FoodsEng" (languages pgf)
  let pr = complete concr (startCat pgf) "" "th" Nothing
  case pr of
    ParseOk x -> print x
    ParseFailed _ _ -> putStrLn "parse failed"
    ParseIncomplete -> putStrLn "input incomplete"
