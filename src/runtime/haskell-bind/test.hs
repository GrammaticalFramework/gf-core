import PGF2
import qualified Data.Map as M

main :: IO ()
main = do
  pgf <- readPGF "/Users/john/repositories/GF/contrib/foods/Foods.pgf"
  let
    Just concr = M.lookup "FoodsEng" (languages pgf)
    loop = do
      putStr "> "
      tks <- words <$> getLine
      let pr = complete concr (startCat pgf) (unwords (init tks)) (last tks) Nothing
      case pr of
        ParseOk x -> print x
        ParseFailed x s -> putStrLn $ "parse failed at " ++ show x ++ " " ++ s
        ParseIncomplete -> putStrLn "input incomplete"
      loop
  loop
