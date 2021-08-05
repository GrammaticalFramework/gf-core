import Test.HUnit
import PGF2

main = do
  gr <- readPGF "tests/basic.pgf"
  runTestTTAndExit (TestCase (assertEqual "abstract names" "basic" (abstractName gr)))
