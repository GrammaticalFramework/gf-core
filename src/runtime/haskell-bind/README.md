# PGF2

This is a Haskell binding to the PGF runtime written in C.

The exposed modules are:

- `PGF2`: a user API similar to Python and Java APIs
- `PGF2.Internal`: an internal module with FFI definitions for the relevant C functions

## How to compile

**Note:** you must have the PGF C runtime already installed and available.
See <https://github.com/GrammaticalFramework/gf-core/blob/master/src/runtime/c/INSTALL>

Once the runtine is installed, you can use:

```
cabal install pgf2
```

## How to use

Simply import `PGF2` in your Haskell program.
The Cabal infrastructure will make sure to tell the compiler where to find the relevant modules.

## Example

```haskell
module Main where

import PGF2
import qualified Data.Map as Map

main = do
  pgf <- readPGF "App12.pgf"
  let Just eng = Map.lookup "AppEng" (languages pgf)
  
  -- Parsing
  let res = parse eng (startCat pgf) "this is a small theatre"
  let ParseOk ((tree,prob):rest) = res
  print tree
  
  -- Linearisation
  let Just expr = readExpr "AdjCN (PositA red_A) (UseN theatre_N)"
  let s = linearize eng expr
  print s
```
