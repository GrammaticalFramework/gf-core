# PGF2

This is a Haskell binding to the PGF runtime written in C.

The exposed modules are:

- `PGF2`: a user API similar to Python and Java APIs
- `PGF2.Transactions`: an interface for modifying an existing grammar at runtime

## How to compile

**Important:** You must have the C runtime already installed and available on your system.
See <https://github.com/GrammaticalFramework/gf-core/blob/master/src/runtime/c/INSTALL>

Once the runtime is installed, you can install this binding with Cabal:

```
cabal install pgf2 --extra-lib-dirs=/usr/local/lib
```

The option extra-lib-dirs is needed if you have installed the C runtime
in the default folder /usr/local/lib on Linux. If you instead have
configured the C runtime with:

```
./configure --prefix=/usr
```

on Linux, or as:

```
./configure --prefix=<your GHC root directory>/mingw
```

on Windows, then you don't need the extra-lib-dirs option here.

You can also add the binding to your `stack.yaml` file:

```yaml
extra-deps:
  - pgf2
extra-lib-dirs:
  - /usr/local/lib
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
