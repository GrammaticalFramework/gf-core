![GF Logo](https://www.grammaticalframework.org/doc/Logos/gf1.svg)

# Grammatical Framework (GF)

![Build majestic runtime](https://github.com/GrammaticalFramework/gf-core/actions/workflows/build-majestic.yml/badge.svg?branch=majestic)

The Grammatical Framework is a grammar formalism based on type theory.
It consists of:

- a special-purpose programming language
- a compiler of the language
- a generic grammar processor

The compiler reads GF grammars from user-provided files, and the
generic grammar processor performs various tasks with the grammars:

- generation
- parsing
- translation
- type checking
- computation
- paraphrasing
- random generation
- syntax editing

GF particularly addresses four aspects of grammars:

- multilinguality (parallel grammars for different languages)
- semantics (semantic conditions of well-formedness, semantic properties of expressions)
- grammar engineering (modularity, abstractions, libraries)
- embeddability in programs written in other languages (C, C++, Haskell, Java, JavaScript)

## Compilation and installation

1. First, you need to install the C Runtime. 
```Bash
cd src/runtime/c
```
Then follow the instructions in the [README.md](src/runtime/c/README.md) in that folder.

2. When the C runtime is installed, you should set up the Haskell runtime
```Bash
cd ../haskell
runghc Setup.hs configure 
runghc Setup.hs build
sudo runghc Setup.hs install
```
If the above commands fail because of missing dependencies, then you must install those first. Use something along the lines:
```Bash
cabal v1-install random --global
```
the same applies for all other dependecies needed here or bellow.

If you use macOS, you might run into problems with installation under ``/usr/lib``, and you should **first** specify the variable for the library path: 
```Bash
export DYLD_LIBRARY_PATH=/usr/local/lib
```
and then you run following commands:
```Bash
runghc Setup.hs configure --prefix=/usr/local
runghc Setup.hs build
sudo DYLD_LIBRARY_PATH=/usr/local/lib runghc Setup.hs install
```
3. Then you need to setup the compiler: 
```Bash
cd ../../compiler/           
runghc Setup.hs configure
runghc Setup.hs build
sudo DYLD_LIBRARY_PATH=/usr/local/lib runghc Setup.hs install
```

For more information, including links to precompiled binaries, see the [download page](https://www.grammaticalframework.org/download/index.html).

## About this repository

On 2018-07-25, the monolithic [GF repository](https://github.com/GrammaticalFramework/GF)
was split in two:

1. [gf-core](https://github.com/GrammaticalFramework/gf-core) —  the GF compiler, shell and runtimes
2. [gf-rgl](https://github.com/GrammaticalFramework/gf-rgl) — the resource grammar library

The former repository is now archived and no longer updated.
The split was performed using [this script](https://github.com/GrammaticalFramework/GF/blob/30ae1b5a5f73513ac5825ca6712186ef8afe9fd4/split/run.sh)
and the output of that script is [here](https://gist.github.com/johnjcamilleri/a6c43ff61f15a9657b457ac94ab7db61).
