![GF Logo](doc/Logos/gf1.svg)

# Grammatical Framework (GF)

[![Build Status](https://travis-ci.org/GrammaticalFramework/gf-core.svg?branch=master)](https://travis-ci.org/GrammaticalFramework/gf-core)

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

The simplest way of installing GF is with the command:
```
cabal install
```

For more details, see the [download page](http://www.grammaticalframework.org/download/index.html)
and [developers manual](http://www.grammaticalframework.org/doc/gf-developers.html).

## About this repository

On 2018-07-25, the monolithic [GF repository](https://github.com/GrammaticalFramework/GF)
was split in two:

1. [gf-core](https://github.com/GrammaticalFramework/gf-core) —  the GF compiler, shell and runtimes
2. [gf-rgl](https://github.com/GrammaticalFramework/gf-rgl) — the resource grammar library

The former repository is now archived and no longer updated.
The split was performed using [this script](https://github.com/GrammaticalFramework/GF/blob/30ae1b5a5f73513ac5825ca6712186ef8afe9fd4/split/run.sh)
and the output of that script is [here](https://gist.github.com/johnjcamilleri/a6c43ff61f15a9657b457ac94ab7db61).
