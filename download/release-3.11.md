---
title: GF 3.11 Release Notes
date: ... December 2020
...

## Installation

See the [download page](index-3.11.html).

## What's new

From this release, the binary GF core packages do not contain the RGL.
The RGL's release cycle is now completely separate from GF's. See [RGL releases](https://github.com/GrammaticalFramework/gf-rgl/releases).

Over 400 changes have been pushed to GF core
since the release of GF 3.10 in December 2018.

## General

- Make the test suite work again.
- Compatibility with new versions of GHC, including multiple Stack files for the different versions.
- Updates to build scripts and CI.
- Bug fixes.

## GF compiler and run-time library

- Huge improvements in time & space requirements for grammar compilation.
- Add CoNLL output to `visualize_tree` shell command.
- Add canonical GF as output format in the compiler.
- Add PGF JSON as output format in the compiler.
- Deprecate JavaScript runtime in favour of updated [TypeScript runtime](https://github.com/GrammaticalFramework/gf-typescript).
- Improvements to Haskell export.
- Improvements to the C runtime.
- Improvements to `gf -server` mode.
- Clearer compiler error messages.

## Other

- Web page and documentation improvements.
- Add WordNet module to GFSE.
