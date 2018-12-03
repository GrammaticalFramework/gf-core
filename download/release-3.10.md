---
title: GF 3.10 Release Notes
date: 2 December 2018
...

## Installation

See the [download page](index.html).

## What's new

In this release, the GF "core" (compiler and runtimes) and RGL have been split into separate repositories.
The binary packages on the downloads page contain both GF and the RGL, but the sources are now separate:
[gf-core](https://github.com/GrammaticalFramework/gf-core) and
[gf-rgl](https://github.com/GrammaticalFramework/gf-rgl).

Over 300 changes have been pushed to GF and over 600 changes have been made to the RGL
since the release of GF 3.9 in August 2017.

## General

- Travis integration for both [GF](https://travis-ci.org/GrammaticalFramework/gf-gf) and [RGL](https://travis-ci.org/GrammaticalFramework/gf-rgl).
- A lot of bug fixes and repository cleanup, including things moved to new repositories:
  - [Phrasebook](https://github.com/GrammaticalFramework/gf-contrib/tree/master/phrasebook)
  - [Wide coverge translator](https://github.com/GrammaticalFramework/wide-coverage)
  - [Mobile apps](https://github.com/GrammaticalFramework/gf-offline-translator)
  - [gftest](https://github.com/GrammaticalFramework/gftest)
  - [gf-mode](https://github.com/GrammaticalFramework/gf-emacs-mode) for Emacs
- A fresh look for the GF website.

## GF compiler and run-time library

- Extensive improvements in the C runtime and bindings to it from Python, Java, Haskell, C#
- A GF shell which uses the C runtime
- Better error messages
- GF now has a Stack configuration file
- The compiler source code has been updated for compatibility with GHC 8.4.3.
- `GF_LIB_PATH` can now be `path1:path2:path3`, not just `path1`
- Add TypeScript type definitions for `gflib.js`
- New compiler/shell options
  - added option `-output-format=java` for producing code for embedded grammars in Java
  - `rf -paragraphs`
  - `linearize -tabtreebank`
  - A new function called `completions` is added in the Haskell runtime and used in PGFService. This makes the extraction of completions more platform independent

## Resource Grammar Library

- [Bash build script](https://github.com/GrammaticalFramework/gf-rgl/blob/master/Setup.sh), for building the RGL without Haskell
- [Windows build script](https://github.com/GrammaticalFramework/gf-rgl/blob/master/Setup.bat), for building the RGL without Haskell on a regular Windows command shell
- New languages:
  - Basque
- Extend and Extra
- Various fixes for several languages.
- Various fixes in the translation dictionaries.

## Apps and Cloud services

- Sort list of public grammars by age by default
- Browser compatibility fixes
- Allow public grammars to be deleted in more cases
- Show grammar comments in the list of public grammars
