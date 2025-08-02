---
title: GF 3.12 Release Notes
date: 03 August 2025
---

## Installation

See the [download page](index-3.12.html).

## What's new
This release adds support for Apple Silicon M1 Mac computers and newer versions of GHC, along with various improvements and bug fixes.

Over 70 commits have been merged to gf-core since the release of GF 3.11 in July 2021.

## General
- Support for ARM, allowing to run GF on Mac computers with Apple Silicon M1
- Support for newer versions of GHC (8.10.7, 9.0.2, 9.2.4, 9.4, 9.6.7)
- Support compiling with Nix
- Better error messages
- Improvements to several GF shell commands
- Several bug fixes and performance improvements

## GF compiler and run-time library
- Syntactic sugar for table update: `table {cases ; vvv => t \! vvv}.t` can now be written as `t ** { cases }`  
- Adjust the `-view` command depending on the OS
- Improve output of the `visualize_dependencies` (`vd`) command for large dependency trees
- Reintroduce syntactic transfer with `pt -transfer` and fix a bug in `pt -compute`
- Bug fix: apply `gt` to all arguments when piped
- Fix many "Invalid character" messages by always encoding GF files in UTF-8
- Improve performance with long extend-lists
- Improve syntax error messages
- Add support for BIND tokens in the Python bindings
- Allow compilation with emscripten

## Other
- Add support for Visual Studio Code