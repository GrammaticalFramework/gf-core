---
title: Grammatical Framework Download and Installation
date: 8 August 2025
---

**GF 3.12** was released on 8 August 2025.

What's new? See the [release notes](release-3.12.html).

#### Note: GF core and the RGL

The following instructions explain how to install **GF core**, i.e. the compiler, shell and run-time systems.
Obtaining the **Resource Grammar Library (RGL)** is done separately; see the section [at the bottom of this page](#installing-the-rgl-from-a-binary-release).

---

## Installing from a binary package

Binary packages are available for Debian/Ubuntu, macOS, and Windows and include:

- GF shell and grammar compiler
- `gf -server` mode
- C run-time system
- Python bindings to the C run-time system

[Binary packages on GitHub](https://github.com/GrammaticalFramework/gf-core/releases/tag/3.12)

#### Debian/Ubuntu

The package targets Ubuntu 24.04 (Noble).
To install it, use:

```
sudo apt install ./gf-3.12-ubuntu-24.04.deb 
```

#### macOS

To install the package, just double-click it and follow the installer instructions.

#### Windows

To install the package:

1. unpack it anywhere and take note of the full path to the folder containing the `.exe` file. 
2. add it to the `PATH` environment variable

For more information, see [Using GF on Windows](https://www.grammaticalframework.org/~inari/gf-windows.html) (latest updated for Windows 10).

## Installing from Hackage

_Instructions applicable for macOS, Linux, and WSL2 on Windows._

[GF is on Hackage](http://hackage.haskell.org/package/gf), so under
normal circumstances the procedure is fairly simple:

```
cabal update
cabal install gf-3.12
```

### Notes

#### GHC version

The GF source code is known to be compilable with GHC versions 7.10 through to 9.6.7.

#### Obtaining Haskell

There are various ways of obtaining Haskell, including:

- ghcup
    1. Install from https://www.haskell.org/ghcup/
    2. `ghcup install ghc 9.6.7`
    3. `ghcup set ghc 9.6.7`
- Stack: https://haskellstack.org/


#### Installation location

The above steps install GF for a single user.
The executables are put in `$HOME/.cabal/bin` (or on macOS in `$HOME/Library/Haskell/bin`),
so you might want to add this directory to your path (in `.bash_profile` or similar):

```
PATH=$HOME/.cabal/bin:$PATH
```

#### Haskeline

GF uses [`haskeline`](http://hackage.haskell.org/package/haskeline), which
on Linux depends on some non-Haskell libraries that won't be installed
automatically by Cabal, and therefore need to be installed manually.
Here is one way to do this:

- On Ubuntu: `sudo apt-get install libghc-haskeline-dev`
- On Fedora: `sudo dnf install ghc-haskeline-devel`

## Installing from source code

### Obtaining

To obtain the source code for the **release**,
download it from [GitHub](https://github.com/GrammaticalFramework/gf-core/releases).

Alternatively, to obtain the **latest version** of the source code:

1. If you haven't already, clone the repository with:
```
git clone https://github.com/GrammaticalFramework/gf-core.git
```
2. If you've already cloned the repository previously, update with:
```
git pull
```

### Installing

You can then install with:
```
cabal install
```

or, if you're a Stack user:

```
stack install
```

<!--The above notes for installing from source apply also in these cases.-->
For more info on working with the GF source code, see the
[GF Developers Guide](../doc/gf-developers.html).

## Installing the Python bindings from PyPI

The Python library is available on PyPI as `pgf`, so it can be installed using:

```
pip install pgf
```

If this doesn't work, you will need to install the C runtime manually; see the instructions [here](https://www.grammaticalframework.org/doc/gf-developers.html#toc12).

---

## Installing the RGL from a binary release

Binary releases of the RGL are made available on [GitHub](https://github.com/GrammaticalFramework/gf-rgl/releases).
In general the steps to follow are:

1. Download a binary release and extract it somewhere on your system.
2. Set the environment variable `GF_LIB_PATH` to point to wherever you extracted the RGL.

For more information, see [Using GF on Windows](https://www.grammaticalframework.org/~inari/gf-windows.html) (latest updated for Windows 10).

## Installing the RGL from source

To compile the RGL, you will need to have GF already installed and in your path.

1. Obtain the RGL source code, either by:
  - cloning with `git clone https://github.com/GrammaticalFramework/gf-rgl.git`
  - downloading a source archive [here](https://github.com/GrammaticalFramework/gf-rgl/archive/master.zip)
2. Run `make` in the source code folder.

For more options, see the [RGL README](https://github.com/GrammaticalFramework/gf-rgl/blob/master/README.md).

---

## Older releases

- [GF 3.11](index-3.11.html) (July 2021)
- [GF 3.10](index-3.10.html) (December 2018)
- [GF 3.9](index-3.9.html) (August 2017)
- [GF 3.8](index-3.8.html) (June 2016)
- [GF 3.7.1](index-3.7.1.html) (October 2015)
- [GF 3.7](index-3.7.html) (June 2015)
- [GF 3.6](index-3.6.html) (June 2014)
- [GF 3.5](index-3.5.html) (August 2013)
- [GF 3.4](index-3.4.html) (January 2013)
- [GF 3.3.3](index-3.3.3.html) (March 2012)
- [GF 3.3](index-3.3.html) (October 2011)
- [GF 3.2.9](index-3.2.9.html) source-only snapshot (September 2011)
- [GF 3.2](index-3.2.html) (December 2010)
- [GF 3.1.6](index-3.1.6.html) (April 2010)
