---
title: Grammatical Framework Download and Installation
...

**GF 3.11** was released on ... December 2020.

What's new? See the [release notes](release-3.11.html).

#### Note: GF core and the RGL

The following instructions explain how to install **GF core**, i.e. the compiler, shell and run-time systems.
Obtaining the **Resource Grammar Library (RGL)** is done separately; see the section at the bottom of this page.

---

## Installing from a binary package

Binary packages are available for Debian/Ubuntu, macOS, and Windows and include:

- GF shell and grammar compiler
- `gf -server` mode
- C run-time system
- Java & Python bindings to the C run-time system

Unlike in previous versions, the binaries **do not** include the RGL.

[Binary packages on GitHub](https://github.com/GrammaticalFramework/gf-core/releases/tag/RELEASE-3.11)

#### Debian/Ubuntu

To install the package use:
```
sudo dpkg -i gf_3.11.deb
```

The Ubuntu `.deb` packages should work on Ubuntu 16.04, 18.04 and similar Linux distributions.

#### macOS

To install the package, just double-click it and follow the installer instructions.

The packages should work on at least 10.13 (High Sierra) and 10.14 (Mojave).

#### Windows

To install the package, unpack it anywhere.

You will probably need to update the `PATH` environment variable to include your chosen install location.

For more information, see [Using GF on Windows](https://www.grammaticalframework.org/~inari/gf-windows.html) (latest updated for Windows 10).

## Installing the latest release from source

[GF is on Hackage](http://hackage.haskell.org/package/gf), so under
normal circumstances the procedure is fairly simple:

1.  Install a recent version of the [Haskell Platform](http://hackage.haskell.org/platform) (see note below)
2.  `cabal update`
3.  On Linux: install some C libraries from your Linux distribution (see note below)
4.  `cabal install gf`

You can also download the source code release from [GitHub](https://github.com/GrammaticalFramework/gf-core/releases),
and follow the instructions below under **Installing from the latest developer source code**.

### Notes

**Installation location**

The above steps installs GF for a single user.
The executables are put in `$HOME/.cabal/bin` (or on macOS in `$HOME/Library/Haskell/bin`),
so you might want to add this directory to your path (in `.bash_profile` or similar):

```
PATH=$HOME/.cabal/bin:$PATH
```

**Build tools**

In order to compile GF you need the build tools **Alex** and **Happy**.
These can be installed via Cabal, e.g.:

```
cabal install alex happy
```

or obtained by other means, depending on your OS.

**Haskeline**

GF uses [`haskeline`](http://hackage.haskell.org/package/haskeline), which
on Linux depends on some non-Haskell libraries that won't be installed
automatically by cabal, and therefore need to be installed manually.
Here is one way to do this:

- On Ubuntu: `sudo apt-get install libghc-haskeline-dev`
- On Fedora: `sudo dnf install ghc-haskeline-devel`

**GHC version**

The GF source code has been updated to compile with GHC versions 7.10 through to 8.8.

## Installing from the latest developer source code

If you haven't already, clone the repository with:

```
git clone https://github.com/GrammaticalFramework/gf-core.git
```

If you've already cloned the repository previously, update with:

```
git pull
```

Then install with:

```
cabal install
```

or, if you're a Stack user:

```
stack install
```

The above notes for installing from source apply also in these cases.
For more info on working with the GF source code, see the
[GF Developers Guide](../doc/gf-developers.html).

## Installing the Python bindings from PyPI

The Python library is available on PyPI as `pgf`, so it can be installed using:

```
pip install pgf
```

We provide binary wheels for Linux and macOS, which include the C runtime and are ready-to-go.
If there is no binary distribution for your platform, this will install the source tarball,
which will attempt to build the binding during installation,
and requires the GF C runtime to be installed on your system.

---

## Installing the RGL from a binary release

Binary releases of the RGL are made available on [GitHub](https://github.com/GrammaticalFramework/gf-rgl/releases).
In general the steps to follow are:

1. Download a binary release and extract it somewhere on your system.
2. Set the environment variable `GF_LIB_PATH` to point to wherever you extracted the RGL.

## Installing the RGL from source

To compile the RGL, you will need to have GF already installed and in your path.

1. Obtain the RGL source code, either by:
  - cloning with `git clone https://github.com/GrammaticalFramework/gf-rgl.git`
  - downloading a source archive [here](https://github.com/GrammaticalFramework/gf-rgl/archive/master.zip)
2. Run `make` in the source code folder.

For more options, see the [RGL README](https://github.com/GrammaticalFramework/gf-rgl/blob/master/README.md).

---

## Older releases

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
