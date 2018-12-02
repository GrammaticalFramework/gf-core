---
title: Grammatical Framework Download and Installation
...

**GF 3.10** was released on 2 December 2018.
It is the first version of GF which _does not include the RGL_.

What's new? See the [release notes](release-3.10.html).

## Binary packages

| Platform        | Download                                           | Features       | How to install                     |
|:----------------|:---------------------------------------------------|:---------------|:-----------------------------------|
| macOS           | [gf-3.10.pkg](gf-3.10.pkg)                         | GF, S, C, J, P | Double-click on the package icon   |
| Ubuntu (64-bit) | [gf\_3.10-1\_amd64.deb](gf_3.10-1_amd64.deb)       | GF, S, C, J, P | `sudo dpkg -i gf_3.10-1_amd64.deb` |
| Windows         | [gf-3.10-bin-windows.zip](gf-3.10-bin-windows.zip) | GF, S          | `unzip gf-3.10-bin-windows.zip`    |

<!--
| macOS           | [gf-3.10-bin-intel-mac.tar.gz](gf-3.10-bin-intel-mac.tar.gz) | GF,S,C,J,P | `sudo tar -C /usr/local -zxf gf-3.10-bin-intel-mac.tar.gz` |
| Raspian 9.1     | [gf\_3.10-1\_armhf.deb](gf_3.10-1_armhf.deb)                 | GF,S,C,J,P | `sudo dpkg -i gf_3.10-1_armhf.deb`                         |
| Ubuntu (32-bit) | [gf\_3.10-1\_i386.deb](gf_3.10-1_i386.deb)                   | GF,S,C,J,P | `sudo dpkg -i gf_3.10-1_i386.deb`                          |
-->

**Features**

- GF = GF shell and grammar compiler
- S = `gf -server` mode
- C = C run-time system
- J/P = Java/Python binding to the C run-time system

### Notes

The Windows package is installed by just unpacking it anywhere. You will
probably need to set the `PATH` and `GF_LIB_PATH` environment variables,
see Inari's notes on [Installing GF on Windows](http://www.grammaticalframework.org/~inari/gf-windows.html#toc3).

The Ubuntu `.deb` packages should work on Ubuntu 16.04 and 17.04 and
similar Linux distributions.

<!-- The Raspian `.deb` package was created on a Raspberry Pi 3 and will
probably work on other ARM-based systems running Debian 9 (stretch) or
similar Linux distributions. -->

The packages for macOS (Mac OS X) should work on at least 10.11 and
10.12 (El Capitan and Sierra).

<!-- The Mac OS and Linux `.tar.gz` packages are designed to be installed in
`/usr/local`. You can install them in other locations, but then you need
to set the `GF_LIB_PATH` environment variable:

```
export GF_LIB_PATH=/usr/local/share/gf-3.10/lib
```

where `/usr/local` should be replaced with the path to the location
where you unpacked the package. -->

## Installing the latest release from source

[GF is on Hackage](http://hackage.haskell.org/package/gf), so under
normal circumstances the procedure is fairly simple:

1.  Install a recent version of the [Haskell
    Platform](http://hackage.haskell.org/platform) (see note below)
2.  `cabal update`
3.  On Linux: install some C libraries from your Linux distribution (see note below)
4.  `cabal install gf`

Note that this installs GF _without_ the RGL.

You can also download full source packages from GitHub at the following links:

- [GF releases](https://github.com/GrammaticalFramework/gf-core/releases)
- [RGL releases](https://github.com/GrammaticalFramework/gf-rgl/releases)

### Notes

**Installation location**

The above steps installs GF for a single user. The executables are put
in `$HOME/.cabal/bin` (or, with recent versions of the Haskell platform
on Mac OS X, in `$HOME/Library/Haskell/bin`), so it is a good idea to
put a line in your `.bash_profile` or `.profile` to add that directory
to you path:

```
PATH=$HOME/.cabal/bin:$PATH
```

or

```
PATH=$HOME/Library/Haskell/bin:$PATH
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
- On Fedora: `sudo yum install ghc-haskeline-devel`

**GHC version**

The GF source code has been updated to compile with GHC 8.4.
Using older versions of GHC (e.g. 8.2, 8.0 and 7.10) should still work too.

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

## Installing the RGL from source

To install the RGL from source,
you can download a release from [GitHub](https://github.com/GrammaticalFramework/gf-rgl/releases)
or get the latest version by cloning the repository:

```
git clone https://github.com/GrammaticalFramework/gf-rgl.git
```

In both cases, once you have the RGL sources you can install them by running:

```
make
```

in the RGL folder.
For more details about building the RGL, see the [RGL README](https://github.com/GrammaticalFramework/gf-rgl/blob/master/README.md).

## Older releases

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
