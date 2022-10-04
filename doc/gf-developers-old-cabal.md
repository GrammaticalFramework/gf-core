
This page contains the old installation instructions from the [Developer's Guide](../doc/gf-developers.html).
We recommend Stack as a primary installation method, because it's easier for a Haskell beginner, and we want to keep the main instructions short.
But if you are an experienced Haskeller and want to keep using Cabal, here are the old instructions using `cabal install`.

Note that some of these instructions may be outdated. Other parts may still be useful.

## Compilation from source with Cabal 

The build system of GF is based on *Cabal*, which is part of the
Haskell Platform, so no extra steps are needed to install it. In the simplest
case, all you need to do to compile and install GF, after downloading the
source code as described above, is

    $ cabal install

This will automatically download any additional Haskell libraries needed to
build GF. If this is the first time you use Cabal, you might need to run
`cabal update` first, to update the list of available libraries.

If you want more control, the process can also be split up into the usual
*configure*, *build* and *install* steps.

### Configure 

During the configuration phase Cabal will check that you have all
necessary tools and libraries needed for GF. The configuration is
started by the command:

    $ cabal configure

If you don't see any error message from the above command then you
have everything that is needed for GF. You can also add the option
`-v` to see more details about the configuration.

You can use `cabal configure --help` to get a list of configuration options.

### Build 

The build phase does two things. First it builds the GF compiler from
the Haskell source code and after that it builds the GF Resource Grammar
Library using the already build compiler.  The simplest command is:

    $ cabal build

Again you can add the option `-v` if you want to see more details.

#### Parallel builds 

If you have Cabal>=1.20 you can enable parallel compilation by using

    $ cabal build -j

or by putting a line

    jobs: $ncpus

in your `.cabal/config` file. Cabal
will pass this option to GHC when building the GF compiler, if you
have GHC>=7.8.

Cabal also passes `-j` to GF to enable parallel compilation of the
Resource Grammar Library. This is done unconditionally to avoid
causing problems for developers with Cabal<1.20. You can disable this
by editing the last few lines in `WebSetup.hs`.

### Install 

After you have compiled GF you need to install the executable and libraries
to make the system usable.

    $ cabal copy
    $ cabal register

This command installs the GF compiler for a single user, in the standard
place used by Cabal.
On Linux and Mac this could be `$HOME/.cabal/bin`.
On Mac it could also be `$HOME/Library/Haskell/bin`.
On Windows this is `C:\Program Files\Haskell\bin`.

The compiled GF Resource Grammar Library will be installed
under the same prefix, e.g. in
`$HOME/.cabal/share/gf-3.3.3/lib` on Linux and
in `C:\Program Files\Haskell\gf-3.3.3\lib` on Windows.

If you want to install in some other place then use the `--prefix`
option during the configuration phase.

### Clean 

Sometimes you want to clean up the compilation and start again from clean
sources. Use the clean command for this purpose:

    $ cabal clean

### Known problems with Cabal 

Some versions of Cabal (at least version 1.16) seem to have a bug that can
cause the following error:

    Configuring gf-3.x...
    setup: Distribution/Simple/PackageIndex.hs:124:8-13: Assertion failed

The exact cause of this problem is unclear, but it seems to happen
during the configure phase if the same version of GF is already installed,
so a workaround is to remove the existing installation with

    ghc-pkg unregister gf

You can check with `ghc-pkg list gf` that it is gone.

## Compilation with make 

If you feel more comfortable with Makefiles then there is a thin Makefile
wrapper arround Cabal for you. If you just type:

    $ make

the configuration phase will be run automatically if needed and after that
the sources will be compiled.

For installation use:

    $ make install

For cleaning:

    $ make clean

## Partial builds of RGL 

**NOTE**: The following doesn't work with recent versions of `cabal`. *(This comment was left in 2015, so make your own conclusions.)*

The resource grammar library can be compiled in two modes: with present
tense only and with all tenses. By default it is compiled with all
tenses. If you want to use the library with only present tense you can
compile it in this special mode with the command:

    $ cabal build present

You could also control which languages you want to be recompiled by
adding the option `langs=list`. For example the following command
will compile only the English and the Swedish language:

    $ cabal build langs=Eng,Swe
