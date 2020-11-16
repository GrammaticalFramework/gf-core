#! /bin/bash

### This script builds a binary distribution of GF from source.
### It assumes that you have Haskell and Cabal installed.
### Two binary package formats are supported (specified with the FMT env var):
### - plain tar files (.tar.gz)
### - OS X installer packages (.pkg)

os=$(uname)     # Operating system name (e.g. Darwin or Linux)
hw=$(uname -m)  # Hardware name (e.g. i686 or x86_64)

cabal="cabal v1-" # Cabal >= 2.4
# cabal="cabal " # Cabal <= 2.2

## Get GF version number from Cabal file
ver=$(grep -i ^version: gf.cabal | sed -e 's/version://' -e 's/ //g')

name="gf-$ver"
destdir="$PWD/dist/$name"          # assemble binary dist here
prefix=${PREFIX:-/usr/local}       # where to install
fmt=${FMT:-tar.gz}                 # binary package format (tar.gz or pkg)
ghc=${GHC:-ghc}                    # which Haskell compiler to use

extralib="$destdir$prefix/lib"
extrainclude="$destdir$prefix/include"
extra="--extra-lib-dirs=$extralib --extra-include-dirs=$extrainclude"

set -e                             # Stop if an error occurs
set -x                             # print commands before executing them

## First configure & build the C run-time system
pushd src/runtime/c
bash setup.sh configure --prefix="$prefix"
bash setup.sh build
bash setup.sh install prefix="$prefix" # hack required for GF build on macOS
bash setup.sh install prefix="$destdir$prefix"
popd

## Build the python binding to the C run-time system
if which >/dev/null python; then
    pushd src/runtime/python
    EXTRA_INCLUDE_DIRS="$extrainclude" EXTRA_LIB_DIRS="$extralib" python setup.py build
    python setup.py install --prefix="$destdir$prefix"
    if [ "$fmt" == pkg ] ; then
        # A hack for Python on OS X to find the PGF modules
        pyver=$(ls "$destdir$prefix/lib" | sed -n 's/^python//p')
        pydest="$destdir/Library/Python/$pyver/site-packages"
        mkdir -p "$pydest"
        ln "$destdir$prefix/lib/python$pyver/site-packages"/pgf* "$pydest"
    fi
    popd
else
    echo "Python is not installed, so the Python binding will not be included"
fi

## Build the Java binding to the C run-time system
if which >/dev/null javac && which >/dev/null jar ; then
    pushd src/runtime/java
    rm -f libjpgf.la # In case it contains the wrong INSTALL_PATH
    if make CFLAGS="-I$extrainclude -L$extralib" INSTALL_PATH="$prefix"
    then
        make INSTALL_PATH="$destdir$prefix" install
    else
        echo "Skipping the Java binding because of errors"
    fi
    popd
else
    echo "Java SDK is not installed, so the Java binding will not be included"
fi

## To find dynamic C run-time libraries when building GF below
export DYLD_LIBRARY_PATH="$extralib" LD_LIBRARY_PATH="$extralib"

## Build GF, with C run-time support enabled
${cabal}install -w "$ghc" --only-dependencies -fserver -fc-runtime $extra
${cabal}configure -w "$ghc" --prefix="$prefix" -fserver -fc-runtime $extra
${cabal}build

## Copy GF to $destdir
${cabal}copy --destdir="$destdir"
libdir=$(dirname $(find "$destdir" -name PGF.hi))
${cabal}register --gen-pkg-config="$libdir/gf-$ver.conf"

## Create the binary distribution package
case $fmt in
    tar.gz)
        targz="$name-bin-$hw-$os.tar.gz"     # the final tar file
        tar --directory "$destdir/$prefix" --gzip --create --file "dist/$targz" .
        echo "Created $targz"
        ;;
    pkg)
        pkg=$name.pkg
        pkgbuild --identifier org.grammaticalframework.gf.pkg --version "$ver" --root "$destdir" --install-location / dist/$pkg
        echo "Created $pkg"
esac

## Cleanup
rm -r "$destdir"
