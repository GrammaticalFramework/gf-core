.PHONY: all build install doc clean html deb pkg bintar sdist

# This gets the numeric part of the version from the cabal file
VERSION=$(shell sed -ne "s/^version: *\([0-9.]*\).*/\1/p" gf.cabal)

# Check if stack is installed
STACK=$(shell if hash stack 2>/dev/null; then echo "1"; else echo "0"; fi)

ifeq ($(STACK),1)
  CMD=stack
else
  CMD=cabal
  CMD_OPT="--force-reinstalls"
endif

all: src/runtime/c/libpgf.la
	${CMD} install gf

src/runtime/c/libpgf.la: src/runtime/c/Makefile
	(cd src/runtime/c; make; sudo make install)

src/runtime/c/Makefile: src/runtime/c/Makefile.in src/runtime/c/configure
	(cd src/runtime/c; ./configure)

src/runtime/c/Makefile.in src/runtime/c/configure: src/runtime/c/configure.ac src/runtime/c/Makefile.am
	(cd src/runtime/c; autoreconf -i)

doc:
	${CMD} haddock

clean:
	${CMD} clean
	bash bin/clean_html

html::
	bash bin/update_html

# Make a debian package. First add a suitable entry with the correct GF version
# number to the top of debian/changelog.
# (Tested on Ubuntu 15.04. You need to install dpkg-dev & debhelper.)
deb:
	dpkg-buildpackage -b -uc

# Make a macOS installer package
pkg:
	FMT=pkg bash bin/build-binary-dist.sh

# Make a binary tar distribution
bintar:
	bash bin/build-binary-dist.sh

#sdist:
#	cabal sdist

# Make a source tar.gz distribution using git to make sure that everything is included.
# We put the distribution in dist/ so it is removed on `make clean`
# sdist:
# 	test -d dist || mkdir dist
# 	git archive --format=tar.gz --output=dist/gf-${VERSION}.tar.gz HEAD
