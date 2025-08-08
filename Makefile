.PHONY: all build install doc clean html deb pkg bintar sdist

# This gets the numeric part of the version from the cabal file
VERSION=$(shell sed -ne "s/^version: *\([0-9.]*\).*/\1/p" gf.cabal)

# Check if stack is installed
STACK=$(shell if hash stack 2>/dev/null; then echo "1"; else echo "0"; fi)

# Check if cabal >= 2.4 is installed (with v1- and v2- commands)
CABAL_NEW=$(shell if cabal v1-repl --help >/dev/null 2>&1 ; then echo "1"; else echo "0"; fi)

ifeq ($(STACK),1)
  CMD=stack
else
  CMD=cabal
  ifeq ($(CABAL_NEW),1)
    CMD_PFX=v1-
  endif
endif

all: build

dist/setup-config: gf.cabal Setup.hs WebSetup.hs
ifneq ($(STACK),1)
	cabal ${CMD_PFX}configure
endif

build: dist/setup-config
	${CMD} ${CMD_PFX}build

install:
ifeq ($(STACK),1)
	stack install
else
	cabal ${CMD_PFX}copy
	cabal ${CMD_PFX}register
endif

doc:
	${CMD} ${CMD_PFX}haddock

clean:
	${CMD} ${CMD_PFX}clean
	bash bin/clean_html

html::
	bash bin/update_html

# Make a debian package. First add a suitable entry with the correct GF version
# number to the top of debian/changelog.
# (Tested on Ubuntu 15.04. You need to install dpkg-dev & debhelper.)
deb:
	dpkg-buildpackage -b -uc -d

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
