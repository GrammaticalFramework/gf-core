# GF Core releases

🚨 WARNING! The information here is preliminary!

## Creating a new release

### 1. Prepare the repository

**Web pages**

1. Create `download/index-X.Y.md` with installation instructions.
1. Create `download/release-X.Y.md` with changelog information.
1. Update `download/index.html` to redirect to the new version.
1. Add announcement in news section in `index.html`

**Version numbers**

1. Update version number in `gf.cabal` (ommitting `-git` suffix)
1. Add a new line in `debian/changelog`

### 2. Create GitHub release

1. When the above changes are committed to the `master` branch in the repository,
   check that all builds are successful:
  - https://github.com/GrammaticalFramework/gf-core/actions
  - https://travis-ci.org/github/GrammaticalFramework/gf-core
1. Create a GitHub release here: https://github.com/GrammaticalFramework/gf-core/releases/new
  with a tag format `RELEASE-X.Y`

### 3. Binary packages

Build and attach binaries to the release by running the relevant GitHub Actions workflows (TODO):

1. Go to https://github.com/GrammaticalFramework/gf-rgl/actions
1. Click "Build [platform] package" under _Workflows_
1. Click "Run workflow" and specify the tag `RELEASE-X.Y`

### 4. Upload to Hackage

1. Run `make sdist`
1. Visit `https://hackage.haskell.org/upload` and upload the file `dist/gf-X.Y.tar.gz`,
   OR upload directly with Cabal (≥2.4): `cabal upload dist/gf-X.Y.tar.gz`
1. If the documentation-building fails on the Hackage server, do:
```
cabal v2-haddock --builddir=dist/docs --haddock-for-hackage --enable-doc
cabal upload --documentation dist/docs/*-docs.tar.gz
```
