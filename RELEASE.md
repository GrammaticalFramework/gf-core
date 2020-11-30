# GF Core releases

**Note:**
The RGL is now released completely separately from GF Core.
See the [RGL's RELEASE.md](https://github.com/GrammaticalFramework/gf-rgl/blob/master/RELEASE.md).

## Creating a new release

### 1. Prepare the repository

**Web pages**

1. Create `download/index-X.Y.md` with installation instructions.
1. Create `download/release-X.Y.md` with changelog information.
1. Update `download/index.html` to redirect to the new version.
1. Add announcement in news section in `index.html`.

**Version numbers**

1. Update version number in `gf.cabal` (ommitting `-git` suffix).
1. Add a new line in `debian/changelog`.

### 2. Create GitHub release

1. When the above changes are committed to the `master` branch in the repository
  and pushed, check that all CI workflows are successful (fixing as necessary):
  - <https://github.com/GrammaticalFramework/gf-core/actions>
  - <https://travis-ci.org/github/GrammaticalFramework/gf-core>
1. Create a GitHub release [here](https://github.com/GrammaticalFramework/gf-core/releases/new) using tag format `RELEASE-X.Y`.

### 3. Binary packages

The binaries will be built automatically by the GitHub Actions workflows,
but the generated artifacts must be manually attached to the release as _assets_.

1. Go to <https://github.com/GrammaticalFramework/gf-core/actions>.
1. Click "Build [platform] Package" under _Workflows_.
1. Choose the workflow run corresponding to the release commit SHA.
1. Download the artifact locally, then add to the release with a name `gf-X.Y-PLATFORM.EXT` (e.g. `gf-3.11-macos.pkg`).

### 4. Upload to Hackage

1. Run `make sdist`
1. Upload the package, either:
    1. **Manually**: visit <https://hackage.haskell.org/upload> and upload the file `dist/gf-X.Y.tar.gz`
    2. **via Cabal (≥2.4)**: `cabal upload dist/gf-X.Y.tar.gz`
1. If the documentation-building fails on the Hackage server, do:
```
cabal v2-haddock --builddir=dist/docs --haddock-for-hackage --enable-doc
cabal upload --documentation dist/docs/*-docs.tar.gz
```

## Miscellaneous

### What is the tag `GF-3.10`?

For GF 3.10, the Core and RGL repositories had already been separated, however
the binary packages still included the RGL. `GF-3.10` is a tag that was created
in both repositories ([gf-core](https://github.com/GrammaticalFramework/gf-core/releases/tag/GF-3.10) and [gf-rgl](https://github.com/GrammaticalFramework/gf-rgl/releases/tag/GF-3.10)) to indicate which versions of each went into the binaries.
