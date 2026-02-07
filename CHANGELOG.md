# Changelog

## 1.1 → 2.0.0.0

### Breaking changes

- **Fixed `groundedF` infinite-loop bug**: `groundedF` now correctly applies
  the characteristic function at each step instead of recursing with unchanged
  arguments. Code relying on `groundedF'` as a workaround can switch back to
  `groundedF`.

- **CLI rewritten**: The `dungell` executable now uses `optparse-applicative`
  instead of `cmdargs`. Command-line flags have changed:
  - Semantics are selected via `--grounded`, `--preferred`, `--stable`,
    `--semi-stable`, or `--all`

### Improvements

- Upgraded to `cabal-version: 3.0` format
- Relaxed `containers` upper bound to build on GHC 9.4+
- Added `Language.Dung` convenience re-export module
- Added test suite (tasty + QuickCheck + doctest)
- Added GitHub Actions CI for GHC 9.4, 9.6, 9.8, 9.10
- Fixed all `-Wall` warnings
- Removed dependency on unmaintained `cmdargs` library
- Converted documentation to Markdown

## 1.0.0.1 → 1.1

This package version is now compatible with the command-line application for
the ICCMA competition. See: <https://github.com/nebasuke/DungICCMA> and the
competition website: <http://argumentationcompetition.org/2015/index.html>

This package has again been significantly extended and now includes:

- Strict version of the grounded fixpoint semantics.
- Definitions of what it means to be a preferred and stable extension.
- Naive implementations of complete, preferred and stable semantics using
  the characteristic function. The fixpoint definitions are actually FASTER
  than the fancy algorithm implementation.
- Included the fast implementations of `intersect`, `nub` and `(\\)` by
  Niklas Hambuechen.
- Added some clarifying text for the "complete" labelling.

## 1.0 → 1.0.0.1

- Fixed the record declaration in Main.hs.
- Added an example file.

## 0.9 → 1.0

This package version is now compatible with the translation package
CarneadesIntoDung. See <http://hackage.haskell.org/package/CarneadesIntoDung/>

This package has furthermore been significantly extended and now includes:

- Preferred, stable and semi-stable semantics along with all definitions from
  Caminada's paper "An Algorithm for Computing Semi-Stable Semantics".
- An Input module, allowing files in standard CEGARTIX/PrefSat format to be
  parsed.
- An Output module, allowing AFs in this package to be outputted in standard
  CEGARTIX/PrefSat format.
- A main executable, allowing input files to be read, argumentation frameworks
  to be outputted and evaluated.
