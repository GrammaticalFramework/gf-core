# LPGF testsuite & benchmark

## Test

LPGF must be equivalent to PGF in terms of linearisation output.

Possible exceptions:
- No handling of variants (design choice)
- Rendering of missing fucntions

## Benchmark

### Compilation

Comparing PGF, LPGF along following criteria:

- Time
- Memory
- Binary file size

### Runtime (linearisation)

Comparing PGF, PGF2, LPGF along following criteria:

- Time
- Memory

### Running

Run each command separately so that memory measurements are isolated.
The `+RTS -T -RTS` is so that GHC can report its own memory usage.

```
stack build --test --bench --no-run-tests --no-run-benchmarks
stack bench --benchmark-arguments "compile pgf  testsuite/lpgf/Foods*.gf +RTS -T -RTS"
stack bench --benchmark-arguments "compile lpgf testsuite/lpgf/Foods*.gf +RTS -T -RTS"
stack bench --benchmark-arguments "run pgf  Foods.pgf  testsuite/lpgf/foods-all.trees +RTS -T -RTS"
stack bench --benchmark-arguments "run pgf2 Foods.pgf  testsuite/lpgf/foods-all.trees +RTS -T -RTS"
stack bench --benchmark-arguments "run lpgf Foods.lpgf testsuite/lpgf/foods-all.trees +RTS -T -RTS"
```
