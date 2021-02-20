# LPGF testsuite & benchmark

## Test

LPGF must be equivalent to PGF in terms of linearisation output.

Possible exceptions:
- No handling of variants (design choice)
- Rendering of missing fucntions

### Running

```
stack build --test --bench --no-run-tests --no-run-benchmarks
stack test gf:test:lpgf # all LPGF tests
stack test gf:test:lpgf --test-arguments="unittests/Params" # specific grammar
stack test gf:test:lpgf --test-arguments="foods/Foods Fre Ger" # specific grammar and languages
```

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
stack bench --benchmark-arguments "compile pgf  testsuite/lpgf/foods/Foods*.gf +RTS -T -RTS"
stack bench --benchmark-arguments "compile lpgf testsuite/lpgf/foods/Foods*.gf +RTS -T -RTS"
stack bench --benchmark-arguments "run pgf  Foods.pgf  testsuite/lpgf/foods/Foods-all.trees +RTS -T -RTS"
stack bench --benchmark-arguments "run pgf2 Foods.pgf  testsuite/lpgf/foods/Foods-all.trees +RTS -T -RTS"
stack bench --benchmark-arguments "run lpgf Foods.lpgf testsuite/lpgf/foods/Foods-all.trees +RTS -T -RTS"
```
