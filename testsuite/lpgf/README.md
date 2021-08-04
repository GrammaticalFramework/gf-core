# LPGF testsuite & benchmark

## Testsuite

LPGF must be equivalent to PGF in terms of linearisation output.

Possible exceptions:
- No handling of variants (design choice)
- Rendering of missing functions

**N.B.**
Phrasebook doesn't compile with RGL after 1131058b68c204a8d1312d2e2a610748eb8032cb

### Running

```
stack build --work-dir .stack-work-test --test --no-run-tests
stack test  --work-dir .stack-work-test gf:test:lpgf # all LPGF tests
stack test  --work-dir .stack-work-test gf:test:lpgf --test-arguments="unittests/Params" # specific grammar
stack test  --work-dir .stack-work-test gf:test:lpgf --test-arguments="foods/Foods Fre Ger" # specific grammar and languages
stack test  --work-dir .stack-work-test gf:test:lpgf --test-arguments="phrasebook/Phrasebook"
```

Set environment variable `DEBUG=1` to enable dumping of intermediate formats.

## Benchmark

Compare performance metrics between LPGF and PGF[2]. Note: correctness is not checked here.

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
stack build --work-dir .stack-work-bench --bench --no-run-benchmarks &&
stack bench --work-dir .stack-work-bench --benchmark-arguments "compile pgf  testsuite/lpgf/foods/Foods*.gf +RTS -T -RTS" &&
stack bench --work-dir .stack-work-bench --benchmark-arguments "compile lpgf testsuite/lpgf/foods/Foods*.gf +RTS -T -RTS" &&
stack bench --work-dir .stack-work-bench --benchmark-arguments "run pgf  Foods.pgf  testsuite/lpgf/foods/Foods-all.trees +RTS -T -RTS" &&
stack bench --work-dir .stack-work-bench --benchmark-arguments "run pgf2 Foods.pgf  testsuite/lpgf/foods/Foods-all.trees +RTS -T -RTS" &&
stack bench --work-dir .stack-work-bench --benchmark-arguments "run lpgf Foods.lpgf testsuite/lpgf/foods/Foods-all.trees +RTS -T -RTS"
```

```
stack build --work-dir .stack-work-bench --bench --no-run-benchmarks &&
stack bench --work-dir .stack-work-bench --benchmark-arguments "compile pgf  testsuite/lpgf/phrasebook/Phrasebook*.gf +RTS -T -RTS" &&
stack bench --work-dir .stack-work-bench --benchmark-arguments "compile lpgf testsuite/lpgf/phrasebook/Phrasebook*.gf +RTS -T -RTS" &&
stack bench --work-dir .stack-work-bench --benchmark-arguments "run pgf  Phrasebook.pgf  testsuite/lpgf/phrasebook/Phrasebook-10000.trees +RTS -T -RTS" &&
stack bench --work-dir .stack-work-bench --benchmark-arguments "run pgf2 Phrasebook.pgf  testsuite/lpgf/phrasebook/Phrasebook-10000.trees +RTS -T -RTS" &&
stack bench --work-dir .stack-work-bench --benchmark-arguments "run lpgf Phrasebook.lpgf testsuite/lpgf/phrasebook/Phrasebook-10000.trees +RTS -T -RTS"
```

## Profiling

```
stack bench --work-dir .stack-work-profile --profile --benchmark-arguments "compile lpgf testsuite/lpgf/phrasebook/PhrasebookFre.gf +RTS -T -p -h -RTS"
```

Produced files:
- `lpgf-bench.prof` - total time and memory allocation (`-p`)
- `lpgf-bench.hp` - heap profile (`-h`)

```
stack exec -- hp2ps -c lpgf-bench.hp && open lpgf-bench.ps
```

**Resources**

- https://downloads.haskell.org/ghc/8.6.5/docs/html/users_guide/profiling.html
- http://book.realworldhaskell.org/read/profiling-and-optimization.html
- https://wiki.haskell.org/Performance


### Honing in

```
stack build --test --bench --no-run-tests --no-run-benchmarks &&
stack bench --benchmark-arguments "compile lpgf testsuite/lpgf/phrasebook/PhrasebookFre.gf +RTS -T -RTS"
```

**Baseline PGF**
- compile: 1.600776s
- size: 2.88 MB  Phrasebook.pgf
Max memory: 328.20 MB

**Baseline LPGF = B**
- compile: 12.401099s
- size: 3.01 MB  Phrasebook.lpgf
Max memory: 1.33 GB

**Baseline LPGF String instead of Text**
- compile: 12.124689s
- size: 3.01 MB  Phrasebook.lpgf
Max memory: 1.34 GB

**Baseline LPGF with impossible pruning**
- compile: 7.406503s
- size: 3.01 MB  Phrasebook.lpgf
Max memory: 1.13 GB


**B -extractStrings**
- compile: 13.822735s
- size: 5.78 MB  Phrasebook.lpgf
Max memory: 1.39 GB

**B -cleanupRecordFields**
- compile: 13.670776s
- size: 3.01 MB  Phrasebook.lpgf
Max memory: 1.48 GB

**No generation at all = E**
- compile: 0.521001s
- size: 3.27 KB  Phrasebook.lpgf
Max memory: 230.69 MB

**+ Concat, Literal, Error, Predef, Tuple, Variant, Commented**
- compile: 1.503594s
- size: 3.27 KB  Phrasebook.lpgf
Max memory: 395.31 MB

**+ Var, Pre, Selection**
- compile: 1.260184s
- size: 3.28 KB  Phrasebook.lpgf
Max memory: 392.17 MB

**+ Record**
- compile: 1.659233s
- size: 7.07 KB  Phrasebook.lpgf
Max memory: 397.41 MB

**+ Projection = X**
- compile: 1.446217s
- size: 7.94 KB  Phrasebook.lpgf
Max memory: 423.62 MB

**X + Param**
- compile: 2.073838s
- size: 10.82 KB  Phrasebook.lpgf
Max memory: 619.71 MB

**X + Table**
- compile: 11.26558s
- size: 2.48 MB  Phrasebook.lpgf
Max memory: 1.15 GB

**RawIdents**
- compile: 5.393466s
- size: 3.01 MB  Phrasebook.lpgf
Max memory: 1.12 GB

### Repeated terms in compilation

**Param and Table**

| Concr         |  Total | Unique | Perc |
|:--------------|-------:|-------:|-----:|
| PhrasebookEng |   8673 |   1724 |  20% |
| PhrasebookSwe |  14802 |   2257 |  15% |
| PhrasebookFin | 526225 |   4866 |   1% |

**Param**

| Concr         |  Total | Unique | Perc |
|:--------------|-------:|-------:|-----:|
| PhrasebookEng |   3211 |     78 |   2% |
| PhrasebookSwe |   7567 |     69 |   1% |
| PhrasebookFin | 316355 |    310 | 0.1% |

**Table**

| Concr         |  Total | Unique | Perc |
|:--------------|-------:|-------:|-----:|
| PhrasebookEng |   5470 |   1654 |  30% |
| PhrasebookSwe |   7243 |   2196 |  30% |
| PhrasebookFin | 209878 |   4564 |   2% |

### After impelementing state monad for table memoisation

**worse!**
- compile: 12.55848s
- size: 3.01 MB  Phrasebook.lpgf
Max memory: 2.25 GB

**Params**

| Concr         |  Total | Misses |  Perc |
|:--------------|-------:|-------:|------:|
| PhrasebookEng |   3211 |     72 |    2% |
| PhrasebookSwe |   7526 |     61 |    1% |
| PhrasebookFin | 135268 |    333 |  0.2% |
| PhrasebookFre | 337102 |     76 | 0.02% |

**Tables**

| Concr         | Total | Misses | Perc |
|:--------------|------:|-------:|-----:|
| PhrasebookEng |  3719 |   3170 |  85% |
| PhrasebookSwe |  4031 |   3019 |  75% |
| PhrasebookFin | 36875 |  21730 |  59% |
| PhrasebookFre | 41397 |  32967 |  80% |

Conclusions:
- map itself requires more memory than actual compilation
- lookup/insert is also as bad as actual compilation

Tried HashMap (deriving Hashable for LinValue), no inprovement.
Using show on LinValue for keys is incredibly slow.

# Notes on compilation

## 1 (see unittests/Params4)

**param defns**
P = P1 | P2
Q = Q1 | Q2
R = RP P | RPQ P Q | R0
X = XPQ P Q

**translation**
NB: tuples may be nested, but will be concatted at runtime

P1        = <1>
P2        = <2>

Q1        = <1>
Q2        = <2>

R P1      = <1,1>
R P2      = <1,2>
RPQ P1 Q1 = <2,1,1>
RPQ P1 Q2 = <2,1,2>
RPQ P2 Q1 = <2,2,1>
RPQ P2 Q2 = <2,2,2>
R0        = <3>

XPQ P1 Q1 = <1,1,1>
XPQ P1 Q2 = <1,1,2>
XPQ P2 Q1 = <1,2,1>
XPQ P2 Q2 = <1,2,2>

P => Str
<"P1","P2">

{p:P ; q:Q} => Str
<<"P1;Q1","P1;Q2">,<"P2;Q1","P2;Q2">>

{p=P2; q=Q1}
<<2>,<1>>

R => Str
< <"RP P1","RP P2">,
  < <"RPQ P1 Q1","RPQ P1 Q2">,
    <"RPQ P2 Q1","RPQ P2 Q2"> >,
 "R0"
>

X => Str
<<<"XPQ P1 Q1","XPQ P1 Q2">,
  <"XPQ P2 Q1","XPQ P2 Q2">>>

{p=P2 ; r=R0}
<<2>,<3>>

{p=P2 ; r1=RP P1 ; r2=RPQ P1 Q2 ; r3=R0 }
< <2>  ,  <1, 1> ,   <2,  1, 2> ,   <3>>

## 2 (see unittests/Params5)

**param defns**

P = P1 | PQ Q
Q = Q1 | QR R
R = R1 | R2

**translation**

P1       = <1>
PQ Q1    = <2,1>
PQ QR R1 = <2,2,1>
PQ QR R2 = <2,2,2>

Q1       = <1>
QR R1    = <2,1>
QR R2    = <2,2>

R1       = <1>
R2       = <2>

P => Str
<"P1",<"PQ Q1",<"PQ (QR R1)","PQ (QR R2)">>>

{q:Q ; p:P} => Str
< <"Q1;P1",<"Q1;PQ Q1",<"Q1;PQ (QR R1)","Q1;PQ (QR R2)">>>,
  <
    <"QR R1;P1",<"QR R1;PQ Q1",<"QR R1;PQ (QR R1)","QR R1;PQ (QR R2)">>>,
    <"QR R2;P1",<"QR R2;PQ Q1",<"QR R2;PQ (QR R1)","QR R2;PQ (QR R2)">>>
  >
>

{q=Q1 ; p=P1}            = <<1>,<1>>
{q=Q1 ; p=PQ Q1}         = <<1>,<2,1>>
{q=Q1 ; p=PQ (QR R1)}    = <<1>,<2,2,1>>
{q=Q1 ; p=PQ (QR R2)}    = <<1>,<2,2,2>>

{q=QR R1 ; p=P1}         = <<2,1>,<1>>
{q=QR R1 ; p=PQ Q1}      = <<2,1>,<2,1>>
{q=QR R1 ; p=PQ (QR R1)} = <<2,1>,<2,2,1>>
{q=QR R1 ; p=PQ (QR R2)} = <<2,1>,<2,2,2>>

{q=QR R2 ; p=P1}         = <<2,2>,<1>>
{q=QR R2 ; p=PQ Q1}      = <<2,2>,<2,1>>
{q=QR R2 ; p=PQ (QR R1)} = <<2,2>,<2,2,1>>
{q=QR R2 ; p=PQ (QR R2)} = <<2,2>,<2,2,2>>

**NOTE**: GF will swap q and p in record, as part of record field sorting, resulting in the following:

{p:P ; q:Q} => Str
< <"P1;Q1", <"P1;QR R1","P1;QR R2">>,
  < <"PQ Q1;Q1", <"PQ Q1;QR R1","PQ Q1;QR R2">>,
    < <"PQ (QR R1);Q1", <"PQ (QR R1);QR R1","PQ (QR R1);QR R2">>,
      <"PQ (QR R2);Q1", <"PQ (QR R2);QR R1","PQ (QR R2);QR R2">>
    >
  >
>

{p=P1 ; q=Q1}            = <<1>,<1>>
{p=P1 ; q=QR R1}         = <<1>,<2,1>>
{p=P1 ; q=QR R2}         = <<1>,<2,2>>

{p=PQ Q1 ; q=Q1}         = <<2,1>,<1>>
{p=PQ Q1 ; q=QR R1}      = <<2,1>,<2,1>>
{p=PQ Q1 ; q=QR R2}      = <<2,1>,<2,2>>

{p=PQ (QR R1) ; q=Q1}    = <<2,2,1>,<1>>
{p=PQ (QR R1) ; q=QR R1} = <<2,2,1>,<2,1>>
{p=PQ (QR R1) ; q=QR R2} = <<2,2,1>,<2,2>>

{p=PQ (QR R2) ; q=Q1}    = <<2,2,2>,<1>>
{p=PQ (QR R2) ; q=QR R1} = <<2,2,2>,<2,1>>
{p=PQ (QR R2) ; q=QR R2} = <<2,2,2>,<2,2>>


{pp: {p:P} ; q:Q} => Str

{pp={p=P1} ; q=Q1}            = <<<1>>,<1>>
{pp={p=P1} ; q=QR R1}         = <<<1>>,<2,1>>
{pp={p=P1} ; q=QR R2}         = <<<1>>,<2,2>>

{pp={p=PQ Q1} ; q=Q1}         = <<<2,1>>, <1>>
{pp={p=PQ Q1} ; q=QR R1}      = <<<2,1>>, <2,1>>
{pp={p=PQ Q1} ; q=QR R2}      = <<<2,1>>, <2,2>>

{pp={p=PQ (QR R1)} ; q=Q1}    = <<<2,2,1>>,<1>>
{pp={p=PQ (QR R1)} ; q=QR R1} = <<<2,2,1>>,<2,1>>
{pp={p=PQ (QR R1)} ; q=QR R2} = <<<2,2,1>>,<2,2>>

{pp={p=PQ (QR R2)} ; q=Q1}    = <<<2,2,2>>,<1>>
{pp={p=PQ (QR R2)} ; q=QR R1} = <<<2,2,2>>,<2,1>>
{pp={p=PQ (QR R2)} ; q=QR R2} = <<<2,2,2>>,<2,2>>
