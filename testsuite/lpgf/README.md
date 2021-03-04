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
stack build --test --bench --no-run-tests --no-run-benchmarks
stack bench --benchmark-arguments "compile pgf  testsuite/lpgf/foods/Foods*.gf +RTS -T -RTS"
stack bench --benchmark-arguments "compile lpgf testsuite/lpgf/foods/Foods*.gf +RTS -T -RTS"
stack bench --benchmark-arguments "run pgf  Foods.pgf  testsuite/lpgf/foods/Foods-all.trees +RTS -T -RTS"
stack bench --benchmark-arguments "run pgf2 Foods.pgf  testsuite/lpgf/foods/Foods-all.trees +RTS -T -RTS"
stack bench --benchmark-arguments "run lpgf Foods.lpgf testsuite/lpgf/foods/Foods-all.trees +RTS -T -RTS"
```


# Notes on compilation

## 1

param defns
  P = P1 | P2
  Q = Q1 | Q2
  R = RP P | RPQ P Q | R0
  X = XPQ P Q

translation (NB: tuples may be listed, but will be concatted at runtime)

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

## 2

param defns
  P = P1 | PQ Q
  Q = Q1 | QR R
  R = R1 | R2

translation

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
