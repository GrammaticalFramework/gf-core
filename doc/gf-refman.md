---
title: GF Language Reference Manual
author: Aarne Ranta, Krasimir Angelov
date: June 2014, GF 3.6
toc: true
---

This document is a reference manual to the GF programming language. GF,
Grammatical Framework, is a special-purpose programming language,
designed to support definitions of grammars.

This document is not an introduction to GF; such introduction can be
found in the GF tutorial available on line on the GF web page,

[`grammaticalframework.org`](http://grammaticalframework.org)

This manual covers only the language, not the GF compiler or interactive
system. We will however make some references to different compiler
versions, if they involve changes of behaviour having to do with the
language specification.

This manual is meant to be fully compatible with GF version 3.0. Main
discrepancies with version 2.8 are indicated, as well as with the
reference article on GF,

A. Ranta, \"Grammatical Framework. A Type Theoretical Grammar
Formalism\", *The Journal of Functional Programming* 14(2), 2004, pp.
145-189.

This article will referred to as \"the JFP article\".

As metalinguistic notation, we will use the symbols

-   *a* === *b* to say that *a* is syntactic sugar for *b*
-   *a* ==\> *b* to say that *a* is computed (or compiled) to *b*


Overview of GF
--------------

GF is a typed functional language, borrowing many of its constructs from
ML and Haskell: algebraic datatypes, higher-order functions, pattern
matching. The module system bears resemblance to ML (functors) but also
to object-oriented languages (inheritance). The type theory used in the
abstract syntax part of GF is inherited from logical frameworks, in
particular ALF (\"Another Logical Framework\"; in a sense, GF is Yet
Another ALF). From ALF comes also the use of dependent types, including
the use of explicit type variables instead of Hindley-Milner
polymorphism.

The look and feel of GF is close to Java and C, due to the use of curly
brackets and semicolons in structuring the code; the expression syntax,
however, follows Haskell in using juxtaposition for function application
and parentheses only for grouping.

To understand the constructs of GF, and especially their limitations in
comparison to general-purpose programming languages, it is essential to
keep in mind that GF is a special-purpose and non-turing-complete
language. Every GF program is ultimately compiled to a **multilingual
grammar**, which consists of an **abstract syntax** and a set of
**concrete syntaxes**. The abstract syntax defines a system of **syntax
trees**, and each concrete syntax defines a mapping from those syntax
trees to **nested tuples** of strings and integers. This mapping is
**compositional**, i.e. **homomorphic**, and moreover **reversible**:
given a nested tuple, there exists an effective way of finding the set
of syntax trees that map to this tuple. The procedure of applying the
mapping to a tree to produce a tuple is called **linearization**, and
the reverse search procedure is called **parsing**. It is ultimately the
requirement of reversibility that restricts GF to be less than
turing-complete. This is reflected in restrictions to recursion in
concrete syntax. Tree formation in abstract syntax, however, is fully
recursive.

Even though run-time GF grammars manipulate just nested tuples, at
compile time these are represented by by the more fine-grained labelled
records and finite functions over algebraic datatypes. This enables the
programmer to write on a higher abstraction level, and also adds type
distinctions and hence raises the level of checking of programs.


The module system
-----------------


### Top-level and supplementary module structure

The big picture of GF as a programming language for multilingual
grammars explains its principal module structure. Any GF grammar must
have an abstract syntax module; it can in addition have any number of
concrete syntax modules matching that abstract syntax. Before going to
details, we give a simple example: a module defining the **category**
`A` of adjectives and one adjective-forming **function**, the zero-place
function `Even`. We give the module the name `Adj`. The GF code for the
module looks as follows:

        abstract Adj = {
          cat A ;
          fun Even : A ;
        }

Here are two concrete syntax modules, one intended for mapping the trees
to English, the other to Swedish. The mappling is defined by `lincat`
definitions assigning a **linearization type** to each category, and
`lin` definitions assigning a **linearization** to each function.

        concrete AdjEng of Adj = {
          lincat A = {s : Str} ;
          lin Even = {s = "even"} ;
        }

        concrete AdjSwe of Adj = {
          lincat A = {s : AForm => Str} ;
          lin Even = {s = table {
            ASg Utr   => "jämn" ;
            ASg Neutr => "jämnt" ;
            APl       => "jämna"
            }
          } ;
          param AForm = ASg Gender | APl ;
          param Gender = Utr | Neutr ;
        }

These examples illustrate the main ideas of multilingual grammars:

-   the concrete syntax must match the abstract syntax:
    -   every `cat` is given a `lincat`
    -   every `fun` is given a `lin`

<!-- -->

-   the concrete syntax is internally coherent:
    -   the `lin` rules respect the types defined by `lincat` rules

<!-- -->

-   concrete syntaxes are independent of each other
    -   they can use different `lincat` and `lin` definitions
    -   they can define their own **parameter types** (`param`)

The first two ideas form the core of the **static checking** of GF
grammars, eliminating the possibility of run-time errors in
linearization and parsing. The third idea gives GF the expressive power
needed to map abstract syntax to vastly different languages.

Abstract and concrete modules are called **top-level grammar modules**,
since they are the ones that remain in grammar systems at run time.
However, in order to support **modular grammar engineering**, GF
provides much more module structure than strictly required in top-level
grammars.

**Inheritance**, also known as **extension**, means that a module can
inherit the contents of one or more other modules to which new
judgements are added, e.g.

        abstract MoreAdj = Adj ** {
          fun Odd : A ;
        }

**Resource modules** define parameter types and **operations** usable in
several concrete syntaxes,

        resource MorphoFre = {
          param Number = Sg | Pl ;
          param Gender = Masc | Fem ;
          oper regA : Str -> {s : Gender => Number => Str} =
            \fin -> {
              s = table {
                Masc => table {Sg => fin ; Pl => fin + "s"} ;
                Fem  => table {Sg => fin + "e" ; Pl => fin + "es"}
              }
            } ;
        }

By **opening**, a module can use the contents of a resource module
without inheriting them, e.g.

        concrete AdjFre of Adj = open MorphoFre in {
          lincat A = {s : Gender => Number => Str} ;
          lin Even = regA "pair" ;
        }

**Interfaces** and **instances** separate the contents of a resource
module to type signatures and definitions, in a way analogous to
abstract vs. concrete modules, e.g.

        interface Lexicon = {
          oper Adjective : Type ;
          oper even_A : Adjective ;
        }

        instance LexiconEng of Lexicon = {
          oper Adjective = {s : Str} ;
          oper even_A = {s = "even"} ;
        }

**Functors** i.e. **parametrized modules** i.e. **incomplete modules**,
defining a concrete syntax in terms of an interface.

        incomplete concrete AdjI of Adj = open Lexicon in {
          lincat A = Adjective ;
          lin Even = even_A ;
        }

A functor can be **instantiated** by providing instances of its open
interfaces.

        concrete AdjEng of Adj = AdjI with (Lexicon = LexiconEng) ;


### Compilation units

The compilation unit of GF source code is a file that contains a module.
Judgements outside modules are supported only for backward
compatibility, as explained [here](#oldgf). Every source file, suffixed
`.gf`, is compiled to a \"GF object file\", suffixed `.gfo` (as of GF
Version 3.0 and later). For runtime grammar objects used for parsing and
linearization, a set of `.gfo` files is linked to a single file suffixed
`.pgf`. While `.gf` and `.gfo` files may contain modules of any kinds, a
`.pgf` file always contains a multilingual grammar with one abstract and
a set of concrete syntaxes.

The following diagram summarizes the files involved in the compilation
process.

`module1.gf module2.gf ... modulen.gf`

==\>

`module1.gfo module2.gfo ... modulen.gfo`

==\>

grammar.pgf

Both `.gf` and `.gfo` files are written in the GF source language;
`.pgf` files are written in a lower-level format. The process of
translating `.gf` to `.gfo` consists of **name resolution**, **type
annotation**, **partial evaluation**, and **optimization**. There is a
great advantage in the possibility to do this separately for GF modules
and saving the result in `.gfo` files. The partial evaluation phase, in
particular, is time and memory consuming, and GF libraries are therefore
distributed in `.gfo` to make their use less arduous.

*In GF before version 3.0, the object files are in a format called
`.gfc`,* *and the multilingual runtime grammar is in a format called
`.gfcm`.*

The standard compiler has a built-in **make facility**, which finds out
what other modules are needed when compiling an explicitly given module.
This facility builds a dependency graph and decides which of the
involved modules need recompilation (from `.gf` to `.gfo`), and for
which the GF object can be used directly.


### Names

Each module *M* defines a set of **names**, which are visible in *M*
itself, in all modules extending *M* (unless excluded, as explained
[here](#restrictedinheritance)), and all modules opening *M*. These
names can stand for abstract syntax categories and functions, parameter
types and parameter constructors, and operations. All these names live
in the same **name space**, which means that a name entering a module
more than once due to inheritance or opening can lead to a **conflict**.
It is specified [here](#renaming) how these conflicts are resolved.

The names of modules live in a name space separate from the other names.
Even here, all names must be distinct in a set of files compiled to a
multilingual grammar. In particular, even files residing in different
directories must have different names, since GF has no notion of
hierarchic module names.

Lexically, names belong to the class of **identifiers**. An idenfifier
is a letter followed by any number of letters, digits, undercores (`_`)
and primes (`'`). Upper- and lower-case letters are treated as distinct.
Nothing dictates the choice of upper or lower-case initials, but the
standard libraries follow conventions similar to Haskell:

-   upper case is used for modules, abstract syntax categories and
    functions, parameter types and constructors, and type synonyms
-   lower case is used for non-type-valued operations and for variables

[]{#identifiers}

\"Letters\" as mentioned in the identifier syntax include all 7-bit
ASCII letters. Iso-latin-1 and Unicode letters are supported in varying
degrees by different tools and platforms, and are hence not recommended
in identifiers.


### The structure of a module

Modules of all types have the following structure:

*moduletype* *name* `=` *extends* *opens* *body*

The part of the module preceding the body is its **header**. The header
defines the type of the module and tells what other modules it inherits
and opens. The body consists of the judgements that introduce all the
new names defined by the module.

Any of the parts *extends*, *opens*, and *body* may be empty. If they
are all filled, delimiters and keywords separate the parts in the
following way:

*moduletype* *name* `=` *extends* `**` `open` *opens* `in` `{` *body*
`}`

The part *moduletype* *name* looks slightly different if the type is
`concrete` or `instance`: the *name* intrudes between the type keyword
and the name of the module being implemented and which really belongs to
the type of the module:

`concrete` *name* `of` *abstractname*

The only exception to the schema of functor syntax is functor
instantiations: the instantiation list is given in a special way between
*extends* and *opens*:

`incomplete concrete` *name* `of` *abstractname* `=` *extends* `**`
*functorname* `with` *instantiations* `**` `open` *opens* `in` `{`
*body* `}`

Logically, the part \"*functorname* `with` *instantiations*\" should
really be one of the *extends*. This is also shown by the fact that it
can have restricted inheritance (concept defined
[here](#restrictedinheritance)).


### Module types, headers, and bodies

The *extends* and *opens* parts of a module header are lists of module
names (with possible qualifications, as defined below
[here](#qualifiednames)). The first step of type checking a module
consists of verifying that these names stand for modules of approptiate
module types. As a rule of thumb,

-   the *extends* of a module must have the same *moduletype*
-   the *opens* of a module must be of type `resource`

However, the precise rules are a little more fine-grained, because of
the presence of interfaces and their instances, and the possibility to
reuse abstract and concrete modules as resources. The following table
gives, for all module types, the possible module types of their
*extends* and *opens*, as well as the forms of judgement legal in that
module type.

  module type                 extends      opens        body
  --------------------------- ------------ ------------ ----------------------------
  `abstract`                  abstract     \-           `cat, fun, def, data`
  `concrete of` *abstract*    concrete     resource\*   `lincat, cat, oper, param`
  `resource`                  resource\*   resource\*   `oper, param`
  `interface`                 resource+    resource\*   `oper, param`
  `instance of` *interface*   resource\*   resource\*   `oper, param`
  `incomplete` concrete       concrete+    resource+    `lincat, cat, oper, param`

The table uses the following shorthands for lists of module types:

-   resource\*: resource, instance, concrete
-   resource+: resource\*, interface, abstract
-   concrete+: concrete, incomplete concrete

The legality of judgements in the body is checked before the judgements
themselves are checked.

The forms of judgement are explained [here](#judgementforms).


### Digression: the logic of module types

Why are the legality conditions of opens and extends so complicated? The
best way to grasp them is probably to consider a simplified logical
model of the module system, replacing modules by types and functions.
This model could actually be developed towards treating modules in GF as
first-class objects; so far, however, this step has not been motivated
by any practical needs.

  module                                     object and type
  ------------------------------------------ -----------------------
  abstract A = B                             A = B : type
  concrete C of A = B                        C = B : A -\> S
  interface I = B                            I = B : type
  instance J of I = B                        J = B : I
  incomplete concrete C of A = open I in B   C = B : I -\> A -\> S
  concrete K of A = C with (I=J)             K = B(J) : A -\> S
  resource R = B                             R = B : I
  concrete C of A = open R in B              C = B(R) : A -\> S

A further step of defining modules as first-class objects would use
GADTs and record types:

-   an abstract syntax is a Generalized Algebraic Datatype (GADT)
-   the target type `S` of concrete syntax is the type of nested tuples
    over strings and integers
-   an interface is a labelled record type
-   an instance is a record of the type defined by the interface
-   a functor, with a module body opening an interface, is a function on
    its instances
-   the instantiation of a functor is an application of the function to
    some instance
-   a resource is a typed labelled record, putting together an interface
    and an instance of it
-   the body of a module opening a resource is as a function on the
    interface implicit in the resource; this function is immediately
    applied to the instance defined in the resource

Slightly unexpectedly, interfaces and instances are easier to understand
in this way than resources - a resource is, indeed, more complex, since
it fuses together an interface and an instance.

[]{#openabstract}

When an abstract is used as an interface and a concrete as its instance,
they are actually reinterpreted so that they match the model. Then the
abstract is no longer a GADT, but a system of *abstract* datatypes, with
a record field of type `Type` for each category, and a function among
these types for each abstract syntax function. A concrete syntax
instantiates this record with linearization types and linearizations.


### Inheritance

After checking that the *extends* of a module are of appropriate module
types, the compiler adds the inherited judgements to the judgements
included in the body. The inherited judgements are not copied entirely,
but their names with links to the inherited module. Conflicts may arise
in this process: a name can have two definitions in the combined pool of
inherited and added judgements. Such a conflict is always an error: GF
provides no way to redefine an inherited constant.

Simple as the definition of a conflict may sound, it has to take care of
the inheritance hierarchy. A very common pattern of inheritance is the
**diamond**: inheritance from two modules which themselves inherit a
common base module. Assume that the base module defines a name `f`:

                N
              /   \
            M1     M2
              \   /
              Base {f}

Now, `N` inherits `f` from both `M1` and `M2`, so is there a conflict?
The answer in GF is *no*, because the \"two\" `f`\'s are in the end the
same: the one defined in `Base`. The situation is thus simpler than in
**multiple inheritance** in languages like C++, because definitions in
GF are **immutable**: neither `M1` nor `M2` can possibly have changed
the definition of `f` given in `Base`. In practice, the compiler manages
inheritance through hierarchy in a very simple way, by just always
creating a link not to the immediate parent, but the original ancestor;
this ancestor can be read from the link provided by the immediate
parent. Here is how links are created from source modules by the
compiler:

        Base {f}
        M1 {m1}   ===>  M1 {Base.f, m1}
        M2 {m2}   ===>  M2 {Base.f, m2}
        N  {n}    ===>  N  {Base.f, M1.m1, M2.m2, n}

[]{#restrictedinheritance}

Inheritance can be **restricted**. This means that a module can be
specified as inheriting *only* explicitly listed constants, or all
constants *except* ones explicitly listed. The syntax uses constant
names in brackets, prefixed by a minus sign in the case of an exclusion
list. In the following configuration, N inherits `a,b,c` from `M1`, and
all names but `d` from `M2`

        N = M1 {a,b,c}, M2-{d}

Restrictions are performed as a part of inheritance linking, module by
module: the link is created for a constant if and only if it is both
included in the module and compatible with the restriction. Thus, for
instance, an inadvertent usage can exclude a constant from one module
but inherit it from another one. In the following configuration, `f` is
inherited via `M1`, if `M1` inherits it.

        N = M1 [a,b,c], M2-[f]

Unintended inheritance may cause problems later in compilation, in the
judgement-level dependency analysis phase. For instance, suppose a
function `f` has category `C` as its type in `M`, and we only include
`f`. The exclusion has the effect of creating an ill-formed module:

        abstract M = {cat C ; fun f : C ;}
        M [f]   ===> {fun f : C ;}

One might expect inheritance restriction to be transitive: if an
included constant *b* depends on some other constant *a*, then *a*
should be included automatically. However, this rule would leave to
hard-to-detect inheritances. And it could only be applied later in the
compilation phase, when the compiler has not only collected the names
defined, but also resolved the names used in definitions.

Yet another pitfall with restricted inheritance is that it must be
stated for each module separately. For instance, a concrete syntax of an
abstract must exclude all those names that the abstract does, and a
functor instantiation must replicate all restrictions of the functor.


### Opening

Opening makes constants from other modules usable in judgements, without
inheriting them. This means that, unlike inheritance, opening is not
transitive.

[]{#qualifiednames}

Opening cannot be restricted as inheritance can, but it can be
**qualified**. This means that the names from the opened modules cannot
be used as such, but only as prefixed by a qualifier and a dot (`.`).
The qualifier can be any identifier, including the name of the module.
Here is an example of an *opens* list:

        open A, (X = XSLTS), (Y = XSLTS), B

If `A` defines the constant `a`, it can be accessed by the names

        a  A.a

If `XSLTS` defines the constant `x`, it can be accessed by the names

        X.x  Y.x  XSLTS.x

Thus qualification by real module name is always possible, and one and
the same module can be qualified in different ways at the same time (the
latter can be useful if you want to be able to change the
implementations of some constants to a different resource later). Since
the qualification with real module name is always possible, it is not
possible to \"swap\" the names of modules locally:

        open (A=B), (B=A) -- NOT POSSIBLE!

The list of qualifiers names and module names in a module header may
thus not contain any duplicates.


### Name resolution

[]{#renaming}

**Name resolution** is the compiler phase taking place after inheritance
linking. It qualifies all names occurring in the definition parts of
judgements (that is, just excluding the defined names themselves) with
the names of the modules they come from. If a name can come from
different modules (that is, not from their common ancestor), a conflict
is reported; this decision is hence not dependent on e.g. types, which
are known only at a later phase.

Qualification of names is the main device for avoiding conflicts in name
resolution. No other information is used, such as priorities between
modules. However, if a name is defined in different opened modules but
never used in the module body, a conflict does not arise: conflicts
arise only when names are used. Also in this respect, opening is thus
different from inheritance, where conflicts are checked independently of
use.

As usual, inner scope has priority in name resolution. This means that
if an identifier is in scope as a bound variable, it will not be
interpreted as a constant, unless qualified by a module name (variable
bindings are explained [here](#variablebinding)).


### Functor instantiations

We have dealt with the principles of module headers, inheritance, and
names in a general way that applies to all module types. The exception
is functor instantiations, that have an extra part of the instantiating
equations, assigning an instance to every interface. Here is a typical
example, displaying the full generality:

        concrete FoodsEng of Foods = PhrasesEng **
          FoodsI-[Pizza] with
            (Syntax = SyntaxEng),
            (LexFoods = LexFoodsEng) **
          open SyntaxEng, ParadigmsEng in {
            lin Pizza = mkCN (mkA "Italian") (mkN "pie") ;
        }

(The example is modified from Section 5.9 in the GF Tutorial.)

The instantiation syntax is similar to qualified *opens*. The
left-hand-side names must be interfaces, the right-hand-side names their
instances. (Recall that `abstract` can be use as `interface` and
`concrete` as its `instance`.) Inheritance from the functor can be
restricted, typically in the purpose of defining some excluded functions
in language-specific ways in the module body.


### Completeness

(This section refers to the forms of judgement introduced
[here](#judgementforms).)

A `concrete` is complete with respect to an `abstract`, if it contains a
`lincat` definition for every `cat` declaration, and a `lin` definition
for every `fun` declaration.

The same completeness criterion applies to functor instantiations. It is
not possible to use a partial functor instantiation, leading to another
functor.

Functors do not need to be complete in the sense concrete modules need.
The missing definitions can then be provided in the body of each functor
instantiation.

A `resource` is complete, if all its `oper` and `param` judgements have
a definition part. While a `resource` must be complete, an `interface`
need not. For an `interface`, it is the definition parts of judgements
are optional.

An `instance` is complete with respect to an `interface`, if it gives
the definition parts of all `oper` and `param` judgements that are
omitted in the `interface`. Giving definitions to judgements that have
already been defined in the `interface` is illegal. Type signatures, on
the other hand, can be repeated if the same types are used.

In addition to completing the definitions in an `interface`, its
instance may contain other judgements, but these must all be complete
with definitions.

Here is an example of an instance and its interface showing the above
variations:

        interface Pos = {
          param Case ;                 -- no definition
          param Number = Sg | Pl ;     -- definition given
          oper Noun : Type = {         -- relative definition given
            s : Number => Case => Str
          } ;
          oper regNoun : Str -> Noun ; -- no definition
        }

        instance PosEng of Pos = {
          param Case = Nom | Gen ;     -- definition of Case
                                       -- Number and Noun inherited
          oper regNoun = \dog -> {     -- type of regNoun inherited
            s = table {                -- definition of regNoun
              Sg => table {
                Nom => dog
                -- etc
              }
            } ;
          oper house_N : Noun =        -- new definition
            regNoun "house" ;
        }


Judgements
----------


### Overview of the forms of judgement

[]{#judgementforms}

A module body in GF is a set of **judgements**. Judgements are
definitions or declarations, sometimes combinations of the two; the
common feature is that every judgement introduces a name, which is
available in the module and whenever the module is extended or opened.

There are several different **forms of judgement**, identified by
different **judgement keywords**. Here is a list of all these forms,
together with syntax descriptions and the types of modules in which each
form can occur. The table moreover indicates whether the judgement has a
default value, and whether it contributes to the **name base**, i.e.
introduces a new name to the scope.

  judgement                     where                        module       default   base
  ----------------------------- ---------------------------- ------------ --------- ------
  `cat` C G                     G context                    abstract     N/A       yes
  `fun` f : A                   A type                       abstract     N/A       yes
  `def` f ps = t                f fun, ps patterns, t term   abstract     yes       no
  `data` C = f `|` \... `|` g   C cat, f\...g fun            abstract     yes       no
  `lincat` C = T                C cat, T type                concrete\*   yes       yes
  `lin` f = t                   f fun, t term                concrete\*   no        yes
  `lindef` C = t                C cat, t term                concrete\*   yes       no
  `linref` C = t                C cat, t term                concrete\*   yes       no
  `printname cat` C = t         C cat, t term                concrete\*   yes       no
  `printname fun` f = t         f fun, t term                concrete\*   yes       no
  `param` P = C`|` \... `|` D   C\...D constructors          resource\*   N/A       yes
  `oper` f : T = t              T type, t term               resource\*   N/A       yes
  `flags` o = v                 o flag, v value              all          yes       N/A

Judgements that have default values are rarely used, except `lincat` and
`flags`, which often need values different from the defaults.

Introducing a name twice in the same module is an error. In other words,
all judgements that have a \"yes\" in the name base column, must have
distinct identifiers on their left-hand sides.

All judgement end with semicolons (`;`).

In addition to the syntax given in the table, many of the forms have
syntactic sugar. This sugar will be explained below in connection to
each form. There are moreover two kinds of syntactic sugar common to all
forms:

-   the judgement keyword is shared between consecutive judgements until
    a new keyword appears:
    `keyw J ; K ;` === `keyw J ; keyw K ;`
-   the right-hand sides of colon (`:`) and equality (`=`) can be
    shared, by using comma (`,`) as separator of left-hand sides, which
    must consist of identifiers
    `c,d : T` === `c : T ; d : T ;`
    `c,d = t` === `c = t ; d = t ;`

These conventions, like all syntactic sugar, are performed at an early
compilation phase, directly after parsing. This means that e.g.

        lin f,g = \x -> x ;

can be correct even though `f` and `g` required different function
types.

Within a module, judgements can occur in any order. In particular, a
name can be used before it is introduced.

The explanations of judgement forms refer to the notions of **type** and
**term** (the latter also called **expression**). These notions will be
explained in detail [here](#expressions).


### Category declarations, cat

[]{#catjudgements}

Category declarations

`cat` *C* *G*

define the **basic types** of abstract syntax. A basic type is formed
from a category by giving values to all variables in the **context**
*G*. If the context is empty, the basic type looks the same as the
category itself. Otherwise, application syntax is used:

*C* *a*~1~\...*a*~n~


### Hypotheses and contexts

[]{#contexts}

A context is a sequence of **hypotheses**, i.e. variable-type pairs. A
hypothesis is written

`(` *x* `:` *T* `)`

and a sequence does not have any separator symbols. As syntactic sugar,

-   variables can share a type,
    `(` *x,y* `:` *T* `)` === `(` *x* `:` *T* `)` `(` *y* `:` *T* `)`
-   a **wildcard** can be used for a variable not occurring in types
    later in the context,
    `(` `_` `:` *T* `)` === `(` *x* `:` *T* `)`
-   if the variable does not occur later, it can be omitted altogether,
    and parentheses are not used,
    *T* === `(` *x* `:` *T* `)`
    But if *T* is more complex than an identifier, it needs parentheses
    to be separated from the rest of the context.

An abstract syntax has **dependent types**, if any of its categories has
a non-empty context.


### Function declarations, fun

Function declarations,

`fun` *f* `:` *T*

define the **syntactic constructors** of abstract syntax. The type *T*
of *f* is built built from basic types (formed from categories) by using
the function type constructor `->`. Thus its form is

(*x*~1~ `:` *A*~1~) `->` \... `->` (*x*~n~ `:` *A*~n~) `->` *B*

where *Ai* are types, called the **argument types**, and *B* is a basic
type, called the **value type** of *f*. The **value category** of *f* is
the category that forms the type *B*.

A **syntax tree** is formed from *f* by applying it to a full list of
arguments, so that the result is of a basic type.

A **higher-order function** is one that has a function type as an
argument. The concrete syntax of GF does not support displaying the
bound variables of functions of higher than second order, but they are
legal in abstract syntax.

An abstract syntax is **context-free**, if it has neither dependent
types nor higher-order functions. Grammars with context-free abstract
syntax are an important subclass of GF, with more limited complexity
than full GF. Whether the *concrete* syntax is context-free in the sense
of the Chomsky hierarchy is independent of the context-freeness of the
abstract syntax.


### Function definitions, def

Function definitions,

`def` *f* *p*~1~ \... *p*~n~ `=` *t*

where *f* is a `fun` function and *p*~i~\# are patterns, impose a
relation of **definitional equality** on abstract syntax trees. They
form the basis of **computation**, which is used when comparing whether
two types are equal; this notion is relevant only if the types are
dependent. Computation can also be used for the **normalization** of
syntax trees, which applies even in context-free abstract syntax.

The set of `def` definitions for *f* can be scattered around the module
in which *f* is introduced as a function. The compiler builds the set of
pattern equations in the order in which the equations appear; this order
is significant in the case of overlapping patterns. All equations must
appear in the same module in which *f* itself declared.

The syntax of patterns will be specified [here](#patternmatching),
commonly for abstract and concrete syntax. In abstract syntax,
**constructor patterns** are those of the form

*C* *p*~1~ \... *p*~n~

where *C* is declared as `data` for some abstract syntax category (see
next section). A **variable pattern** is either an identifier or a
wildcard.

A common pitfall is to forget to declare a constructor as data, which
causes it to be interpreted as a variable pattern in definitions.

Computation is performed by applying definitions and beta conversions,
and in general by using **pattern matching**. Computation and pattern
matching are explained commonly for abstract and concrete syntax
[here](#patternmatching).

In contrast to concrete syntax, abstract syntax computation is
completely **symbolic**: it does not produce a value, but just another
term. Hence it is not an error to have incomplete systems of pattern
equations for a function. In addition, the definitions can be
**recursive**, which means that computation can fail to terminate; this
can never happen in concrete syntax.


### Data constructor definitions, data

A data constructor definition,

`data` *C* `=` *f*~1~ `|` \... `|` *f*~n~

defines the functions *f1*\...*fn* to be **constructors** of the
category *C*. This means that they are recognized as constructor
patterns when used in function definitions.

In order for the data constructor definition to be correct,
*f*~1~\...*f*~n~ must be functions with *C* as their value category.

The complete set of constructors for a category *C* is the union of all
its data constructor definitions. Thus a category can be \"extended\" by
new constructors afterwards. However, all these constructor definitions
must appear in the same module in which the category is itself defined.

There is syntactic sugar for declaring a function as a constructor at
the same time as introducing it:

`data` *f* : *A*~1~ `->` \... `->` *A*~n~ `->` *C* *t*~1~ \... *t*~m~

===

`fun` *f* : *A*~1~ `->` \... `->` *A*~n~ `->` *C* *t*~1~ \... *t*~m~ ;
`data` *C* = *f*


### The semantic status of an abstract syntax function

There are three possible statuses for a function declared in a `fun`
judgement:

-   primitive notion: the default status
-   constructor: the function appears on the right-hand side in `data`
    judgement
-   defined: the function has a `def` definition

The \"constructor\" and \"defined\" statuses are in contradiction with
each other, whereas the primitive notion status is overridden by any of
the two others.

This distinction is relevant for the semantics of abstract syntax, not
for concrete syntax. It shows in the way patterns are treated in
equations in `def` definitions: a constructor in a pattern matches only
itself, whereas any other name is treated as a variable pattern, which
matches anything.


### Linearization type definitions, lincat

A linearization type definition,

`lincat` *C* `=` *T*

defines the type of linearizations of trees whose type has category *C*.
Type dependences have no effect on the linearization type.

The type *T* must be a **legal linearization type**, which means that it
is a *record type* whose fields have either parameter types, the type
Str of strings, or table or record types of these. In particular,
function types may not appear in *T*. A detailed explanation of types in
concrete syntax will be given [here](#cnctypes).

If *K* is the concrete syntax of an abstract syntax *A*, then *K* must
define the linearization type of all categories declared in *A*.
However, the definition can be omitted from the source code, in which
case the default type `{s : Str}` is used.


### Linearization definitions, lin

A linearization definition,

`lin` *f* `=` *t*

defines the linearizations function of function *f*, i.e. the function
used for linearizing trees formed by *f*.

The type of *t* must be the homomorphic image of the type of *f*. In
other words, if

`fun` *f* `:` *A*~1~ `->` \... `->` *A*~n~ `->` *A*

then

`lin` *f* `:` *A*~1~\* `->` \... `->` *A*~n~\* `->` *A*\*

where the type *T*\* is defined as follows depending on *T*:

-   (*C* *t*~1~ \... *t*~n~)\* = *T*, if `lincat` *C* `=` *T*
-   (*B*~1~ `->` \... `->` *B*~m~ `->` *B*)\* = *B*\*
    `** {$0,...,$m : Str}`

The second case is relevant for higher-order functions only. It says
that the linearization type of the value type is extended by adding a
string field for each argument types; these fields store the variable
symbol used for the binding of each variable.

[]{#HOAS}

Since the arguments of a function argument are treated as bare strings,
orders higher than the second are irrelevant for concrete syntax.

There is syntactic sugar for binding the variables of the linearization
of a function on the left-hand side:

`lin` *f* *p* `=` *t* === `lin` *f* `= \`*p* `->` *t*

The pattern *p* must be either a variable or a wildcard (`_`); this is
what the syntax of lambda abstracts (`\p -> t`) requires.


### Linearization default definitions, lindef

[]{#lindefjudgements}

A linearization default definition,

`lindef` *C* `=` *t*

defines the default linearization of category *C*, i.e. the function
applicable to a string to make it into an object of the linearization
type of *C*.

Linearization defaults are invoked when linearizing variable bindings in
higher-order abstract syntax. A variable symbol is then presented as a
string, which must be converted to correct type in order for the
linearization not to fail with an error.

The other use of the defaults is for linearizing metavariables and
abstract functions without linearization in the concrete syntax. In the
first case the default linearization is applied to the string `"?X"`
where `X` is the unique index of the metavariable, and in the second
case the string is `"[f]"` where `f` is the name of the abstract
function with missing linearization.

Usually, linearization defaults are generated by using the default rule
that \"uses the symbol itself for every string, and the first value of
the parameter type for every parameter\". The precise definition is by
structural recursion on the type:

-   default(Str,s) = s
-   default(P,s) = \#1(P)
-   default(P =\> T,s) = `\\_ =>` default(T,s)
-   default(`{`\... ; r : R ; \...`}`,s) = `{`\... ; r : default(R,s) ;
    \...`}`

The notion of the first value of a parameter type (\#1(P)) is defined
[below](#paramvalues).


### Linearization reference definitions, linref

[]{#linrefjudgements}

A linearization reference definition,

`linref` *C* `=` *t*

defines the reference linearization of category *C*, i.e. the function
applicable to an object of the linearization type of *C* to make it into
a string.

The reference linearization is always applied to the top-level node of
the abstract syntax tree. For example when we linearize the tree
`f x1 x2 .. xn`, then we first apply `f` to its arguments which gives us
an object of the linearization type of its category. After that we apply
the reference linearization for the same category to get a string out of
the object. This is particularly useful when the linearization type of
*C* contains discontious constituents. In this case usually the
reference linearization glues the constituents together to produce an
intuitive linearization string.

The reference linearization is also used for linearizing metavariables
which stand in function position. For example the tree
`f (? x1 x2 .. xn)` is linearized as follows. Each of the arguments
`x1 x2 .. xn` is linearized, and after that the reference linearization
of the its category is applied to the output of the linearization. The
result is a sequence of `n` strings which are concatenated into a single
string. The final string is the input to the default linearization of
the category for the argument of `f`. After applying the default
linearization we get an object that we could safely pass to `f`.

Usually, linearization references are generated by using the rule that
\"picks the first string in the linearization type\". The precise
definition is by structural recursion on the type:

-   reference(Str,o) = Just o
-   reference(P,s) = Nothing
-   reference(P =\> T,o) = reference(T,o ! \#1(P)) \|\| reference(T,o !
    \#2(P)) \|\| \... \|\| reference(T,o ! \#n(P))
-   reference({r1 : R1; \... rn : Rn},o) = reference(R1, o.r1) \|\|
    reference(R2, o.r2) \|\| \... \|\| reference(Rn, o.rn)

Here each call to reference returns either `(Just o)` or `Nothing`. When
we compute the reference for a table or a record then we pick the
reference for the first expression for which the recursive call gives us
`Just`. If we get `Nothing` for all of them then the final result is
`Nothing` too.


### Printname definitions, printname cat and printname fun

A category printname definition,

`printname cat` *C* `=` *s*

defines the printname of category *C*, i.e. the name used in some
abstract syntax information shown to the user.

Likewise, a function printname definition,

`printname fun` *f* `=` *s*

defines the printname of function *f*, i.e. the name used in some
abstract syntax information shown to the user.

The most common use of printnames is in the interactive syntax editor,
where printnames are displayed in menus. It is possible e.g. to adapt
them to each language, or to embed HTML tooltips in them (as is used in
some HTML-based editor GUIs).

Usually, printnames are generated automatically from the symbol and/or
concrete syntax information.


### Parameter type definitions, param

[]{#paramjudgements}

A parameter type definition,

`param` *P* `=` *C*~1~ *G*~1~ `|` \... `|` *C*~n~ *G*~n~

defines a parameter type *P* with the **parameter constructors**
*C*~1~\...*C*~n~, with their respective contexts *G*~1~\...*G*~n~.

[]{#paramtypes}

Contexts have the same syntax as in `cat` judgements, explained
[here](#catjudgements). Since dependent types are not available in
parameter type definitions, the use of variables is never necessary. The
types in the context must themselves be **parameter types**, which are
defined as follows:

-   Given the judgement `param` *P* \..., *P* is a parameter type.
-   A record type of parameter types is a parameter type.
-   `Ints` *n* (an initial segment of integers) is a parameter type.

The names defined by a parameter type definition include both the type
name *P* and the constructor names *C*~i~. Therefore all these names
must be distinct in a module.

A parameter type may not be recursive, i.e. *P* itself may not occur in
the contexts of its constructors. This restriction extends to mutual
recursion: we say that *P* **depends** on the types that occur in the
contexts of its constructors and on all types that those types depend
on, and state that *P* may not depend on itself.

In an `interface module`, it is possible to declare a parameter type
without defining it,

`param` *P* `;`


### Parameter values

[]{#paramvalues}

All parameter types are finite, and the GF compiler will internally
compute them to **lists of parameter values**. These lists are formed by
traversing the `param` definitions, usually respecting the order of
constructors in the source code. For records, bibliographical sorting is
applied. However, both the order of traversal of `param` definitions and
the order of fields in a record are specified in a compiler-internal
way, which means that the programmer should not rely on any particular
order.

The order of the list of parameter values can affect the program in two
cases:

-   in the default `lindef` definition ([here](#lindefjudgements)), the
    first value is chosen
-   in course-of-value tables ([here](#tables)), the compiler-internal
    order is followed

The first usage implies that, if `lindef` definitions are essential for
the application, they should be given manually. The second usage implies
that course-of-value tables should be avoided in hand-written GF code.

In run-time grammar generation, all parameter values are translated to
integers denotions positions in these parameter lists.


### Operation definitions, oper

An operation definition,

`oper` *h* `:` *T* `=` *t*

defines an **operation** *h* of type *T*, with the computation rule

*h* ==\> *t*

The type *T* can be any concrete syntax type, including function types
of any order. The term *t* must have the type *T*, as defined
[here](#expressions).

As syntactic sugar, the type can be omitted,

`oper` *h* `=` *t*

which works in two cases

-   the type can be inferred from *t* (compiler-dependent)
-   the definition occurs in an `instance` and the type is given in the
    `interface`

It is also possible to give the type and the definition separately:

`oper` *h* `:` *T* ; `oper` *h* `=` *t* === `oper` *h* `:` *T* `=` *t*

The order of the type part and the definition part is free, and there
can be other judgements in between. However, they must occur in the same
`resource` module for it to be complete (as defined
[here](#completeness)). In an `interface` module, it is enough to give
the type.

When only the definition is given, it is possible to use a shorthand
similar to `lin` judgements:

`oper` *h* *p* `=` *t* === `oper` *h* `=` `\`*p* `->` *t*

The pattern *p* is either a variable or a wildcard (`_`).

Operation definitions may not be recursive, not even mutually recursive.
This condition ensures that functions can in the end be eliminated from
concrete syntax code (as explained [here](#functionelimination)).


### Operation overloading

[]{#overloading}

One and the same operation name *h* can be used for different
operations, which have to have different types. For each call of *h*,
the type checker selects one of these operations depending on what type
is expected in the context of the call. The syntax of overloaded
operation definitions is

`oper` *h* `= overload {`*h* : *T*~1~ = *t*~1~ ; \... ; *h* : *T*~n~ =
*t*~n~`}`

Notice that *h* must be the same in all cases. This format can be used
to give the complete implementation; to give just the types, e.g. in an
interface, one can use the form

`oper` *h* `: overload {`*h* : *T*~1~ ; \... ; *h* : *T*~n~`}`

The implementation of this operation typing is given by a judgement of
the first form. The order of branches need not be the same.


### Flag definitions, flags

A flag definition,

`flags` *o* `=` *v*

sets the value of the flag *o*, to be used when compiling or using the
module.

The flag *o* is an identifier, and the value *v* is either an identifier
or a quoted string.

Flags are a kind of metadata, which do not strictly belong to the GF
language. For instance, compilers do not necessarily check the
consistency of flags, or the meaningfulness of their values. The
inheritance of flags is not well-defined; the only certain rule is that
flags set in the module body override the settings from inherited
modules.

Here are some flags commonly included in grammars.

  flag         value                description                        module
  ------------ -------------------- ---------------------------------- ----------
  `coding`     character encoding   encoding used in string literals   concrete
  `startcat`   category             default target of parsing          abstract

The possible values of these flags are specified [here](#flagvalues).
Note that the `lexer` and `unlexer` flags are deprecated. If you need
their functionality, you should use supply them to GF shell commands
like so:

    put_string -lextext "страви, напої" | parse

A summary of their possible values can be found at the [GF shell
reference](http://www.grammaticalframework.org/doc/gf-shell-reference.html).


Types and expressions
---------------------


### Overview of expression forms

[]{#expressions}

Like many dependently typed languages, GF makes no syntactic distinction
between expressions and types. An illegal use of a type as an expression
or vice versa comes out as a type error. Whether a variable, for
instance, stands for a type or an expression value, can only be resolved
from its context of use.

One practical consequence of the common syntax is that global and local
definitions (`oper` judgements and `let` expressions, respectively) work
in the same way for types and expressions. Thus it is possible to
abbreviate a type occurring in a type expression:

        let A = {s : Str ; b : Bool} in A -> A -> A

Type and other expressions have a system of **precedences**. The
following table summarizes all expression forms, from the highest to the
lowest precedence. Some expressions are moreover left- or
right-associative.

  prec      expression example                   explanation
  --------- ------------------------------------ -----------------------------------
  7         `c`                                  constant or variable
  7         `Type`                               the type of types
  7         `PType`                              the type of parameter types
  7         `Str`                                the type of strings/token lists
  7         `"foo"`                              string literal
  7         `123`                                integer literal
  7         `0.123`                              floating point literal
  7         `?`                                  metavariable
  7         `[]`                                 empty token list
  7         `[C a b]`                            list category
  7         `["foo bar"]`                        token list
  7         `{"s : Str ; n : Num}`               record type
  7         `{"s = "foo" ; n = Sg}`              record
  7         `<Sg,Fem,Gen>`                       tuple
  7         `<n : Num>`                          type-annotated expression
  6 left    `t.r`                                projection or qualification
  5 left    `f a`                                function application
  5         `table {Sg => [] ; _ => "xs"}`       table
  5         `table P [a ; b ; c]`                course-of-values table
  5         `case n of {Sg => [] ; _ => "xs"}`   case expression
  5         `variants {"color" ; "colour"}`      free variation
  5         `pre {vowel => "an" ; _ => "a"}`     prefix-dependent choice
  4 left    `t ! v`                              table selection
  4 left    `A * B`                              tuple type
  4 left    `R ** {b : Bool}`                    record (type) extension
  3 left    `t + s`                              token gluing
  2 left    `t ++ s`                             token list concatenation
  1 right   `\x,y -> t`                          function abstraction (\"lambda\")
  1 right   `\\x,y => t`                         table abstraction
  1 right   `(x : A) -> B`                       dependent function type
  1 right   `A -> B`                             function type
  1 right   `P => T`                             table type
  1 right   `let x = v in t`                     local definition
  1         `t where {x = v}`                    local definition
  1         `in M.C "foo"`                       rule by example

Any expression in parentheses (`(`*exp*`)`) is in the highest precedence
class.


### The functional fragment: expressions in abstract syntax

[]{#functiontype}

The expression syntax is the same in abstract and concrete syntax,
although only a part of the syntax is actually usable in well-typed
expressions in abstract syntax. An abstract syntax is essentially used
for defining a set of types and a set of functions between those types.
Therefore it needs essentially the **functional fragment** of the
syntax. This fragment comprises two kinds of types:

-   **basic types**, of form *C a1\...an* where
    -   `cat` *C* (*x*~1~ : *A*~1~)\...(*x*~n~ : *A*~n~), including the
        predefined categories `Int`, `Float`, and `String` explained
        [here](#predefabs)
    -   *a*~1~ : *A*~1~,\...,*a*~n~ : *A*~n~{*x*~1~ =
        *a*~1~,\...,*x*~n-1~=*a*~n-1~}

<!-- -->

-   **function types**, of form (*x* : *A*) `->` *B*, where
    -   *A* is a type
    -   *B* is a type possibly depending on *x* : *A*

When defining basic types, we used the notation *t*{*x*~1~ =
*t*~1~,\...,*x*~n~=*t*~n~} for the **substitution** of values to
variables. This is a metalevel notation, which denotes a term that is
formed by replacing the free occurrences of each variable *x*~i~ by
*t*~i~.

These types have six kinds of expressions:

-   **constants**, *f* : *A* where
    -   `fun` *f* : *A*

<!-- -->

-   **literals** for integers, floats, and strings (defined in
    [here](#predefabs))

<!-- -->

-   **variables**, *x* : *A* where
    -   *x* has been introduced by a binding

<!-- -->

-   **applications**, *f a* : *B*{*x*=*a*}, where
    -   *f* : (*x* : *A*) `->` *B*
    -   *a* : *A*

<!-- -->

-   **abstractions**, `\`*x* `->` *b* : (*x* : *A*) `->` *B*, where
    -   *b* : *B* possibly depending on *x* : *A*

<!-- -->

-   **metavariables**, `?`, as introduced in intermediate phases of
    incremental type checking; metavariables are not permitted in GF
    source code

[]{#variablebinding}

The notion of **binding** is defined for occurrences of variables in
subexpressions as follows:

-   in (*x* : *A*) `->` *B*, *x* is bound in *B*
-   in `\`*x* `->` *b*, *x* is bound in *b*
-   in `def` *f* *p*~1~ \... *p*~n~ = *t*, any pattern variable
    introduced in any *pi* is bound in *t* (as defined
    [here](#patternmatching))

As syntactic sugar, function types have sharing of types and suppression
of variables, in the same way as contexts (defined [here](#contexts)):

-   variables can share a type,
    `(` *x,y* `:` *A* `)` `->` *B* === `(` *x* `:` *A* `) -> (` *y* `:`
    *A* `) ->` *B*
-   a **wildcard** can be used for a variable not occurring later in the
    type,
    `(` `_` `:` *A* `) ->` *B* === `(` *x* `:` *T* `) ->` *B*
-   if the variable does not occur later, it can be omitted altogether,
    and parentheses are not used,
    *A* `->` *B* === `(` *\_* `:` *A* `) ->` *B*

There is analogous syntactic sugar for constant functions,

`\`*\_* `->` *t* === `\`*x* `->` *t*

where *x* does not occur in *t*, and for multiple lambda abstractions:

`\`*p,q* `->` *t* === `\`*p* `->` `\`*q* `->` *t*

where *p* and *q* are variables or wild cards (`_`).


### Conversions

Among expressions, there is a relation of **definitional equality**
defined by four **conversion rules**:

-   **alpha conversion**: `\`*x* `->` *b* = `\`*y* `->` *b*{*x*=*y*}

<!-- -->

-   **beta conversion**: (`\`*x* `->` *b*) *a* = *b*{*x*=*a*}

<!-- -->

-   **delta conversion**: *f* *a*~1~ \... *a*~n~ = *tg*, if
    -   there is a definition `def` *f* *p*~1~ \... *p*~n~ = *t*
    -   this definition is the first for *f* that matches the sequence
        *a*~1~ \.... *a*~n~, with the substitution *g*

<!-- -->

-   **eta conversion**: *c* = `\`*x* `->` *c x*, if *c* : (*x* : *A*)
    `->` *B*

Pattern matching substitution used in delta conversion is defined
[here](#patternmatching).

An expression is in **beta-eta-normal form** if

-   it has no subexpressions to which beta conversion applies (beta
    normality)
-   each constant or variable whose type is a function type must be
    **eta-expanded**, i.e. made into an abstract equal to it by eta
    conversion (eta normality)

Notice that the iteration of eta expansion would lead to an expression
not in beta-normal form.


### Syntax trees

[]{#syntaxtrees}

The **syntax trees** defined by an abstract syntax are well-typed
expressions of basic types in beta-eta normal form. Linearization
defined in concrete syntax applies to all and only these expressions.

There is also a direct definition of syntax trees, which does not refer
to beta and eta conversions: keeping in mind that a type always has the
form

(*x*~1~ : *A*~1~) `->` \... `->` (*x*~n~ : *A*~n~) `->` *B*

where *Ai* are types and *B* is a basic type, a syntax tree is an
expression

*b* *t*~1~ \... *t*~n~ : *B\'*

where

-   *B\'* is the basic type *B*{*x*~1~ = *t*~1~,\...,*x*~n~ = *t*~n~}
-   `fun` *b* : (*x*~1~ : *A*~1~) `->` \... `->` (*x*~n~ : *A*~n~) `->`
    *B*
-   each *t*~i~ has the form `\`*z*~1~,\...,*z*~m~ `->` *c* where *A*~i~
    is
    (*y*~1~ : *B*~1~) `->` \... `->` (*y*~m~ : *B*~m~) `->` *B*


### Predefined types in abstract syntax

[]{#predefabs}

GF provides three predefined categories for abstract syntax, with
predefined expressions:

   category  expressions
  ---------- ---------------------------------------
    `Int`    integer literals, e.g. `123`
   `Float`   floating point literals, e.g. `12.34`
   `String`  string literals, e.g. `"foo"`

These categories take no arguments, and they can be used as basic types
in the same way as if they were introduced in `cat` judgements. However,
it is not legal to define `fun` functions that have any of these types
as value type: their only well-typed expressions are literals as defined
in the above table.


### Overview of expressions in concrete syntax

[]{#cnctypes}

Concrete syntax is about defining mappings from abstract syntax trees to
**concrete syntax objects**. These objects comprise

-   records
-   tables
-   strings
-   parameter values

Thus functions are not concrete syntax objects; however, the mappings
themselves are expressed as functions, and the source code of a concrete
syntax can use functions under the condition that they can be eliminated
from the final compiled grammar (which they can; this is one of the
fundamental properties of compilation, as explained in more detail in
the *JFP* article).

Concrete syntax thus has the same function types and expression forms as
abstract syntax, specified [here](#functiontype). The basic types
defined by categories (`cat` judgements) are available via grammar reuse
explained [here](#reuse); this also comprises the predefined categories
`Float` and `String`.


### Values, canonical forms, and run-time variables

In abstract syntax, the conversion rules fiven [here](#conversions)
define a computational relation among expressions, but there is no
separate notion of a **value** of computation: the value (the end point)
of a computation chain is simply an expression to which no more
conversions apply. In general, we are interested in expressions that
satisfy the conditions of being syntax trees (as defined
[here](#syntaxtrees)), but there can be many computationally equivalent
syntax trees which nonetheless are distinct syntax trees and hence have
different linearizations. The main use of computation in abstract syntax
is to compare types in dependent type checking.

In concrete syntax, the notion of values is central. At run time, we
want to compute the values of linearizations; at compile time, we want
to perform **partial evaluation**, which computes expressions as far as
possible. To specify what happens in computation we therefore have to
distinguish between **canonical forms** and other forms of expressions.
The canonical forms are defined separately for each form of type,
whereas the other forms may usually produce expressions of any type.

[]{#linexpansion} []{#runtimevariables}

What is done at compile time is the elimination of any noncanonical
forms, except for those depending on **run-time variables**. Run-time
variables are the same as the **argument variables** of linearization
rules, i.e. the variables *x*~1~,\...,*x*~n~ in

`lin` *f* `= \` *x*~1~,\...,*x*~n~ `->` *t*

where

`fun` *f* `:` (*x*~1~ : *A*~1~) `->` \... `->` (*x*~n~ : *A*~n~) `->`
*B*

Notice that this definition refers to the **eta-expanded** linearization
term, which has one abstracted variable for each argument type of *f*.
These variables are not necessarily explicit in GF source code, but
introduced by the compiler.

Since certain expression forms should be eliminated in compilation but
cannot be eliminated if run-time variables appear in them, errors can
appear late in compilation. This is an issue with the following
expression forms:

-   gluing (`s + t`), defined [here](#gluing)
-   pattern matching on strings, defined [here](#patternmatching)
-   predefined string operations, defined [here](#predefcnc) (those
    taking `Str` arguments)


### Token lists, tokens, and strings

[]{#strtype}

The most prominent basic type is `Str`, the type of **token lists**.
This type is often sloppily referred to as the type of **strings**; but
it should be kept in mind that the objects of `Str` are *lists* of
strings rather than single strings.

Expressions of type `Str` have the following canonical forms:

-   **tokens**, i.e. **string literals**, in double quotes, e.g. `"foo"`
-   **the empty token list**, `[]`
-   **concatenation**, *s* `++` *t*, where *s,t* : `Str`
-   **prefix-dependent choice**,
    `pre {p1 => s1 ; ... ; pn => sn ; _ => s }`, where
    -   *s*, *s*~1~,\...,*s*~n~, *p*~1~,\...,*p*~n~ : `Str`

For convenience, the notation is overloaded so that tokens are
identified with singleton token lists, and there is no separate type of
tokens (this is a change from the *JFP* article). The notion of a token
is still important for compilation: all tokens introduced by the grammar
must be known at compile time. This, in turn, is required by the parsing
algorithms used for parsing with GF grammars.

In addition to string literals, tokens can be formed by a specific
non-canonical operator:

-   **gluing**, *s* `+` *t*, where *s,t* : `Str`

[]{#gluing}

Being noncanonical, gluing is equipped with a computation rule: string
literals are glued by forming a new string literal, and empty token
lists can be ignored:

-   `"foo" + "bar"` ==\> `"foobar"`
-   *t* `+ []` ==\> *t*
-   `[] +` *t* ==\> *t*

Since tokens must be known at compile time, the operands of gluing may
not depend on run-time variables, as defined [here](#runtimevariables).

As syntactic sugar, token lists can be given as bracketed string
literals, where spaces separate tokens:

-   **token lists**, `["one two three"]` === `"one" ++ "two" ++ "three"`

Notice that there are no empty tokens, but the expression `[]` can be
used in a context requiring a token, in particular in gluing expression
below. Since `[]` denotes an empty token list, the following computation
laws are valid:

-   *t* `++ []` ==\> *t*
-   `[] ++` *t* ==\> *t*

Moreover, concatenation and gluing are associative:

-   s `+` (t `+` u) ==\> s `+` t `+` u
-   s `++` (t `++` u) ==\> s `++` t `++` u

For the programmer, associativity and the empty token laws mean that the
compiler can use them to simplify string expressions. It also means that
these laws are respected in pattern matching on strings.

A prime example of prefix-dependent choice operation is the following
approximative expression for the English indefinite article:

      pre {
        ("a" | "e" | "i" | "o") => "an" ;
        _ => "a"
      } ;

This expression can be computed in the context of a subsequent token:

-   `pre {p1 => s1 ; ... ; pn => sn ; _ => s } ++ t` ==\>
    -   *s*~i~ for the first *i* such that the prefix *p*~i~ matches
        *t*, if it exists
    -   *s* otherwise

The **matching prefix** is defined by comparing the string with the
prefix of the token. If the prefix is a variant list of strings, then it
matches the token if any of the strings in the list matches it.

The computation rule can sometimes be applied at compile time, but it
general, prefix-dependent choices need to be passed to the run-time
grammar, because they are not given a subsequent token to compare with,
or because the subsequent token depends on a run-time variable.

The prefix-dependent choice expression itself may not depend on run-time
variables.

*There is an older syntax for prefix-dependent choice, namely:
`pre { s ; s1 / p1 ; ... ; sn / pn}`. This syntax will not accept
strings as patterns.*

*In GF prior to 3.0, a specific type* `Strs` *is used for defining
prefixes,* *instead of just* `variants` *of* `Str`.


### Records and record types

A **record** is a collection of objects of possibly different types,
accessible by **projections** from the record with **labels** pointing
to these objects. A record is also itself an object, whose type is a
**record type**. Record types have the form

`{` *r*~1~ : *A*~1~ `;` \... `;` *r*~n~ : *A*~n~ `}`

where *n* \>= 0, each *A*~i~ is a type, and the labels *r*~i~ are
distinct. A record of this type has the form

`{` *r*~1~ = *a*~1~ `;` \... `;` *r*~n~ = *a*~n~ `}`

where each \#aii : \"Aii. A limiting case is the **empty record type**
`{}`, which has the object `{}`, the **empty record**.

The **fields** of a record type are its parts of the form *r* : *A*,
also called **typings**. The **fields** of a record are of the form *r*
= *a*, also called **value assignments**. Value assignments may
optionally indicate the type, as in *r* : *A* = *a*.

The order of fields in record types and records is insignificant: two
record types (or records) are equal if they have the same fields, in any
order, and a record is an object of a record type, if it has
type-correct value assignments for all fields of the record type. The
latter definition implies the even stronger principle of **record
subtyping**: a record can have any type that has some subset of its
fields. This principle is explained further [here](#subtyping).

All fields in a record must have distinct labels. Thus it is not
possible e.g. to \"redefine\" a field \"later\" in a record.

Lexically, labels are identifiers (defined [here](#identifiers)). This
is with the exception of the labels selecting bound variables in the
linearization of higher-order abstract syntax, which have the form
`$`*i* for an integer *i*, as specified [here](#HOAS). In source code,
these labels should not appear in records fields, but only in
selections.

Labels occur only in syntactic positions where they cannot be confused
with constants or variables. Therefore it is safe to write, as in
`Prelude`,

        ss : Str -> {s : Str} = \s -> {s = s} ;

A **projection** is an expression of the form

*t*.*r*

where *t* must be a record and *r* must be a label defined in it. The
type of the projection is the type of that field. The computation rule
for projection returns the value assigned to that field:

`{` \... `;` *r* = *a* `;` \... `}.`*r* ==\> *a*

Notice that the dot notation *t*.*r* is also used for qualified names as
specified [here](#qualifiednames). This ambiguity follows tradition and
convenience. It is resolved by the following rules (before type
checking):

1.  if *t* is a bound variable or a constant in scope, *t*.*r* is
    type-checked as a projection
2.  otherwise, *t*.*r* is type-checked as a qualified name

As syntactic sugar, types and values can be shared:

-   `{` \... `;` *r,s* : *A* `;` \... `}` === `{` \... `;` *r* : *A* `;`
    *s* : *A* `;` \... `}`
-   `{` \... `;` *r,s* = *a* `;` \... `}` === `{` \... `;` *r* = *a* `;`
    *s* = *a* `;` \... `}`

Another syntactic sugar are **tuple types** and **tuples**, which are
translated by endowing their unlabelled fields by the labels `p1`,
`p2`,\... in the order of appearance of the fields:

-   *A*~1~ `*` \... `*` *A*~n~ === `{` `p1` : *A*~1~ `;` \... `;` `pn` :
    *A*~n~ `}`
-   `<`*a*~1~ `,` \... `,` *a*~n~ `>` === `{` `p1` = *a*~1~`;` \... `;`
    `pn` = *a*~n~ `}`

A **record extension** is formed by adding fields to a record or a
record type. The general syntax involves two expressions,

*R* `**` *S*

The result is a record type or a record with a union of the fields of
*R* and *S*. It is therefore well-formed if

-   both *R* and *S* are either records or record types
-   the labels in *R* and *S* are disjoint, if *R* and *S* are record
    types

(Since GF version 3.6) If *R* and *S* are record objects, then the
labels in them need not be disjoint. Labels defined in *S* are then
given priority, so that record extensions in fact works as **record
update**. A common pattern of using this feature is

      lin F x ... = x ** {r = ... x.r ...}

where `x` is a record with many fields, just one of which is updated.
Following the normal binding conditions, `x.r` on the right hand side
still refers to the old value of the `r` field.


### Subtyping

The possibility of having superfluous fields in a record forms the basis
of the **subtyping** relation. That *A* is a subtype of *B* means that
*a : A* implies *a : B*. This is clearly satisfied for records with
superfluous fields:

-   if *R* is a record type without the label *r*, then *R* `** {` *r* :
    *A* `}` is a subtype of *R*

The GF grammar compiler extends subtyping to function types by
**covariance** and **contravariance**:

-   covariance: if *A* is a subtype of *B*, then *C* `->` *A* is a
    subtype of *C* `->` *B*
-   contravariance: if *A* is a subtype of *B*, then *B* `->` *C* is a
    subtype of *A* `->` *C*

The logic of these rules is natural: if a function is returns a value in
a subtype, then this value is *a fortiori* in the supertype. If a
function is defined for some type, then it is *a fortiori* defined for
any subtype.

In addition to the well-known principles of record subtyping and co- and
contravariance, GF implements subtyping for initial segments of
integers:

-   if *m* \< *n*, then `Ints` *m* is a subtype of `Ints` *n*
-   `Ints` *n* is a subtype of `Integer`

As the last rule, subtyping is transitive:

-   if *A* is a subtype of *B* and *B* is a subtype of *C*, then *A* is
    a subtype of *C*.


### Tables and table types

[]{#tables}

One of the most characteristic constructs of GF is **tables**, also
called **finite functions**. That these functions are finite means that
it is possible to finitely enumerate all argument-value pairs; this, in
turn, is possible because the argument types are finite.

A **table type** has the form

*P* `=>` *T*

where *P* must be a parameter type in the sense defined
[here](#paramtypes), whereas *T* can be any type.

Canonical expressions of table types are **tables**, of the form

`table` `{` *V*~1~ `=>` *t*~1~ ; \... ; *V*~n~ `=>` *t*~n~ `}`

where *V*~1~,\...,*V*~n~ is the complete list of the parameter values of
the argument type *P* (defined [here](#paramvalues)), and each *t*~i~ is
an expression of the value type *T*.

In addition to explicit enumerations, tables can be given by **pattern
matching**,

`table` `{`*p*~1~ `=>` *t*~1~ ; \... ; *p*~m~ `=>` *t*~m~`}`

where *p*~1~,\....,*p*~m~ is a list of patterns that covers all values
of type *P*. Each pattern *p*~i~ may bind some variables, on which the
expression *t*~i~ may depend. A complete account of patterns and pattern
matching is given [here](#patternmatching).

A **course-of-values table** omits the patterns and just lists all
values. It uses the enumeration of all values of the argument type *P*
to pair the values with arguments:

`table` *P* `[`*t*~1~ ; \... ; *t*~n~`]`

This format is not recommended for GF source code, since the ordering of
parameter values is not specified and therefore a compiler-internal
decision.

The argument type can be indicated in ordinary tables as well, which is
sometimes helpful for type inference:

`table` *P* `{` \... `}`

The **selection** operator `!`, applied to a table *t* and to an
expression *v* of its argument type

*t* `!` *v*

returns the first pattern matching result from *t* with *v*, as defined
[here](#patternmatching). The order of patterns is thus significant as
long as the patterns contain variables or wildcards. When the compiler
reorders the patterns following the enumeration of all values of the
argument type, this order no longer matters, because no overlap remains
between patterns.

The GF compiler performs **table expansion**, i.e. an analogue of eta
expansion defined [here](#conversions), where a table is applied to all
values to its argument type:

*t* : *P* `=>` *T* ==\> `table` *P* `[`*t* `!` *V*~1~ ; \... ; *t* `!`
*V*~n~`]`

As syntactic sugar, one-branch tables can be written in a way similar to
lambda abstractions:

`\\`*p* `=>` *t* === `table {`*p* `=>` *t* `}`

where *p* is either a variable or a wildcard (`_`). Multiple bindings
can be abbreviated:

`\\`*p,q* `=>` *t* === `\\`*p* `=>` `\\`*q* `=>` *t*

**Case expressions** are syntactic sugar for selections:

`case` *e* `of {`\...`}` === `table {`\...`} !` *e*


### Pattern matching

[]{#patternmatching}

We will list all forms of patterns that can be used in table branches.
We define their **variable bindings** and **matching substitutions**.

We start with the patterns available for all parameter types, as well as
for the types `Integer` and `Str`.

-   A constructor pattern *C* *p*~1~\...*p*~n~ binds the union of all
    variables bound in the subpatterns *p*~1~,\...,*p*~n~. It matches
    any value *C* *V*~1~\...*V*~n~ where each *p*~i~\# matches *V*~i~,
    and the matching substitution is the union of these substitutions.
-   A record pattern `{` *r*~1~ `=` *p*~1~ `;` \... `;` *r*~n~ `=`
    *p*~n~ `}` binds the union of all variables bound in the subpatterns
    *p*~1~,\...,*p*~n~. It matches any value `{` *r*~1~ `=` *V*~1~ `;`
    \... `;` *r*~n~ `=` *V*~n~ `;` \...`}` where each *p*~i~\# matches
    *V*~i~, and the matching substitution is the union of these
    substitutions.
-   A variable pattern *x* (identifier other than parameter constructor)
    binds the variable *x*. It matches any value *V*, with the
    substitution {*x* = *V*}.
-   The wild card `_` binds no variables. It matches any value, with the
    empty substitution.
-   A disjunctive pattern *p* `|` *q* binds the intersection of the
    variables bound by *p* and *q*. It matches anything that either *p*
    or *q* matches, with the first substitution starting with *p*
    matches, from which those variables that are not bound by both
    patterns are removed.
-   A negative pattern `-` *p* binds no variables. It matches anything
    that *p* does *not* match, with the empty substitution.
-   An alias pattern *x* `@` *p* binds *x* and all the variables bound
    by *p*. It matches any value *V* that *p* matches, with the same
    substition extended by {*x* = *V*}.

The following patterns are only available for the type `Str`:

-   A string literal pattern, e.g. `"s"`, binds no variables. It matches
    the same string, with the empty substitution.
-   A concatenation pattern, *p* `+` *q*, binds the union of variables
    bound by *p* and *q*. It matches any string that consists of a
    prefix matching *p* and a suffix matching *q*, with the union of
    substitutions corresponding to the first match (see below).
-   A repetition pattern *p*`*` binds no variables. It matches any
    string that can be decomposed into strings that match *p*, with the
    empty substitution.

The following pattern is only available for the types `Integer` and
`Ints` *n*:

-   An integer literal pattern, e.g. `214`, binds no variables. It
    matches the same integer, with the empty substitution.

All patterns must be **linear**: the same pattern variable may occur
only once in them. This is what makes it straightforward to speak about
unions of binding sets and substitutions.

Pattern matching is performed in the order in which the branches appear
in the source code: the branch of the first matching pattern is
followed. In concrete syntax, the type checker reject sets of patterns
that are not exhaustive, and warns for completely overshadowed patterns.
It also checks the type correctness of patterns with respect to the
argument type. In abstract syntax, only type correctness is checked, no
exhaustiveness or overshadowing.

It follows from the definition of record pattern matching that it can
utilize partial records: the branch

        {g = Fem} => t

in a table of type `{g : Gender ; n : Number} => T` means the same as

        {g = Fem ; n = _} => t

Variables in regular expression patterns are always bound to the **first
match**, which is the first in the sequence of binding lists. For
example:

-   `x + "e" + y` matches `"peter"` with `x = "p", y = "ter"`
-   `x + "er"*` matches `"burgerer"` with `x = "burg"`


### Free variation

An expressions of the form

`variants` `{`*t*~1~ ; \... ; *t*~n~`}`

where all *t*~i~ are of the same type *T*, has itseld type *T*. This
expression presents *t*~i~,\...,*t*~n~ as being in **free variation**:
the choice between them is not determined by semantics or parameters. A
limiting case is

`variants {}`

which encodes a rule saying that there is no way to express a certain
thing, e.g. that a certain inflectional form does not exist.

A common wisdom in linguistics is that \"there is no free variation\",
which refers to the situation where *all* aspects are taken into
account. For instance, the English negation contraction could be
expressed as free variation,

        variants {"don't" ; "do" ++ "not"}

if only semantics is taken into account, but if stylistic aspects are
included, then the proper formulation might be with a parameter
distinguishing between informal and formal style:

        case style of {Informal => "don't" ; Formal => "do" ++ "not"}

Since there is not way to choose a particular element from a
\`\`variants\` list, free variants is normally not adequate in
libraries, nor in grammars meant for natural language generation. In
application grammars meant to parse user input, free variation is a way
to avoid cluttering the abstract syntax with semantically insignificant
distinctions and even to tolerate some grammatical errors.

Permitting `variants` in all types involves a major modification of the
semantics of GF expressions. All computation rules have to be lifted to
deal with lists of expressions and values. For instance,

*t* `!` `variants` `{`*t*~1~ ; \... ; *t*~n~`}` ==\> `variants` `{`*t*
`!` *t*~1~ ; \... ; *t* `!` *t*~n~`}`

This is done in such a way that variation does not distribute to records
(or other product-like structures). For instance, variants of records,

        variants {{s = "Auto" ; g = Neutr} ; {s = "Wagen" ; g = Masc}}

is *not* the same as a record of variants,

        {s = variants {"Auto" ; "Wagen"} ; g = variants {Neutr ; Masc}}

Variants of variants are flattened,

`variants` `{`\...; `variants` `{`*t*~1~ ;\...; *t*~n~`}` ;\...`}` ==\>
`variants` `{`\...; *t*~1~ ;\...; *t*~n~ ;\...`}`

and singleton variants are eliminated,

`variants` `{`*t*`}` ==\> *t*


### Local definitions

A **local definition**, i.e. a **let expression** has the form

`let` *x* : *T* = *t* `in` *e*

The type of *x* must be *T*, which also has to be the type of *t*.
Computation is performed by substituting *t* for *x* in *e*:

`let` *x* : *T* = *t* `in` *e* ==\> *e* {*x* = *t*}

As syntactic sugar, the type can be omitted if the type checker is able
to infer it:

`let` *x* = *t* `in` *e*

It is possible to compress several local definitions into one block:

`let` *x* : *T* = *t* `;` *y* : *U* = *u* `in` *e* === `let` *x* : *T* =
*t* `in` `let` *y* : *U* = *u* `in` *e*

Another notational variant is a definition block appearing after the
main expression:

*e* `where` `{`\...`}` === `let` `{`\...`}` `in` *e*

Curly brackets are obligatory in the `where` form, and can also be
optionally used in the `let` form.

Since a block of definitions is treated as syntactic sugar for a nested
`let` expression, a constant must be defined before it is used: the
scope is not mutual, as in a module body. Furthermore, unlike in `lin`
and `oper` definitions, it is *not* possible to bind variables on the
left of the equality sign.


### Function applications in concrete syntax

[]{#functionelimination}

Fully compiled concrete syntax may not include expressions of function
types except on the outermost level of `lin` rules, as defined
[here](#linexpansion). However, in the source code, and especially in
`oper` definitions, functions are the main vehicle of code reuse and
abstraction. Thus function types and functions follow the same rules as
in abstract syntax, as specified [here](#functiontype). In particular,
the application of a lambda abstract is computed by beta conversion.

To ensure the elimination of functions, GF uses a special computation
rule for pushing function applications inside tables, since otherwise
run-time variables could block their applications:

(`table` `{`*p*~1~ `=>` *f*~1~ ; \... ; *p*~n~ `=>` *f*~n~ `}` `!` *e*)
*a* ==\> `table` `{`*p*~1~ `=>` *f*~1~ *a* ; \... ; *p*~n~ `=>` *f*~n~
*a*`}` `!` *e*

Also parameter constructors with non-empty contexts, as defined
[here](#paramjudgements), result in expressions in application form.
These expressions are never a problem if their arguments are just
constructors, because they can then be translated to integers
corresponding to the position of the expression in the enumaration of
the values of its type. However, a constructor applied to a run-time
variable may need to be converted as follows:

*C*\...*x*\... ==\> `case` *x* of `{_ =>` *C*\...*x*`}`

The resulting expression, when processed by table expansion as explained
[here](#tables), results in *C* being applied to just values of the type
of *x*, and the application thereby disappears.


### Reusing top-level grammars as resources

[]{#reuse}

*This section is valid for GF 3.0, which abandons the \"lock field\"*
*discipline of GF 2.8.*

As explained [here](#openabstract), abstract syntax modules can be
opened as interfaces and concrete syntaxes as their instances. This
means that judgements are, as it were, translated in the following way:

-   `cat` *C* *G* ===\> `oper` *C* : `Type`
-   `fun` *f* : *T* ===\> `oper` *f* : *T*
-   `lincat` *C* = *T* ===\> `oper` *C* : `Type` = *C*
-   `lin` *f* = *t* ===\> `oper` *f* = *t*

Notice that the value *T* of `lincat` definitions is not disclosed in
the translation. This means that the type *C* remains abstract: the only
ways of building an object of type *C* are the operations *f* obtained
from *fun* and *lin* rules.

The purpose of keeping linearization types abstract is to enforce
**grammar checking via type checking**. This means that any well-typed
operation application is also well-typed in the sense of the original
grammar. If the types were disclosed, then we could for instance easily
confuse all categories that have the linearization type `{s : Str}`. Yet
another reason is that revealing the types makes it impossible for the
library programmers to change their type definitions afterwards.

Library writers may occasionally want to have access to the values of
linearization types. The way to make it possible is to add an extra
construction operation to a module in which the linearization type is
available:

        oper MkC : T -> C = \x -> x

In object-oriented terms, the type *C* itself is **protected**, whereas
*MkC* is a **public constructor** of *C*. Of course, it is possible to
make these constructors overloaded (concept explained
[here](#overloading)), to enable easy access to special cases.


### Predefined concrete syntax types

[]{#predefcnc}

The following concrete syntax types are predefined:

-   `Str`, the type of tokens and token lists (defined [here](#strtype))
-   `Integer`, the type of nonnegative integers
-   `Ints` *n*, the type of integers from *0* to *n*
-   `Type`, the type of (concrete syntax) types
-   `PType`, the type of parameter types

The last two types are, in a way, extended by user-written grammars,
since new parameter types can be defined in the way shown
[here](#paramjudgements), and every paramater type is also a type. From
the point of view of the values of expressions, however, a `param`
declaration does not extend `PType`, since all parameter types get
compiled to initial segments of integers.

Notice the difference between the concrete syntax types `Str` and
`Integer` on the one hand, and the abstract syntax categories `String`
and `Int`, on the other. As *concrete syntax* types, the latter are
treated in the same way as any reused categories: their objects can be
formed by using syntax trees (string and integer literals).

*The type name* `Integer` *replaces in GF 3.0 the name* `Int`, *to avoid
confusion with the abstract syntax type and to be analogous* *with the*
`Str` *vs.* `String` *distinction.*


### Predefined concrete syntax operations

The following predefined operations are defined in the resource module
`prelude/Predef.gf`. Their implementations are defined as a part of the
GF grammar compiler.

  -------------------------------------------------------------------------------------------------
  operation      type                              explanation
  -------------- --------------------------------- ------------------------------------------------
  `PBool`        `PType`                           `PTrue | PFalse`

  `Error`        `Type`                            the empty type

  `Int`          `Type`                            the type of integers

  `Ints`         `Integer -> Type`                 the type of integers from 0 to n

  `error`        `Str -> Error`                    forms error message

  `length`       `Str -> Int`                      length of string

  `drop`         `Integer -> Str -> Str`           drop prefix of length

  `take`         `Integer -> Str -> Str`           take prefix of length

  `tk`           `Integer -> Str -> Str`           drop suffix of length

  `dp`           `Integer -> Str -> Str`           take suffix of length

  `eqInt`        `Integer -> Integer -> PBool`     test if equal integers

  `lessInt`      `Integer -> Integer -> PBool`     test order of integers

  `plus`         `Integer -> Integer -> Integer`   add integers

  `eqStr`        `Str -> Str -> PBool`             test if equal strings

  `occur`        `Str -> Str -> PBool`             test if occurs as substring

  `occurs`       `Str -> Str -> PBool`             test if any char occurs

  `show`         `(P : Type) -> P -> Str`          convert param to string

  `read`         `(P : Type) -> Str -> P`          convert string to param

  `toStr`        `(L : Type) -> L -> Str`          find the \"first\" string

  `nonExist`     `Str`                             a special token marking\
                                                   non-existing morphological forms

  `BIND`         `Str`                             a special token marking\
                                                   that the surrounding tokens should not\
                                                   be separated by space

  `SOFT_BIND`    `Str`                             a special token marking\
                                                   that the surrounding tokens may not\
                                                   be separated by space

  `SOFT_SPACE`   `Str`                             a special token marking\
                                                   that the space between the surrounding tokens\
                                                   is optional

  `CAPIT`        `Str`                             a special token marking\
                                                   that the first character in the next token\
                                                   should be capitalized

  `ALL_CAPIT`    `Str`                             a special token marking\
                                                   that the next word should be\
                                                   in all capital letters
  -------------------------------------------------------------------------------------------------

Compilation eliminates these operations, and they may therefore not take
arguments that depend on run-time variables.

The module `Predef` is included in the *opens* list of all modules, and
therefore does not need to be opened explicitly.


Flags and pragmas
-----------------


### Some flags and their values

[]{#flagvalues}

The flag `coding` in concrete syntax sets the **character encoding**
used in the grammar. Internally, GF uses unicode, and `.pgf` files are
always written in UTF8 encoding. The presence of the flag `coding=utf8`
prevents GF from encoding an already encoded file.

The flag `startcat` in abstract syntax sets the default start category
for parsing, random generation, and any other grammar operation that
depends on category. Its legal values are the categories defined or
inherited in the abstract syntax.


### Compiler pragmas

**Compiler pragmas** are a special form of comments prefixed with `--#`.
Currently GF interprets the following pragmas.

  pragma         explanation
  -------------- ---------------------------------
  `-path=`PATH   path list for searching modules

For instance, the line

        --# -path=.:present:prelude:/home/aarne/GF/tmp

in the top of `FILE.gf` causes the GF compiler, when invoked on
`FILE.gf`, to search through the current directory (`.`) and the
directories `present`, `prelude`, and `/home/aarne/GF/tmp`, in this
order. If a directory `DIR` is not found relative to the working
directory, `$(GF_LIB_PATH)/DIR` is searched. `$GF_LIB_PATH` can be a
colon-separated list of directories, in which case each directory in the
list contributes to the search path expansion.


Alternative grammar input formats
---------------------------------

While the GF language as specified in this document is the most
versatile and powerful way of writing GF grammars, there are several
other formats that a GF compiler may make available for users, either to
get started with small grammars or to semiautomatically convert grammars
from other formats to GF. Here are the ones supported by GF 2.8 and 3.0.


### Old GF without modules

[]{#oldgf}

Before GF compiler version 2.0, there was no module system, and all
kinds of judgement could be written in all files, without any headers.
This format is still available, and the compiler (version 2.8) detects
automatically if a file is in the current or the old format. However,
the old format is not recommended because of pure modularity and missing
separate compilation, and also because libraries are not available,
since the old and the new format cannot be mixed. With version 2.8,
grammars in the old format can be converted to modular grammar with the
command

        > import -o FILE.gf

which rewrites the grammar divided into three files: an abstract, a
concrete, and a resource module.


### Context-free grammars

A quick way to write a GF grammar is to use the context-free format,
also known as BNF. Files of this form are recognized by the suffix
`.cf`. Rules in these files have the form

*Label* `.` *Cat* `::=` (*String* \| *Cat*)\* `;`

where *Label* and *Cat* are identifiers and *String* quoted strings.

There is a shortcut form generating labels automatically,

*Cat* `::=` (*String* \| *Cat*)\* `;`

In the shortcut form, vertical bars (`|`) can be used to give several
right-hand-sides at a time. An empty right-hand side means the singleton
of an empty sequence, and not an empty union.

Just like old-style GF files (previous section), contex-free grammar
files can be converted to modular GF by using the `-o` option to the
compiler in GF 2.8.


### Extended BNF grammars

Extended BNF (`FILE.ebnf`) goes one step further from the shortcut
notation of previous section. The rules have the form

*Cat* `::=` *RHS* `;`

where an *RHS* can be any regular expression built from quoted strings
and category symbols, in the following ways:

  RHS item          explanation
  ----------------- ------------------------
  *Cat*             nonterminal
  *String*          terminal
  *RHS* *RHS*       sequence
  *RHS* `|` *RHS*   alternatives
  *RHS* `?`         optional
  *RHS* `*`         repetition
  *RHS* `+`         non-empty repetition\|

Parentheses are used to override standard precedences, where `|` binds
weaker than sequencing, which binds weaker than the unary operations.

The compiler generates not only labels, but also new categories
corresponding to the regular expression combinations actually in use.

Just like `.cf` files (previous section), `.ebnf` files can be converted
to modular GF by using the `-o` option to the compiler in GF 2.8.


### Example-based grammars

**Example-based grammars** (`.gfe`) provide a way to use resource
grammar libraries without having to know the names of functions in them.
The compiler works as a preprocessor, saving the result in a (`.gf`)
file, which can be compiled as usual.

If a library is implemented as an abstract and concrete syntax, it can
be used for parsing. Calls of library functions can therefore be formed
by parsing strings in the library. GF has an expression format for this,

`in` *C* *String*

where *C* is the category in which to parse (it can be qualified by the
module name) and the string is the input to parser. Expressions of this
form are replaced by the syntax trees that result. These trees are
always type-correct. If several parses are found, all but the first one
are given in comments.

Here is an example, from `GF/examples/animal/`:

        --# -resource=../../lib/present/LangEng.gfc
        --# -path=.:present:prelude

        incomplete concrete QuestionsI of Questions = open Lang in {
          lincat
            Phrase = Phr ;
            Entity = N ;
            Action = V2 ;
          lin
            Who  love_V2 man_N           = in Phr "who loves men" ;
            Whom man_N love_V2           = in Phr "whom does the man love" ;
            Answer woman_N love_V2 man_N = in Phr "the woman loves men" ;
        }

The `resource` pragma shows the grammar that is used for parsing the
examples.

Notice that the variables `love_V2`, `man_N`, etc, are actually
constants in the library. In the resulting rules, such as

        lin Whom = \man_N -> \love_V2 ->
          PhrUtt NoPConj (UttQS (UseQCl TPres ASimul PPos
            (QuestSlash whoPl_IP (SlashV2 (DetCN (DetSg (SgQuant
              DefArt)NoOrd)(UseN man_N)) love_V2)))) NoVoc ;

those constants are nonetheless treated as variables, following the
normal binding conventions, as stated [here](#renaming).


The grammar of GF
-----------------

The following grammar is actually used in the parser of GF, although we
have omitted some obsolete rules still included in the parser for
backward compatibility reasons.

This document was automatically generated by the *BNF-Converter*. It was
generated together with the lexer, the parser, and the abstract syntax
module, which guarantees that the document matches with the
implementation of the language (provided no hand-hacking has taken
place).


The lexical structure of GF
---------------------------


### Identifiers

Identifiers *Ident* are unquoted strings beginning with a letter,
followed by any combination of letters, digits, and the characters `_ '`
reserved words excluded.


### Literals

Integer literals *Integer* are nonempty sequences of digits.

String literals *String* have the form `"`*x*`"`}, where *x* is any
sequence of any characters except `"` unless preceded by `\`.

Double-precision float literals *Double* have the structure indicated by
the regular expression `digit+ '.' digit+ ('e' ('-')? digit+)?` i.e.
two sequences of digits separated by a decimal point, optionally
followed by an unsigned or negative exponent.


### Reserved words and symbols

The set of reserved words is the set of terminals appearing in the
grammar. Those reserved words that consist of non-letter characters are
called symbols, and they are treated in a different way from those that
are similar to identifiers. The lexer follows rules familiar from
languages like Haskell, C, and Java, including longest match and spacing
conventions.

The reserved words used in GF are the following:

- `PType`
- `Str`
- `Strs`
- `Type`
- `abstract`
- `case`
- `cat`
- `concrete`
- `data`
- `def`
- `flags`
- `fun`
- `in`
- `incomplete`
- `instance`
- `interface`
- `let`
- `lin`
- `lincat`
- `lindef`
- `linref`
- `of`
- `open`
- `oper`
- `param`
- `pre`
- `printname`
- `resource`
- `strs`
- `table`
- `transfer`
- `variants`
- `where`
- `with`                       

The symbols used in GF are the following:

- `;`
- `=`
- `:`
- `->`
- `{`
- `}`
- `**`
- `,`
- `(`
- `)`
- `[`
- `]`
- `-`
- `.`
- `|`
- `?`
- `<`
- `>`
- `@`
- `!`
- `*`
- `+`
- `++`
- `\`
- `=>`
- `_`
- `$`
- `/`

### Comments

Single-line comments begin with `--`.
Multiple-line comments are enclosed with `{-` and `-}`.


The syntactic structure of GF
-----------------------------

Terminal appear as `code`.
The symbols **->** (production), **|** (union) and **eps** (empty rule) belong to the BNF notation.
All other symbols are non-terminals.

  --------------------- -------- ------------------------------------------------------------------------------------
  *Grammar*             **->**   *\[ModDef\]*
  *\[ModDef\]*          **->**   **eps**
                        **|**    *ModDef* *\[ModDef\]*
  *ModDef*              **->**   *ModDef* `;`
                        **|**    *ComplMod* *ModType* `=` *ModBody*
  *ModType*             **->**   `abstract` *Ident*
                        **|**    `resource` *Ident*
                        **|**    `interface` *Ident*
                        **|**    `concrete` *Ident* `of` *Ident*
                        **|**    `instance` *Ident* `of` *Ident*
                        **|**    `transfer` *Ident* `:` *Open* `->` *Open*
  *ModBody*             **->**   *Extend* *Opens* `{` *\[TopDef\]* `}`
                        **|**    *\[Included\]*
                        **|**    *Included* `with` *\[Open\]*
                        **|**    *Included* `with` *\[Open\]* `**` *Opens* `{` *\[TopDef\]* `}`
                        **|**    *\[Included\]* `**` *Included* `with` *\[Open\]*
                        **|**    *\[Included\]* `**` *Included* `with` *\[Open\]* `**` *Opens* `{` *\[TopDef\]* `}`
  *\[TopDef\]*          **->**   **eps**
                        **|**    *TopDef* *\[TopDef\]*
  *Extend*              **->**   *\[Included\]* `**`
                        **|**    **eps**
  *\[Open\]*            **->**   **eps**
                        **|**    *Open*
                        **|**    *Open* `,` *\[Open\]*
  *Opens*               **->**   **eps**
                        **|**    `open` *\[Open\]* `in`
  *Open*                **->**   *Ident*
                        **|**    `(` *QualOpen* *Ident* `)`
                        **|**    `(` *QualOpen* *Ident* `=` *Ident* `)`
  *ComplMod*            **->**   **eps**
                        **|**    `incomplete`
  *QualOpen*            **->**   **eps**
  *\[Included\]*        **->**   **eps**
                        **|**    *Included*
                        **|**    *Included* `,` *\[Included\]*
  *Included*            **->**   *Ident*
                        **|**    *Ident* `[` *\[Ident\]* `]`
                        **|**    *Ident* `-` `[` *\[Ident\]* `]`
  *Def*                 **->**   *\[Name\]* `:` *Exp*
                        **|**    *\[Name\]* `=` *Exp*
                        **|**    *Name* *\[Patt\]* `=` *Exp*
                        **|**    *\[Name\]* `:` *Exp* `=` *Exp*
  *TopDef*              **->**   `cat` *\[CatDef\]*
                        **|**    `fun` *\[FunDef\]*
                        **|**    `data` *\[FunDef\]*
                        **|**    `def` *\[Def\]*
                        **|**    `data` *\[DataDef\]*
                        **|**    `param` *\[ParDef\]*
                        **|**    `oper` *\[Def\]*
                        **|**    `lincat` *\[PrintDef\]*
                        **|**    `lindef` *\[Def\]*
                        **|**    `linref` *\[Def\]*
                        **|**    `lin` *\[Def\]*
                        **|**    `printname` `cat` *\[PrintDef\]*
                        **|**    `printname` `fun` *\[PrintDef\]*
                        **|**    `flags` *\[FlagDef\]*
  *CatDef*              **->**   *Ident* *\[DDecl\]*
                        **|**    `[` *Ident* *\[DDecl\]* `]`
                        **|**    `[` *Ident* *\[DDecl\]* `]` `{` *Integer* `}`
  *FunDef*              **->**   *\[Ident\]* `:` *Exp*
  *DataDef*             **->**   *Ident* `=` *\[DataConstr\]*
  *DataConstr*          **->**   *Ident*
                        **|**    *Ident* `.` *Ident*
  *\[DataConstr\]*      **->**   **eps**
                        **|**    *DataConstr*
                        **|**    *DataConstr* `|` *\[DataConstr\]*
  *ParDef*              **->**   *Ident* `=` *\[ParConstr\]*
                        **|**    *Ident* `=` `(` `in` *Ident* `)`
                        **|**    *Ident*
  *ParConstr*           **->**   *Ident* *\[DDecl\]*
  *PrintDef*            **->**   *\[Name\]* `=` *Exp*
  *FlagDef*             **->**   *Ident* `=` *Ident*
  *\[Def\]*             **->**   *Def* `;`
                        **|**    *Def* `;` *\[Def\]*
  *\[CatDef\]*          **->**   *CatDef* `;`
                        **|**    *CatDef* `;` *\[CatDef\]*
  *\[FunDef\]*          **->**   *FunDef* `;`
                        **|**    *FunDef* `;` *\[FunDef\]*
  *\[DataDef\]*         **->**   *DataDef* `;`
                        **|**    *DataDef* `;` *\[DataDef\]*
  *\[ParDef\]*          **->**   *ParDef* `;`
                        **|**    *ParDef* `;` *\[ParDef\]*
  *\[PrintDef\]*        **->**   *PrintDef* `;`
                        **|**    *PrintDef* `;` *\[PrintDef\]*
  *\[FlagDef\]*         **->**   *FlagDef* `;`
                        **|**    *FlagDef* `;` *\[FlagDef\]*
  *\[ParConstr\]*       **->**   **eps**
                        **|**    *ParConstr*
                        **|**    *ParConstr* `|` *\[ParConstr\]*
  *\[Ident\]*           **->**   *Ident*
                        **|**    *Ident* `,` *\[Ident\]*
  *Name*                **->**   *Ident*
                        **|**    `[` *Ident* `]`
  *\[Name\]*            **->**   *Name*
                        **|**    *Name* `,` *\[Name\]*
  *LocDef*              **->**   *\[Ident\]* `:` *Exp*
                        **|**    *\[Ident\]* `=` *Exp*
                        **|**    *\[Ident\]* `:` *Exp* `=` *Exp*
  *\[LocDef\]*          **->**   **eps**
                        **|**    *LocDef*
                        **|**    *LocDef* `;` *\[LocDef\]*
  *Exp6*                **->**   *Ident*
                        **|**    *Sort*
                        **|**    *String*
                        **|**    *Integer*
                        **|**    *Double*
                        **|**    `?`
                        **|**    `[` `]`
                        **|**    `data`
                        **|**    `[` *Ident* *Exps* `]`
                        **|**    `[` *String* `]`
                        **|**    `{` *\[LocDef\]* `}`
                        **|**    `<` *\[TupleComp\]* `>`
                        **|**    `<` *Exp* `:` *Exp* `>`
                        **|**    `(` *Exp* `)`
  *Exp5*                **->**   *Exp5* `.` *Label*
                        **|**    *Exp6*
  *Exp4*                **->**   *Exp4* *Exp5*
                        **|**    `table` `{` *\[Case\]* `}`
                        **|**    `table` *Exp6* `{` *\[Case\]* `}`
                        **|**    `table` *Exp6* `[` *\[Exp\]* `]`
                        **|**    `case` *Exp* `of` `{` *\[Case\]* `}`
                        **|**    `variants` `{` *\[Exp\]* `}`
                        **|**    `pre` `{` *Exp* `;` *\[Altern\]* `}`
                        **|**    `strs` `{` *\[Exp\]* `}`
                        **|**    *Ident* `@` *Exp6*
                        **|**    *Exp5*
  *Exp3*                **->**   *Exp3* `!` *Exp4*
                        **|**    *Exp3* `*` *Exp4*
                        **|**    *Exp3* `**` *Exp4*
                        **|**    *Exp4*
  *Exp1*                **->**   *Exp2* `+` *Exp1*
                        **|**    *Exp2*
  *Exp*                 **->**   *Exp1* `++` *Exp*
                        **|**    `\` *\[Bind\]* `->` *Exp*
                        **|**    `\` `\` *\[Bind\]* `=>` *Exp*
                        **|**    *Decl* `->` *Exp*
                        **|**    *Exp3* `=>` *Exp*
                        **|**    `let` `{` *\[LocDef\]* `}` `in` *Exp*
                        **|**    `let` *\[LocDef\]* `in` *Exp*
                        **|**    *Exp3* `where` `{` *\[LocDef\]* `}`
                        **|**    `in` *Exp5* *String*
                        **|**    *Exp1*
  *Exp2*                **->**   *Exp3*
  *\[Exp\]*             **->**   **eps**
                        **|**    *Exp*
                        **|**    *Exp* `;` *\[Exp\]*
  *Exps*                **->**   **eps**
                        **|**    *Exp6* *Exps*
  *Patt2*               **->**   `_`
                        **|**    *Ident*
                        **|**    *Ident* `.` *Ident*
                        **|**    *Integer*
                        **|**    *Double*
                        **|**    *String*
                        **|**    `{` *\[PattAss\]* `}`
                        **|**    `<` *\[PattTupleComp\]* `>`
                        **|**    `(` *Patt* `)`
  *Patt1*               **->**   *Ident* *\[Patt\]*
                        **|**    *Ident* `.` *Ident* *\[Patt\]*
                        **|**    *Patt2* `*`
                        **|**    *Ident* `@` *Patt2*
                        **|**    `-` *Patt2*
                        **|**    *Patt2*
  *Patt*                **->**   *Patt* `|` *Patt1*
                        **|**    *Patt* `+` *Patt1*
                        **|**    *Patt1*
  *PattAss*             **->**   *\[Ident\]* `=` *Patt*
  *Label*               **->**   *Ident*
                        **|**    `$` *Integer*
  *Sort*                **->**   `Type`
                        **|**    `PType`
                        **|**    `Str`
                        **|**    `Strs`
  *\[PattAss\]*         **->**   **eps**
                        **|**    *PattAss*
                        **|**    *PattAss* `;` *\[PattAss\]*
  *\[Patt\]*            **->**   *Patt2*
                        **|**    *Patt2* *\[Patt\]*
  *Bind*                **->**   *Ident*
                        **|**    `_`
  *\[Bind\]*            **->**   **eps**
                        **|**    *Bind*
                        **|**    *Bind* `,` *\[Bind\]*
  *Decl*                **->**   `(` *\[Bind\]* `:` *Exp* `)`
                        **|**    *Exp4*
  *TupleComp*           **->**   *Exp*
  *PattTupleComp*       **->**   *Patt*
  *\[TupleComp\]*       **->**   **eps**
                        **|**    *TupleComp*
                        **|**    *TupleComp* `,` *\[TupleComp\]*
  *\[PattTupleComp\]*   **->**   **eps**
                        **|**    *PattTupleComp*
                        **|**    *PattTupleComp* `,` *\[PattTupleComp\]*
  *Case*                **->**   *Patt* `=>` *Exp*
  *\[Case\]*            **->**   *Case*
                        **|**    *Case* `;` *\[Case\]*
  *Altern*              **->**   *Exp* `/` *Exp*
  *\[Altern\]*          **->**   **eps**
                        **|**    *Altern*
                        **|**    *Altern* `;` *\[Altern\]*
  *DDecl*               **->**   `(` *\[Bind\]* `:` *Exp* `)`
                        **|**    *Exp6*
  *\[DDecl\]*           **->**   **eps**
                        **|**    *DDecl* *\[DDecl\]*
  --------------------- -------- ------------------------------------------------------------------------------------
