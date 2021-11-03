The concrete syntax in GF is expressed in a special kind of functional language. Unlike in other functional languages, all GF programs are computed at compile time. The result of the computation is another program in a simplified formalized called Parallel Multiple Context-Free Grammar (PMCFG). More on that later. For now we will only discuss how the computations in a GF program work.

At the heart of the GF compiler is the so called partial evaluator. It computes GF terms but it also have the added super power to be able to work with unknown variables. Consider for instance the term ``\s -> s ++ ""``. A normal evaluator cannot do anything with it, since in order to compute the value of the lambda function, you need to know the value of ``s``. In the computer science terminology the term is already in its normal form. A partial evaluator on the other hand, will just remember that ``s`` is a variable with an unknown value and it will try to compute the expression in the body of the function. After that it will construct a new function where the body is precomputed as much as it goes. In the concrete case the result will be ``\s -> s``, since adding an empty string to any other string produces the same string.

Another super power of the partial evaluator is that it can work with meta variables. The syntax for meta variables in GF is ``?0, ?1, ?2, ...``, and they are used as placeholders which mark parts of the program that are not finished yet. The partial evaluator has no problem to work with such incomplete programs. Sometimes the result of the computation depends on a yet unfinished part of the program, then the evaluator just suspends the computation. In other cases, the result is completely independent of the existance of metavariables. In the later, the evaluator will just return the result.

One of the uses of the evaluator is during type checking where we must enforce certain constraints. The constraints may for instance indicate that the only way for them to be satisfied is to assign a fixed value to one or more of the meta variables. The partial evaluator does that as well. Another use case is during compilation to PMCFG. The compiler to PMCFG, in certain cases assigns to a metavariable all possible values that the variable may have and it then produces different results.

In the rest of we will discuss the implementation of the partial evaluator.

# Simple Lambda Terms

We will start with the simplest possible subset of the GF language, also known as simple lambda calculus. It is defined as an algebraic data type in Haskell, as follows:
```Haskell
data Term
  = Vr Ident           -- e.g. variables: x,y,z ...
  | Cn Ident           -- e.g. constructors: cons, nil, etc.
  | App Term Term      -- e.g. function application: @f x@
  | Abs Ident Term     -- e.g. \x -> t
```

```Haskell
type Env = [(Ident,Value)]
data Value
  = VApp Ident [Value]
  | VClosure Env Term
```

```Haskell
eval env (Vr x)         vs  = apply (lookup x env) vs
eval env (Cn c)         vs  = VApp c vs
eval env (App t1 t2)    vs  = eval env t1 (eval env t2 : vs)
eval env (Abs b x t)    []  = return (VClosure env (Abs b x t))
eval env (Abs b x t) (v:vs) = eval ((x,v):env) t vs

apply (VApp c vs0)                   vs = VApp c (vs0++vs)
apply (VClosure env (Abs b x t)) (v:vs) = eval ((x,v):env) t vs
```

# Variants

# Meta Variables
