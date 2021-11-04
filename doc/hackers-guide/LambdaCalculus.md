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
The result from the evaluation of a GF term is either a constructor applied to a list of other values or an unapplied lambda abstraction:
```Haskell
type Env = [(Ident,Value)]
data Value
  = VApp Ident [Value]
  | VClosure Env Term
```
In the later case we also preserve the environment as it was when we encountered the lambda abstraction. That is necessary since the body of the abstraction may contain free variables whose values are defined in the environment.

The evaluation itself is simple:
```Haskell
eval env (Vr x)       vs  = apply (lookup x env) vs
eval env (Cn c)       vs  = VApp c vs
eval env (App t1 t2)  vs  = eval env t1 (eval env t2 : vs)
eval env (Abs x t)    []  = VClosure env (Abs x t)
eval env (Abs x t) (v:vs) = eval ((x,v):env) t vs

apply (VApp c vs0)                vs  = VApp c (vs0++vs)
apply (VClosure env (Abs x t)) (v:vs) = eval ((x,v):env) t vs
```
Here the we use the `apply` function to apply an already evaluated term to a list of other values.

When we talk about functional languages, we usually discuss the evaluation order and we differentiate between about lazy and strict languages. Simply speaking, a strict language evaluates the arguments of a function before the function is called. In a lazy language, on the other hand, the arguments are passed unevaluated and are computed only if the value is really needed for the execution of the function. The main advantage of lazy languages is that they guarantee the termination of the computation in some cases where strict languages don't. The GF language does not allow recursion and therefore all programs terminate. Looking from only that angle it looks like the evaluation order is irrelevant in GF. Perhaps that is also the reason why this has never been discussed before. The question, however, becomes relevant again if we want to have an optimal semantics for variants. As we will see in the next section, the only way to get that is if we define GF as a lazy language.

After that discussion, there is an interesting question. Does the eval/apply implementation above define a strict or a lazy language? We have the rule:
```Haskell
eval env (App t1 t2)  vs  = eval env t1 (eval env t2 : vs)
```
where we see that when a term `t1` is applied to a term `t2` then both get evaluated. The answer to the question then depends on the semantics of the implementation language. Since the evaluation is implemented in Haskell, `eval env t2` would not be computed unless if the value is really neeeded. Therefore, our implementation defines a new lazy language. On the other hand, if the same algorithm is directly transcribed in ML then it will define a strict one instead of a lazy one.

```Haskell
value2term i (VApp c vs) =
  foldl (\t v -> App t (value2term i v)) (Cn c) vs
value2term i (VGen j vs) =
  foldl (\t v -> App t (value2term i v)) (Vr ('v':show j)) vs
value2term i (VClosure env (Abs x t)) =
  let v = eval ((x,VGen i []):env) t []
  in Abs ('v':show i) (value2term (i+1) v)
```

# Variants

# Meta Variables
