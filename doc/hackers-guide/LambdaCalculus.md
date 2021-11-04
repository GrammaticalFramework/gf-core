The concrete syntax in GF is expressed in a special kind of functional language. Unlike in other functional languages, all GF programs are computed at compile time. The result of the computation is another program in a simplified formalized called Parallel Multiple Context-Free Grammar (PMCFG). More on that later. For now we will only discuss how the computations in a GF program work.

At the heart of the GF compiler is the so called partial evaluator. It computes GF terms but it also have the added super power to be able to work with unknown variables. Consider for instance the term ``\s -> s ++ ""``. A normal evaluator cannot do anything with it, since in order to compute the value of the lambda function, you need to know the value of ``s``. In the computer science terminology the term is already in its normal form. A partial evaluator on the other hand, will just remember that ``s`` is a variable with an unknown value and it will try to compute the expression in the body of the function. After that it will construct a new function where the body is precomputed as much as it goes. In the concrete case the result will be ``\s -> s``, since adding an empty string to any other string produces the same string.

Another super power of the partial evaluator is that it can work with meta variables. The syntax for meta variables in GF is ``?0, ?1, ?2, ...``, and they are used as placeholders which mark parts of the program that are not finished yet. The partial evaluator has no problem to work with such incomplete programs. Sometimes the result of the computation depends on a yet unfinished part of the program, then the evaluator just suspends the computation. In other cases, the result is completely independent of the existance of metavariables. In the later, the evaluator will just return the result.

One of the uses of the evaluator is during type checking where we must enforce certain constraints. The constraints may for instance indicate that the only way for them to be satisfied is to assign a fixed value to one or more of the meta variables. The partial evaluator does that as well. Another use case is during compilation to PMCFG. The compiler to PMCFG, in certain cases assigns to a metavariable all possible values that the variable may have and it then produces different results.

In the rest of we will discuss the implementation of the partial evaluator.

# Simple Lambda Terms

We will start with the simplest possible subset of the GF language, also known as simple lambda calculus. It is defined as an algebraic data type in Haskell, as follows:
```Haskell
data Term
  = Vr Ident           -- i.e. variables: x,y,z ...
  | Cn Ident           -- i.e. constructors: cons, nil, etc.
  | App Term Term      -- i.e. function application: @f x@
  | Abs Ident Term     -- i.e. \x -> t
```
The result from the evaluation of a GF term is either a constructor applied to a list of other values, or an unapplied lambda abstraction:
```Haskell
type Env = [(Ident,Value)]
data Value
  = VApp Ident [Value]   -- i.e. constructor application
  | VClosure Env Term    -- i.e. a closure contains an environment and the term for a lambda abstraction
  | VGen Int [Value]     -- we will also need that special kind of value for the partial evaluator
```
For the lambda abstractions we build a closure which preserves the environment as it was when we encountered the abstraction. That is necessary since its body may contain free variables whose values are defined in the environment.

The evaluation itself is simple:
```Haskell
eval env (Vr x)          args = apply (lookup x env) args
eval env (Cn c)          args = VApp c args
eval env (App t1 t2)     args = eval env t1 (eval env t2 : args)
eval env (Abs x t)         [] = VClosure env (Abs x t)
eval env (Abs x t) (arg:args) = eval ((x,v):env) t args

apply (VApp c vs)                    args = VApp c (vs++args)
apply (VClosure env (Abs x t)) (arg:args) = eval ((x,arg):env) t args
apply (VGen i vs)                    args = VGen i (vs++args)
```
Here the we use the `apply` function to apply an already evaluated term to a list of arguments.

When we talk about functional languages, we usually discuss the evaluation order and we differentiate between about lazy and strict languages. Simply speaking, a strict language evaluates the arguments of a function before the function is called. In a lazy language, on the other hand, the arguments are passed unevaluated and are computed only if the value is really needed for the execution of the function. The main advantage of lazy languages is that they guarantee the termination of the computation in some cases where strict languages don't. The GF language does not allow recursion and therefore all programs terminate. Looking from only that angle it looks like the evaluation order is irrelevant in GF. Perhaps that is also the reason why this has never been discussed before. The question, however, becomes relevant again if we want to have an optimal semantics for variants. As we will see in the next section, the only way to get that is if we define GF as a lazy language.

After that discussion, there is an interesting question. Does the eval/apply implementation above define a strict or a lazy language? We have the rule:
```Haskell
eval env (App t1 t2)  vs  = eval env t1 (eval env t2 : vs)
```
where we see that when a term `t1` is applied to a term `t2` then both get evaluated. The answer to the question then depends on the semantics of the implementation language. Since the evaluation is implemented in Haskell, `eval env t2` would not be computed unless if its value is really neeeded. Therefore, our implementation defines a new lazy language. On the other hand, if the same algorithm is directly transcribed in ML then it will define a strict one instead of a lazy one.

So far we only defined the evaluator which does the usual computations, but it still can't simplify terms like ``\s -> s ++ ""`` where the simplification happens under the lambda abstraction. The normal evaluator would simply return the abstraction unchanged. To take the next step, we also need a function which takes a value and produces a new term which is precomputed as much as possible:
```Haskell
value2term i (VApp c vs) =
  foldl (\t v -> App t (value2term i v)) (Cn c) vs
value2term i (VGen j vs) =
  foldl (\t v -> App t (value2term i v)) (Vr ('v':show j)) vs
value2term i (VClosure env (Abs x t)) =
  let v = eval ((x,VGen i []):env) t []
  in Abs ('v':show i) (value2term (i+1) v)
```
The interesting rule here is how closures are turned back to terms. We simply evaluate the body of the lambda abstraction with an environment which binds the variable with the special value `VGen i []`. That value stands for the free variable bound by the `i`-th lambda abstraction counted from the outset of the final term inwards. The only thing that we can do with a free variable is to apply it to other values and this is exactly what `apply` does above. After we evaluate the body of the lambda abstraction, the final value is turned back to a term and we reapply a lambda abstraction on top of it. Note that here we also use `i` as a way to generate fresh variables. Whenever, `value2term` encounters a `VGen` it concerts it back to a variable, i.e. `Vr ('v':show j)`.

Given the two functions `eval` and `value2term`, a partial evaluator is defined as:
```Haskell
normalForm t = value2term 0 (eval [] t [])
```

Of course the rules above describe only the core of a functional language. If we really want to be able to simplify terms like ``\s -> s ++ ""``, then we must
add string operations as well. The full implementation of GF for instance knows that an empty string concatenated with any other value results in the same value. This is true even if the other value is actually a variable, i.e. a `VGen` in the internal representation. On the other hand, it knows that pattern matching on a variable is impossible to precompute. In other words, the partial evaluator would leave the term:
```GF
\x -> case x of {
        _+"s" -> x+"'"
        _     -> x+"'s"
      }
```
unchanged since it can't know whether the value of `x` ends with `"s"`.

# Variants

GF supports variants which makes its semantics closer to the language [Curry](https://en.wikipedia.org/wiki/Curry_(programming_language)) than to Haskell. We support terms like `("a"|"b")` which are used to define equivalent linearizations for one and the same semantic term. Perhaps the most prototypical example is for spelling variantions. For instance, if we want to blend British and American English into the same language then we can use `("color"|"colour")` whenever either of the forms is accepted.

The proper implementation for variants complicates the semantics of the language a lot. Consider the term `(\x -> x + x) ("a"|"b")`! Its value depends on whether our language is defined as lazy or strict. In a strict language, we will first evaluate the argument:
```GF
   (\x -> x + x) ("a"|"b")
=> ((\x -> x + x) "a") | ((\x -> x + x) "b")
=> ("a"+"a") | ("b"+"b")
=> ("aa"|"bb")
```
and therefore there are only two values `"aa"´ and `"bb"´. On the other hand in a lazy language, we will do the function application first:
```GF
   (\x -> x + x) ("a"|"b")
=> ("a"|"b") + ("a"|"b")
=> ("aa"|"ab"|"ba"|"bb")
```
and get four different values. The experience shows that a semantics producing only two values is more useful since it gives us a way to control how variants are expanded. If you want the same variant to appear in two different places, just bind the variant to a variable first! It looks like a strict evaluation order has an advantage here. Unfortunately that is not always the case. Consider another example, in a strict order:
```GF
   (\x -> "c") ("a"|"b")
=> ((\x -> "c") "a") | ((\x -> "c") "b")
=> ("c" | "c")
```
Here we get two variants with one and the same value "c". A lazy evaluation order would have avoided the redundancy since `("a"|"b")` would never have been computed.

The best strategy is to actually use lazy evaluation but not to treat the variants as values. Whenever we encounter a variant term, we just split the evaluation in two different branches, one for each variant. At the end of the computation, we get a set of values which does not contain variants. The partial evaluator converts each value back to a term and combines all terms back to a single one by using a top-level variant. The first example would then compute as:
```GF
   (\x -> x + x) ("a"|"b")
=> x + x where x = ("a"|"b")

-- Branch 1:
=> x + x where x = "a"
=> "a" + "a" where x = "a"
=> "aa"

-- Branch 2:
=> x + x where x = "b"
=> "b" + "b" where x = "b"
=> "bb"
```
Here the first step proceeds without branching. We just compute the body of the lambda function while remembering that `x` is bound to the unevaluated term `("a"|"b")`. When we encounter the concatenation `x + x`, then we actually need the value of `x`. Since it is bound to a variant, we must split the evaluation into two branches. In each branch `x` is rebound to either of the two variants `"a"` or `"b"`. The partial evaluator would then recombine the results into `"aa"|"bb"`.

If we consider the second example, it will proceed as:
```GF
   (\x -> "c") ("a"|"b")
=> "c" where x = ("a"|"b")
=> "c"
```
since we never ever needed the value of `x`.

There are a lot of other even more interesting examples when we take into account that GF also supports record types and parameter types. Consider this:
```GF
   (\x -> x.s1+x.s1) {s1="s"; s2="a"|"b"}
=> x.s1+x.s1 where x = {s1="s"; s2="a"|"b"}
=> "s"+"s"
=> "ss"
```
Here when we encounter `x.s1`, we must evaluate `x` and then its field `s1` but not `s2`. Therefore, there is only one variant. On the other hand, here:
```GF
   (\x -> x.s2+x.s2) {s1="s"; s2="a"|"b"}
=> x.s2+x.s2 where x = {s1="s"; s2="a"|"b"}

-- Branch 1
x.s2+x.s2 where x = {s1="s"; s2="a"}
"a"+"a"
"aa"

-- Branch 2
x.s2+x.s2 where x = {s1="s"; s2="b"}
"b"+"b"
"bb"
```
we branch only after encountering the variant in the `s2` field.

The implementation for variants requires the introduction of a nondeterministic monad with a support for logical variables. See this [paper](https://gup.ub.gu.se/file/207634):

Claessen, Koen & Ljunglöf, Peter. (2000). Typed Logical Variables in Haskell. Electronic Notes Theoretical Computer Science. 41. 37. 10.1016/S1571-0661(05)80544-4.

for possible implementations. Our concrete implemention is built on top of the `ST` monad in Haskell and provides the primitives:
```Haskell
newThunk :: Env s -> Term -> EvalM s (Thunk s)
newEvaluatedThunk :: Value s -> EvalM s (Thunk s)
force :: Thunk s -> EvalM s (Value s)
msum :: [EvalM s a] -> EvalM s a
```
Here, a `Thunk` is either an unevaluated term or an already computed value. Internally, it is implement as an `STRef`. If the thunk is unevaluated, it can be forced to an evaluated state by calling `force`. In addition, `msum` makes it possible to nondeterministically branch into a list of possible actions.

The terms and the values in the extended language are similar with two exceptions. We add the constructor `FV` for encoding variants in the terms, and the constructors for values now take lists of thunks instead of values:
```Haskell
data Term
  = Vr Ident           -- i.e. variables: x,y,z ...
  | Cn Ident           -- i.e. constructors: cons, nil, etc.
  | App Term Term      -- i.e. function application: @f x@
  | Abs Ident Term     -- i.e. \x -> t
  | FV [Term]          -- i.e. a list of variants: t1|t2|t3|...

type Env s = [(Ident,Thunk s)]
data Value s
  = VApp Ident [Thunk s]   -- i.e. constructor application
  | VClosure (Env s) Term  -- i.e. a closure contains an environment and the term for a lambda abstraction
  | VGen Int [Thunk s]     -- i.e. an internal representation for free variables
```

```Haskell
eval env (Vr x)          args = do tnk <- lookup x env
                                   v   <- force tnk
                                   apply v args
eval env (Cn c)          args = return (VApp c args)
eval env (App t1 t2)     args = do tnk <- newThunk env t2
                                   eval env t1 (tnk : args)
eval env (Abs x t)         [] = return (VClosure env (Abs x t))
eval env (Abs x t) (arg:args) = eval ((x,arg):env) t args
eval env (FV ts)         args = msum [eval env t args | t <- ts]

apply (VApp f  vs)                   args = return (VApp f (vs++args))
apply (VClosure env (Abs x t)) (arg:args) = eval ((x,arg):env) t args
apply (VGen i  vs)                   args = return (VGen i (vs++args))
```

```Haskell
value2term i (VApp c tnks) =
  foldM (\t tnk -> fmap (App t) (force tnk >>= value2term i)) (Cn c) tnks
value2term i (VGen j tnks) =
  foldM (\t tnk -> fmap (App t) (force tnk >>= value2term i)) (Vr ('v':show j)) tnks
value2term i (VClosure env (Abs x t)) = do
  tnk <- newEvaluatedThunk (VGen i [])
  v <- eval ((x,tnk):env) t []
  t <- value2term (i+1) v
  return (Abs ('v':show i) t)

normalForm gr t =
  case runEvalM gr (eval [] t [] >>= value2term 0) of
    [t] -> t
    ts  -> FV ts
```

# Meta Variables
