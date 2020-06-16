## unsupported token gluing `foo + bar`

There was a problem in an expression using +, e.g. `foo + bar`.
This can be due to two causes, check which one applies in your case.

1. You are trying to use + on runtime arguments. Even if you are using
`foo + bar` in an oper, make sure that the oper isn't called in a
linearization that takes arguments. Both of the following are illegal:

    lin Test foo bar = foo.s + bar.s          -- explicit + in a lin
    lin Test foo bar = opWithPlus foo bar     -- the oper uses +

2. One of the arguments in `foo + bar` is a bound variable
from pattern matching a string, but the cases are non-exhaustive.
Example:
    case "test" of {
      x + "a" => x + "b"   -- no applicable case for "test", so x = ???
    } ;

You can fix this by adding a catch-all case in the end:
    { x + "a" => x + "b" ;
      _       => "default case" } ;

3. If neither applies to your problem, submit a bug report and we
will update the error message and this documentation.

    https://github.com/GrammaticalFramework/gf-core/issues
