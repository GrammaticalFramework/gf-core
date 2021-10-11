# Data Marshalling Strategies

The runtime is designed to be used from a high-level programming language, which means that there are frequent foreign calls between the host language and C. This also implies that all the data must be frequently marshalled between the binary representations of the two languages. This is usually trivial and well supported for primitive types like numbers and strings but for complex data structures we need to design our own strategy.

The most central data structure in GF is of course the abstract syntax expression. The other two secondary but closely related structures are types and literals. These are complex structures and no high-level programming language will let us to manipulate them directly unless if they are in the format that the runtime of the language understands. There are three main strategies to deal with complex data accross a language boundry:

1. Keep the data in the C world and provide only an opaque handle to the host language. This means that all operations over the data must be done in C via foreign calls.
2. Design a native host-language representation. For each foreign call the data is copied from the host language to the C representation and vice versa. Copying is obviously bad, but not too bad if the data is small. The added benefit is that now both languages have first-class access to the data. As a bonus, the garbage collector of the host language now understands the data and can immediately release it if part of it becomes unreachable.
3. Keep the data in the host language. The C code has only an indirect access via opaque handles and calls back to the host language. The program in the host language has first-class access and the garbage collector can work with the data. No copying is needed.

The old C runtime used option 1. Obviously, this means that abstract expressions cannot be manipulated directly, but this is not the only problem. When the application constructs abstract expressions from different pieces, a whole a lot of overhead is added. First, the design was such that data in C must always be allocated from a memory pool. This means that even if we want to make a simple function application, we first must allocate a pool which adds memory overhead. In addition, the host language must allocate an object which wraps arround the C structure. The net effect is that while the plain abstract function application requires the allocation of only two pointers, the actually allocated data may be several times bigger if the application builds the expression piece by piece. The situation is better if the expression is entirely created from the runtime and the application just needs to keep a reference to it.

Another problem is that when the runtime has to create a whole bunch of expressions, for instance as a result from parsing or random and exhaustive generation, then all the expressions are allocated in the same memory pool. The application gets separate handles to each of the produced expressions, but the memory pool is released only after all of the handles become unreachable. Obviously the problem here is that different expressions share the same pool. Unfortunately this is hard to avoid since although the expressions are different, they usually share common subexpression. Identifying the shared parts would be expensive and at the end it might mean that each expression node must be allocated in its own pool.

The path taken in the new runtime is a combination of strategies 2 and 3. The abstract expressions are stored in the heap of the host language and use a native for that language representation.

# Abstract Expressions in Different Languages

In Haskell, abstract expressions are represented with an algebraic data type:
```Haskell
data Expr =
   EAbs BindType Var Expr
 | EApp Expr Expr
 | ELit Literal
 | EMeta  MetaId
 | EFun   Fun
 | EVar   Int
 | ETyped Expr Type
 | EImplArg Expr
```
while in Python and all other object-oriented languages an expression is represented with objects of different classes:
```Python
class Expr: pass
class ExprAbs(Expr): pass
class ExprApp(Expr): pass
class ExprLit(Expr): pass
class ExprMeta(Expr): pass
class ExprFun(Expr): pass
class ExprVar(Expr): pass
class ExprTyped(Expr): pass
class ExprImplArg(Expr): pass
```

The runtime needs its own representation as well but only when an expression is stored in a .ngf file. This happens for instance with all types in the abstract syntax of the grammar. Since the type system allows dependent types, some type signature might contain expressions too. Another appearance for abstract expressions is in function definitions, i.e. in the def rules.

Expressions in the runtime are represented with C structures which on the other hand may contain tagged references to other structures. The lowest four bits of each reference encode the type of structure that it points to, while the rest contain the file offsets in the memory mapped file. For example, function application is represented as:
```C++
struct PgfExprApp {
  static const uint8_t tag = 1;

  PgfExpr fun;
  PgfExpr arg;
};
```
Here the constant `tag` says that any reference to a PgfExprApp structure must contain the value 1 in its lowest four bits. The fields `fun` and `arg` refer to the function and the argument for that application. The type PgfExpr is defined as:
```C++
typedef uintptr_t object;
typedef object PgfExpr;
```
In order to dereference an expression, we first neeed to pattern match and then obtain a `ref<>` object:
```C++
switch (ref<PgfExpr>::get_tag(e)) {
...
case PgfExprApp::tag: {
    auto eapp = ref<PgfExprApp>::untagged(e);
    // do something with eapp->fun and eapp->arg
    ...
    break;
}
...
}
```

The representation in the runtime is internal and should never be exposed to the host language. Moreover, these structures live in the memory mapped file and as we discussed in Section "[Memory Model](memory_model.md)" accessing them requires special care. This also means that occasionally the runtime must make a copy from the native representation to the host representation and vice versa. For example, function:
```Haskell
functionType :: PGF -> Fun -> Maybe Type
```
must look up the type of an abstract syntax function in the .ngf file and return its type. The type, however, is in the native representation and it must first be copied in the host representation. The converse also happens. When the compiler wants to add a new abstract function to the grammar, it creates its type in the Haskell heap, which the runtime later copies to the native representation in the .ngf file. This is not much different from any other database. The database file usually uses a different data representation than what the host language has.

In most other runtime operations, copying is not necessary. The only thing that the runtime needs to know is how to create new expressions in the heap of the host and how to pattern match on them. For that it calls back to code implemented differently for each host language. For example in:
```Haskell
readExpr :: String -> Maybe Expr
```
the runtime knows how to read an abstract syntax expression, while for the construction of the actual value it calls back to Haskell. Similarly:
```Haskell
showExpr :: [Var] -> Expr -> String
```
uses code implemented in Haskell to pattern match on the different algebraic constructors, while the text generation itself happens inside the runtime.

# Marshaller and Unmarshaller

The marshaller and the unmarshaller are the two key data structures which bridge together the different representation realms for abstract expressions and types. The structures have two equivalent definitions, one in C++:
```C++
struct PgfMarshaller {
    virtual object match_lit(PgfUnmarshaller *u, PgfLiteral lit)=0;
    virtual object match_expr(PgfUnmarshaller *u, PgfExpr expr)=0;
    virtual object match_type(PgfUnmarshaller *u, PgfType ty)=0;
};

struct PgfUnmarshaller {
    virtual PgfExpr eabs(PgfBindType btype, PgfText *name, PgfExpr body)=0;
    virtual PgfExpr eapp(PgfExpr fun, PgfExpr arg)=0;
    virtual PgfExpr elit(PgfLiteral lit)=0;
    virtual PgfExpr emeta(PgfMetaId meta)=0;
    virtual PgfExpr efun(PgfText *name)=0;
    virtual PgfExpr evar(int index)=0;
    virtual PgfExpr etyped(PgfExpr expr, PgfType typ)=0;
    virtual PgfExpr eimplarg(PgfExpr expr)=0;
    virtual PgfLiteral lint(size_t size, uintmax_t *v)=0;
    virtual PgfLiteral lflt(double v)=0;
    virtual PgfLiteral lstr(PgfText *v)=0;
    virtual PgfType dtyp(int n_hypos, PgfTypeHypo *hypos,
                         PgfText *cat,
                         int n_exprs, PgfExpr *exprs)=0;
    virtual void free_ref(object x)=0;
};
```
and one in C:
```C
typedef struct PgfMarshaller PgfMarshaller;
typedef struct PgfMarshallerVtbl PgfMarshallerVtbl;
struct PgfMarshallerVtbl {
    object (*match_lit)(PgfUnmarshaller *u, PgfLiteral lit);
    object (*match_expr)(PgfUnmarshaller *u, PgfExpr expr);
    object (*match_type)(PgfUnmarshaller *u, PgfType ty);
};
struct PgfMarshaller {
    PgfMarshallerVtbl *vtbl;
};

typedef struct PgfUnmarshaller PgfUnmarshaller;
typedef struct PgfUnmarshallerVtbl PgfUnmarshallerVtbl;
struct PgfUnmarshallerVtbl {
    PgfExpr (*eabs)(PgfUnmarshaller *this, PgfBindType btype, PgfText *name, PgfExpr body);
    PgfExpr (*eapp)(PgfUnmarshaller *this, PgfExpr fun, PgfExpr arg);
    PgfExpr (*elit)(PgfUnmarshaller *this, PgfLiteral lit);
    PgfExpr (*emeta)(PgfUnmarshaller *this, PgfMetaId meta);
    PgfExpr (*efun)(PgfUnmarshaller *this, PgfText *name);
    PgfExpr (*evar)(PgfUnmarshaller *this, int index);
    PgfExpr (*etyped)(PgfUnmarshaller *this, PgfExpr expr, PgfType typ);
    PgfExpr (*eimplarg)(PgfUnmarshaller *this, PgfExpr expr);
    PgfLiteral (*lint)(PgfUnmarshaller *this, size_t size, uintmax_t *v);
    PgfLiteral (*lflt)(PgfUnmarshaller *this, double v);
    PgfLiteral (*lstr)(PgfUnmarshaller *this, PgfText *v);
    PgfType (*dtyp)(PgfUnmarshaller *this,
                    int n_hypos, PgfTypeHypo *hypos,
                    PgfText *cat,
                    int n_exprs, PgfExpr *exprs);
    void (*free_ref)(PgfUnmarshaller *this, object x);
};
struct PgfUnmarshaller {
    PgfUnmarshallerVtbl *vtbl;
};
```
Which one you will get, depends on whether you import `pgf/pgf.h` from C or C++.

As we can see, most of the arguments for the different methods are of type `PgfExpr`, `PgfType` or `PgfLiteral`. These are all just type synonyms for the type `object`, which on the other hand is nothing else but a number with enough bits to hold an address if necessary. The interpretation of the number depends on the realm in which the object lives. The following table shows the interpretations for four languages as well as the one used internally in the .ngf files:
|          | PgfExpr        | PgfLiteral        | PgfType        |
|----------|----------------|-------------------|----------------|
| Haskell  | StablePtr Expr | StablePtr Literal | StablePtr Type |
| Python   | ExprObject *   | PyObject *        | TypeObject *   |
| Java     | jobject        | jobject           | jobject        |
| .NET     | GCHandle       | GCHandle          | GCHandle       |
| internal | file offset    | file offset       | file offset    |

The marshaller is the structure that lets the runtime to pattern match on an expression. When one of the match methods is executed, it checks the kind of expr, literal or type and calls the corresponding method from the unmarshaller which it gets as an argument. The method on the other hand gets as arguments the corresponding sub-expressions and attributes.

Generally the role of an unmarshaller is to construct things. For example, the variable `unmarshaller` in `PGF2.FFI` is an object which can construct new expressions in the Haskell heap from the already created children. Function `readExpr`, for instance, passes that one to the runtime to instruct it that the result must be in the Haskell realm.

Constructing objects is not the only use of an unmarshaller. The implementation of `showExpr` passes to `pgf_print_expr` an abstract expression in Haskell and the `marshaller` defined in PGF2.FFI. That marshaller knows how to pattern match on Haskell expressions and calls the right methods from whatever unmarhaller is given to it. What it will get in that particular case is a special unmarshaller which does not produce new representations of abstract expressions, but generates a string.


# Literals

Finally, we should have a few remarks about how values of the literal types `String`, `Int` and `Float` are represented in the runtime.

`String` is represented as the structure:
```C
typedef struct {
    size_t size;
    char text[];
} PgfText;
```
Here the first field is the size of the string in number of bytes. The second field is the string itself, encoded in UTF-8. Just like in most modern languages, the string may contain the zero character and that is not an indication for end of string. This means that functions like `strlen` and `strcat` should never be used when working with PgfText. Despite that the text is not zero terminated, the runtime always allocates one more last byte for the text content and sets it to zero. That last byte is not included when calculating the field `size`. The purpose is that with that last zero byte the GDB debugger knows how to show the string properly. Most of the time, this doesn't incur any memory overhead either since `malloc` always allocates memory in size divisible by the size of two machine words. The consequence is that usually there are some byte left unused at the end of every string anyway.

`Int` is like the integers in Haskell and Python and can have arbitrarily many digits. In the runtime, the value is represented as an array of `uintmax_t` values. Each of these values contains as many decimal digits as it is possible to fit in `uintmax_t`. For example on a 64-bit machine, 
the maximal value that fits is 18446744073709551616. However, the left-most digit here is at most 1, this means that if we want to represend an arbitrary sequence of digits, the maximal length of the sequence must be at most 19. Similarly on a 32-bit machine each value in the array will store 9 decimal digits. Finally the sign of the number is stored as the sign of the first number in the array which is always threated as `intmax_t`.

Just to have an example, the number `-774763251095801167872` is represented as the array `{-77, 4763251095801167872}`. Note that this representation is not at all suitable for implementing arithmetics with integers, but is very simple to use for us since the runtime only needs to to parse and linearize numbers.

`Float` is trivial and is just represented as the type `double` in C/C++. This can also be seen in the type of the method `lflt` in the unmarshaller.

