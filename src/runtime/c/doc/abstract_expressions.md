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


|          | PgfExpr        | PgfLiteral        | PgfType        |
|----------|----------------|-------------------|----------------|
| Haskell  | StablePtr Expr | StablePtr Literal | StablePtr Type |
| Python   | ExprObject *   | PyObject *        | TypeObject *   |
| Java     | jobject        | jobject           | jobject        |
| .NET     | GCHandle       | GCHandle          | GCHandle       |
| internal | file offset    | file offset       | file offset    |
