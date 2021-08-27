# Data Marshalling Strategies

The runtime is designed to be used from a high-level programming language, which means that there are frequent foreign calls between the host language and C. This also implies that all the data must be frequently marshalled between the binary representations of the two languages. This is usually trivial and well supported for primitive types like numbers and strings but for complex data structures we need to design our own strategy.

The most central data structure in GF is of course the abstract syntax expression. The other two secondary but closely related structures are types and literals. These are complex structures and no high-level programming language will let us to manipulate them directly unless if they are in the format that the runtime of the language understands. There are three main strategies to deal with complex data accross a language boundry:

1. Keep the data in the C world and provide only an opaque handle to the host language. This means that all operations over the data must be done in C via foreign calls.
2. Design a native host language representation. For each foreign call the data is copied from the host language to the C representation and vice versa. Copying is obviously bad, but not too bad if the data is small. The added benefit is that now both languages have first-class access to the data. As a bonus, the garbage collector of the host language now understands the data and can immediately release it if part of it becomes unreachable.
3. Keep the data in the host language. The C code has only an indirect access via opaque handles and callbacks to the host language. The program in the host language has first-class access and the garbage collector can work with the data. No copying is needed.

The old C runtime used option 1. Obviously, this means that abstract expressions cannot be manipulated directly, but this is not the only problem. When the application constructs abstract expressions from different pieces, a whole a lot of overhead is added. First, the design was such that data in C must always be allocated from a memory pool. This means that even if we want to make a simple function application, we first must allocate a pool which adds memory overhead. In addition the host language must allocate an object which wraps arround the data in C. The net effect is that while the plain function application requires the allocation of only two pointers, the actually allocated data may be several times bigger if the application builds the expression piece by piece. The situation is better if the expression is entirely created from the runtime and the application just needs to keep a reference to it.

Another problem is that when the runtime must create a whole bunch of expressions, for instance as a result from parsing or random and exhaustive generation, then all the expressions are allocated in the same memory pool. The application gets separate handles to each of the produced expressions, but the memory pool is released only after all of the handles become unreachable. Obviously the problem here is that different expressions share the same pool. Unfortunately this is hard to avoid since although the expressions are different, they usually share common subexpression. Identifying the shared subexpression would be expensive and at the end it might mean that each expression node must be allocated in its own pool.

|         | PgfExpr        | PgfLiteral        | PgfType        |
|---------|----------------|-------------------|----------------|
| Haskell | StablePtr Expr | StablePtr Literal | StablePtr Type |
| Python  | ExprObject *   | PyObject *        | TypeObject *   |
| Java    | jobject        | jobject           | jobject        |
| NGF     | file offset    | file offset       | file offset    |
