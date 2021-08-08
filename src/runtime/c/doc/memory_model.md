# The different storage files

The purpose of the .ngf files is to be used as on disk databases that store grammars. Their format is platform dependent and they should not be copied from
one platform to another. In contrast the .pgf files are platform independent and can be moved around. The runtime can import a .pgf file and create an .ngf file.
Conversely a .pgf file can be exported from an already existing .ngf file.

The internal relation between the two file is more interesting. The runtime uses its own memory allocator which always allocates memory from a memory mapped file.
The file may be explicit or an annonymous one. The .ngf is simply a memory image saved in a file. This means that loading the file is always immediate. 
You just create a new mapping and the kernel will load memory pages on demand.

On the other hand a .pgf file is a version of the grammar serialized in a platform independent format. This means that loading this type of file is always slower.
Fortunately, you can always create an .ngf file from it to speedup later reloads.

The runtime has three ways to load a grammar:

* loading a .pgf:
```Haskell
readPGF :: FilePath -> IO PGF
```
This loads the .pgf into an annonymous memory mapped file. In practice, this means that instead of allocating memory from an explicit file, the runtime will still
use the normal swap file.

* loading a .pgf and booting a new .ngf:
```Haskell
bootPGF :: FilePath -> FilePath -> IO PGF
```
The grammar is loaded from a .pgf (the first argument) and the memory is mapped to an explicit .ngf (second argument). The .ngf file is created by the function
and a file with the same name should not exist before the call.

* loading an existing memory image:
```Haskell
readNGF :: FilePath -> IO PGF
```
Once an .ngf file exists, it can be mapped back to memory by using this function. This call is always guaranteed to be fast. The same function can also
create new empty .ngf files. If the file does not exist, then a new one will be created which contains an empty grammar. The grammar could then be extended
by dynamically adding functions and categories.

# The content of an .ngf file

The .ngf file is a memory image but this is not the end of the story. The problem is that there is no way to control at which address the memory image would be
mapped. On Posix systems, `mmap` takes as hint the mapping address but the kernel may choose to ignore it. There is also the flag MAP_FIXED, which makes the hint
into a constraint, but then the kernel may fail to satisfy the constraint. For example that address may already be used for something else. Furthermore, if the
same file is mapped from several processes (if they all load the same grammar), it would be difficult to find an address which is free in all of them. 
Last but not least using `MAP_FIXED` is considered a security risk.

Since the start address of the mapping can change, using traditional memory pointers withing the mapped area is not possible. The only option is to use offsets
relative to the beginning of the area. In other words, if normally we would have written `p->x`, now we have the offset `o` which we must use like this:
```C++
((A*) (current_base+o))->x
```

Writing the explicit pointer arithmetics and typecasts, each time when we dereference a pointer, is not better than Vogon poetry and it
becomes worse when using a chain of arrow operators. The solution is to use the operator overloading in C++. 
There is the type `ref<A>` which wraps around a file offset to a data item of type `A`. The operators `->` and `*`
are overloaded for the type and they do the necessary pointer arithmetics and type casts.

This solves the problem with code readability but creates another problem. How do `->` and `*` know the address of the memory mapped area? Obviously,
`current_base` must be a global variable and there must be a way to initialize it. More specifically it must be thread local to allow different threads to
work without collisions.

A database (a memory mapped file) in the runtime is represented by the type `DB`. Before any of the data in the database is accessed, the database must
be brought into scope. Bringing into scope means that `current_base` is initialized to point to the mapping area for that database. After that any dereferencing
of a reference will be done relative to the corresponding database. This is how scopes are defined:
```C++
{
    DB_scope scope(db, READER_SCOPE);
    ...
}
```
Here `DB_scope` is a helper type and `db` is a pointer to the database that you want to bring into scope. The constructor for `DB_scope` saves the old value
for `current_base` and then sets it to point to the area of the given database. Conversely the destructor, restores the previous value. 

The use of `DB_scope` is reentrant, i.e. you can do this:
```C++
{
    DB_scope scope(db1, READER_SCOPE);
    ...
    {
        DB_scope scope(db2, READER_SCOPE);
        ...
    }
    ...
}
```
What you can't do is to have more than one database in scope simultaneously. Fortunately, that is not needed. All API functions start a scope
and the internals of the runtime always work with the current database in scope.

Note the flag `READER_SCOPE`. You can use either `READER_SCOPE` or `WRITER_SCOPE`. In addition to selecting the database, the DB_scope also enforces,
the single writer, multiple readers policy. The main problem is that a writer may have to enlarge the current file, which consequently may mean
that the kernel should relocate the mapping area to a new address. If there are readers at the same time, they way break since they expect that the mapped
area is at a particular location.

# Developing Writers

There is one important complication when developing procedures modifying the database. Every call to `DB::malloc` may potentially have to enlarge the mapped area
which sometimes leads to changing `current_base`. That would not have been a problem if GCC was not sometimes caching variables in registers. Look at the following code:
```C++
p->r = foo();
```
Here `p` is a reference which is used to access another reference `r`. On the other hand, `foo()` is a procedure which directly or indirectly calls `DB::malloc`.
GCC compiles assignments by first computing the address to modify, and then it evaluates the right hand side. This means that while `foo()` is beeing evaluated the address computed on the left-hand side is saved in a register or somewhere in the stack. But now, if it happens that the allocation in `foo()` has changed
`current_base`, then the saved address is no longer valid.

That first problem is solved by overloading the assignment operator for `ref<A>`:
```C++
ref<A>& operator= (const ref<A>& r) {
    offset = r.offset;
    return *this;
}
```
On a first sight, nothing special happens here and it looks like the overloading is redundant. However, now the assignments are compiled in a very different way.
The overloaded operator is inlined, so there is no real method call and we don't get any overhead. The real difference is that now, whatever is on the left-hand side of the assignment becomes the value of the `this` pointer, and `this` is always the last thing to be evaluated in a method call. This solves the problem.
`foo()` is evaluated first and if it changes `current_base`, the change will be taken into account when computing the left-hand side of the assignment.
    
Unfortunately, this is not the only problem. A similar thing happens when the arguments of a function are calls to other functions. See this:
```C++
foo(p->r,bar(),q->r)
```
Where now `bar()` is the function that do allocation. The compiler is free to keep in a register the value of `current_base` that it needs for the evaluation of
`p->r`, while it evaluates `bar()`. But if `current_base` has changed, then the saved value would be invalid while computing `q->r`. There doesn't seem to be
a work around for this. The only solution is to:
    
**Never call a function that allocates as an argument to another function**
    
Instead we call allocating functions on a separate line and we save the result in a temporary variable.

    
# Thread Local Variables

A final remark is the compilation of thread local variables. When a thread local variable is compiled in a position dependent code, i.e. in executables, it is
compiled efficiently by using the fs register which points to the thread local segment. Unfortunately, that is not the case by default for shared
libraries like our runtime. In that case, GCC applies the global-dynamic model which means that access to a thread local variable is internally implemented 
with a call to the function ´__tls_get_addr´. Since `current_base` is used all the time, this adds overhead.

The solution is to define the variable with the attribute `__attribute__((tls_model("initial-exec")))` which says that it should be treated as if it is defined
in an executable. This removes the overhead, but adds the limitation that the runtime should not be loaded with `dlopen`.
