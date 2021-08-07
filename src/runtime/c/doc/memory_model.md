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
relative to the beginning of the area. On other words, if normally we would have had a pointer `p`, now we have the offset `o` which is converted to a
pointer by using `current_base+o`.

Writing explicitly the pointer arithmetics and the corresponding typecasts, each time when we dereference a pointer, is too tedious and verbose. Instead,
we use the operator overloading in C++. There is the type `ref<A>` which wraps around a file offset to a data item of type `A`. The operators `->` and `*`
are overloaded for that type and they do the necessary pointer arithmetics and type casts.

This solves the problem with code readability but creates another problem. How do `->` and `*` know the address of the memory mapped area? Obviously,
`current_base` must be a static variable and there must be a way to initialize that variable.

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
