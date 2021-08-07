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

