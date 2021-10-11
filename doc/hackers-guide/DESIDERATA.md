This is an experiment to develop **a majestic new GF runtime**.

The reason is that there are several features that we want to have and they all require a major rewrite of the existing C runtime.
Instead of beating the old code until it starts doing what we want, it is time to start from scratch.

# New Features

The features that we want are:

- We want to support **even bigger grammars that don't fit in the main memory** anymore. Instead, they should reside on the disc and parts will be loaded on demand.
The current design is that all memory allocated for the grammars should be from memory-mapped files. In this way the only limit for the grammar size will
be the size of the virtual memory, i.e. 2^64 bytes. The swap file is completely circumvented, while all of the available RAM can be used as a cache for loading parts
of the grammar.

- We want to be able to **update grammars dynamically**. This is a highly desired feature since recompiling large grammars takes hours.
Instead, dynamic updates should happen instantly.

- We want to be able to **store additional information in the PGF**. For example that could be application specific semantic data.
Another example is to store the source code of the different grammar rules, to allow the compiler to recompile individual rules.

- We want to **allow a single file to contain slightly different versions of the grammar**. This will be a kind of a version control system,
which will allow different users to store their own grammar extensions while still using the same core content.

- We want to **avoid the exponential explosion in the size of PMCFG** for some grammars. This happens because PMCFG as a formalism is too low-level.
By enriching it with light-weight variables, we can make it more powerful and hopefully avoid the exponential explosion.

- We want to finally **ditch the old Haskell runtime** which has long outlived its time.

There are also two bugs in the old C runtime whose fixes will require a lot of changes, so instead of fixing the old runtime we do it here:

- **Integer literals in the C runtime** are implemented as 32-bit integers, while the Haskell runtime used unlimited integers.
Python supports unlimited integers too, so it would be nice to support them in the new runtime as well.

- The old C runtime assumed that **String literals are terminated with the NULL character**. None of the modern languages (Haskell, Python, Java, etc) make
that assumption, so we should drop it too.

# Consequences

The desired features will have the following implementation cosequences.

- The switch from memory-based to disc-based runtime requires one big change. Before it was easy to just keep a pointer from one object to another.
Unfortunately this doesn't work with memory-mapped files, since every time when you map a file into memory it may end up at a different virtual address.
Instead we must use file offsets. In order to make programming simpler, the new runtime will be **implemented in C++ instead of C**. This allows us to overload
the arrow operator (`->`) which will dynamically convert file offsets to in-memory pointers.

- The choice of C++ also allows us to ditch the old `libgu` library and **use STL** instead.

- The content of the memory mapped files is platform-specific. For that reason there will be two grammar representations:
     - **Native Grammar Format** (`.ngf`) - which will be instantly loadable by just mapping it to memory, but will be platform-dependent.
     - **Portable Grammar Format** (`.pgf`) - which will take longer to load but will be more compact and platform independent.
  The runtime will be able to load `.pgf` files and convert them to `.ngf`. Conversely `.pgf` can be exported from the current `.ngf`.
