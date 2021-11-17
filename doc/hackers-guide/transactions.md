# Transactions

The `.ngf` files that the runtime creates are actual databases which are used to get quick access to the grammars. Like in any database, we also make it possible to dynamically change the data. In our case this means that we can add and remove functions and categories at any time. Moreover, any changes happen in transactions which ensure that changes are not visible until the transaction is commited. The rest of the document describes how the transactions are implemented.

# Databases and Functional Languages

The database model of the runtime is specifically designed to be friendly towards pure functional languages like Haskell. In a usual database, updates happen constantly and therefore executing one and the same query at different times would yield different results. In our grammar databases, queries correspond to operations like parsing, linearization and generation. This means that if we had used the usual database model, all these operations would have to be bound to the IO monad. Consider this example:
```Haskell
main = do
  gr <- readNGF "Example.ngf"
  functionType gr "f" >>= print
  -- modify the grammar gr
  functionType gr "f" >>= print
```
Here we ask for the type of a function before and after an arbitrary update in the grammar `gr`. Obviously if we allow that, then `functionType` would have to be in the IO monad, e.g.:

```Haskell
functionType :: PGF -> Fun -> IO Type
```

Although this is a possible way to go, it would mean that the programmer would have to do all grammar related work in the IO. This is not nice and against the spirit of functional programming. Moreover, all previous implementations of the runtime have assumed that most operations are pure. If we go along that path then this will cause a major breaking change.

Fortunately there is an alternative. Read-only operations remain pure functions, but any update should create a new revision of the database rather than modifying the existing one. Compare this example with the previous:
```Haskell
main = do
  gr <- readNGF "Example.ngf"
  print (functionType gr "f")
  gr2 <- modifyPGF gr $ do
           -- do all updates here
  print (functionType gr2 "f")
```
Here `modifyPGF` allows us to do updates but the updates are performed on a freshly created clone of the grammar `gr`. The original grammar is never ever modified. After the changes the variable `gr2` is a reference to the new revision. While the transaction is in progress we cannot see the currently changing revision, and therefore all read-only operations can remain pure. Only after the transaction is complete, do we get to use `gr2`, which will not allowed to change anymore.

Note also that above `functionType` is used with its usual pure type:
```Haskell
functionType :: PGF -> Fun -> Type
```
This is safe since the API never exposes database revisions which are not complete. Furthermore, the programmer is free to keep several revisions of the same database simultaneously. In this example:
```Haskell
main = do
  gr <- readNGF "Example.ngf"
  gr2 <- modifyPGF gr $ do
           -- do all updates here
  print (functionType gr "f", functionType gr2 "f")
```
The last line prints the type of function `"f"` in both the old and the new revision. Both are still available.

The API as described so far would have been complete if all updates were happening in a single thread. In reality we can expect that there might be several threads or processes modifying the database. The database ensures a multiple readers/single writer exclusion but this doesn't mean that another process/thread cannot modify the database while the current one is reading an old revision. In a parallel setting, `modifyPGF` first merges the revision which the process is using with the latest revision in the database. On top of that the specified updates are performed. The final revision after the updates is returned as a result.

**TODO: Merges are still not implemented.**

The process can also ask for the latest revision by calling `checkoutPGF`, see bellow.

# Databases and Imperative Languages

In imperative languages, the state of the program constantly changes and the considerations in the last section do not apply. All read-only operations always work with the latest revision. Bellow is the previous example translated to Python:
```Python
gr = readNGF("Example.ngf")
print(functionType(gr,"f"))
with gr.transaction() as t:
  # do all updates here by using t
print(functionType(gr,"f"))
```
Here the first call to `functionType` returns the old type of "f", while the second call retrives the type after the updates. The transaction itself is initiated by the `with` statement. Inside the with statement `gr` will still refer to the old revision since the new one is not complete yet. If the `with` statement is finished without exceptions then `gr` is updated to point to the new one. If an exception occurs then the new revision is discarded, which corresponds to a transaction rollback. Inside the `with` block, the object `t` of type `Transaction` provides methods for modifying the data.

# Branches

Since the database already supports revisions, it is a simple step to support branches as well. A branch is just a revision with a name. When you open a database with `readNGF`, the runtime looks up and returns the revision (branch) with name `master`. There might be other branches as well. You can retrieve a specific branch by calling:
```Haskell
checkoutPGF :: PGF -> String -> IO (Maybe PGF)
```
Here the string is the branch name. New branches can be created by using:
```Haskell
branchPGF :: PGF -> String -> Transaction a -> IO PGF
```
Here we start with an existing revision, apply a transaction and store the result in a new branch with the given name.

# Implementation

In this section we summarize important design decisions related to the internal implementation.

## API
The low-level API for transactions consists of only four functions:
```C
PgfRevision pgf_clone_revision(PgfDB *db, PgfRevision revision,
                               PgfText *name,
                               PgfExn *err);

void pgf_free_revision(PgfDB *pgf, PgfRevision revision);

void pgf_commit_revision(PgfDB *db, PgfRevision revision,
                         PgfExn *err);

PgfRevision pgf_checkout_revision(PgfDB *db, PgfText *name,
                                  PgfExn *err);
```
Here `pgf_clone_revision` makes a copy of an existing revision and — if `name` is not `NULL` — changes its name. The new revision is transient and exists only until it is released with `pgf_free_revision`. Transient revisions can be updated with the API for adding functions and categories. To make a revision persistent, call `pgf_commit_revision`. After the revision is made persistent it will stay in the database even after you call `pgf_free_revision`. Moreover, it will replace the last persistent revision with the same name. The old revision will then become transient and will exist only until all clients call `pgf_free_revision` for it.

Persistent revisions can never be updated. Instead you clone it to create a new transient revision. That one is updated and finally it replaces the existing persistent revision.

This design for transactions may sound unusual but it is just another way to present the copy-on-write strategy. There instead of transaction logs, each change to the data is written in a new place and the result is made available only after all changes are in place. This is for instance what the [LMDB](http://www.lmdb.tech/doc/) (Lightning Memory-Mapped Database) does and it has also served as an inspiration for us.

## Functional Data Structures

From an imperative point of view, it may sound wasteful that a new copy of the grammar is created for each transaction. Functional programmers on the other hand know that with a functional data structure, you can make a copy which shares as much of the data with the original as possible. Each new version copies only those bits that are different from the old one. For example the main data structure that we use to represent the abstract syntax of a grammar is a size-balanced binary tree as described by:

- Stephen Adams, "Efficient sets: a balancing act", Journal of Functional Programming 3(4):553-562, October 1993, http://www.swiss.ai.mit.edu/~adams/BB/.

- J. Nievergelt and E.M. Reingold, "Binary search trees of bounded balance", SIAM journal of computing 2(1), March 1973.

This is also the same algorithm used by Data.Map in Haskell. There are also other possible implementations (B-Trees for instance), and they may be considered if the current one turns our too inefficient.


## Garbage Collection

We use reference counting to keep track of which objects should be kept alive. For instance, `pgf_free_revision` knows that a transient revision should be removed only when its reference count reaches zero. This means that there is no process or thread using it. The function also checks whether the revision is persistent. Persistent revisions are never removed since they can always be retrieved with `checkoutPGF`.

Clients are supposed to correctly use `pgf_free_revision` to indicate that they don't need a revision any more. Unfortunately, this is not always possible to guarantee. For example many languages with garbage collection call `pgf_free_revision` from a finalizer method. In some languages, however, the finalizer is not guaranteed to be executed if the process terminates before the garbage collection is done. Haskell is one of those languages. Even in languages with reference counting like Python, the process may get killed by the operating system and then the finalizer may still not be executed.

The solution is that we count on the database clients to correctly report when a revision is not needed. In addition, to be on the safe side, on a fresh database restart we explictly clean all leftover transient revisions. This means that even if a client is killed or if it does not correctly release its revisions, the worst that can happen is a memory leak until the next restart. Here by fresh restart we mean a situation where a process opens a database which is not used by anyone else. In order to detect that case we maintain a list of processes who currently have access to the file. While a new process is added, we also remove all processes in the list who are not alive anymore. If at the end the list contains only one element, then this is a fresh restart.

## Inter-process Communication

One and the same database may be opened by several processes. In that case, each process creates a mapping of the database into his own address space. The mapping is shared, which means that if a page from the database gets loaded in memory, it is loaded in a single place in the physical memory. The physical memory is then assigned possibly different virtual addresses in each process. All processes can read the data simultaneously, but if we let them to change it at the same time, all kinds of problems may happen. To avoid that, we store a single-writer/multiple-readers lock in the database file, which the processes use for synchronization.

## Atomicity

The transactions serve two goals. First they make it possible to isolate readers from seeing unfinished changes from writers. Second, they ensure atomicity. A database change should be either completely done or not done at all. The use of transient revisions ensures the isolation but the atomicity is only partly taken care of.

Think about what happens when a writer starts updating a transient revision. All the data is allocated in a memory mapped file. From the point of view of the runtime, all changes happen in memory. When all is done, the runtime calls `msync` which tells the kernel to flush all dirty pages to disk. The problem is that the kernel is also free to flush pages at any time. For instance, if there is not enough memory, it may decide to swap out pages earlier and reuse the released physical space to swap in other virtual pages. This would be fine if the transaction eventually succeeds. However, if this doesn't happen then the image in the file is already changed.

We can avoid the situation by calling [mlock](https://man7.org/linux/man-pages/man2/mlock.2.html) and telling the kernel that certain pages should not be swapped out. The question is which pages to lock. We can lock them all, but this is too much. That would mean that as soon as a page is touched it will never leave the physical memory. Instead, it would have been nice to tell the kernel -- feel free to swap out clean pages but, as soon as they get dirty, keep them in memory until further notice. Unfortunately there is no way to do that directly.

The work around is to first use [mprotect](https://man7.org/linux/man-pages/man2/mprotect.2.html) and keep all pages as read-only. Any attempt to change a page will cause segmentation fault which we can capture. If the change happens during a transaction then we can immediate lock the page and add it to the list of modified pages. When a transaction is successful we sync all modified pages. If an attempt to change a page happens outside of a transaction, then this is either a bug in the runtime or the client is trying to change an address which it should not change. In any case this prevents unintended changes in the data.


**TODO: atomicity is not implemented yet**
