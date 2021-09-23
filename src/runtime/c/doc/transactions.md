# Transactions

The .ngf files that the runtime creates are actual databases which are used to get quick access to the grammars. Like in any database, we also make it possible to dynamically change the data. In our case this means that we can add and remove functions and categories at any time. Moreover, any changes happen in transactions which ensure that changes are not visible until the transaction is commited. The rest of the document describes how the transactions are implemented.

# Databases and Functional Languages

The database model of the runtime is specifically designed to be friendly towards pure functional languages like Haskell. In a usual database, updates happen constantly and therefore executing one and the same query at different times would yield different results. In our grammar databases, queries correspond to operations like parsing, linearization and generation. This means that if we had used the usual database model, all these operations would have to be bound to the IO monad. Consider this example:
```Haskell
main = do
  gr <- readNGF "Example.ngf"
  functionType gr "f" >>= print
  <... modify the grammar gr ...>
  functionType gr "f" >>= print
```
Here we ask for the type of a function before and after an arbitrary update in the grammar `gr`. Obviously if we allow that then `functionType` would have to be in the IO monad, e.g.:
```Haskell
functionType :: PGF -> Fun -> IO Type
```

Although this is a possible way to go, it would mean that the programmer would have to do all grammar related work in the IO. This is not nice and against the spirit of functional programming. Moreover, all previous implementations of the runtime have assumed that most operations are pure. If we go along that path then this will cause a majour breaking change.

Fortunately there is an alternative. Read-only operations remain pure functions, but then any update should create a new revision of the database rather than modifying the existing one. Compare this example with the previous:
```Haskell
main = do
  gr <- readNGF "Example.ngf"
  print (functionType gr "f")
  gr2 <- modifyPGF gr $ do
           -- do all updates here
  print (functionType gr2 "f")
```
Here `modifyPGF` allows us to do updates but the updates are performed on a freshly created clone of the grammar `gr`. The original grammar is never ever modified. After the changes the variable `gr2` is a reference to the new revision. While the transaction is in progress we cannot see from the currently changing revision, and therefore all read-only operations can remain pure. Only after the transaction is complete then we get to use `gr2` which would not change anymore.

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

**TODO: Interprocess synhronization is still not implemented**

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
Here the first call to `functionType` returns the old type of "f", while the second call retrives the type after the updates.

# Branches

# Implementation
## Persistent Data Structures
## Garbage Collection
## Atomicity
