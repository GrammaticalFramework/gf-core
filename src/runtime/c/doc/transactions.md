# Transactions

The .ngf files that the runtime creates are actual databases which are used to get quick access to the grammars. Like in any database, we also make it possible to dynamically change the data. In our case this means that we can add and remove functions and categories at any time. Moreover, any changes happen in transactions which ensure that changes are not visible until the transaction is commited. The rest of the document describes how the transactions are implemented.

# Databases and Functional Language

The database model is specifically designed to be friendly towards pure functional languages like Haskell.
