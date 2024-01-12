from typing import Any

class PGF:
    """
    A class which represents a grammar loaded from a .pgf or an .ngf file.
    An object is created by calling readPGF, bootNGF, readNGF, or newNGF.
    """

    @property
    def abstractName(self) -> str:
        """
        Returns the name of the abstract syntax for the grammar.
        """
        ...

    def getAbstractFlag(self, flag_name: str) -> Any:
        """
        Looks up the value of a flag associated with the abstract syntax.
        The value can only have types int, string or float.
        If there is no flag with that name the KeyError exception is thrown.
        """
        ...
    
    def getGlobalFlag(self, flag_name: str) -> Any:
        """
        Looks up the value of a global flag associated with the entire grammar.
        The value can only have types int, string or float.
        If there is no flag with that name the KeyError exception is thrown.
        """
        ...

    def embed(self, module_name: str) -> EmbeddedGrammar:
        """
        Dynamically creates a Python module with the given name which
        exposes all abstract functions in the grammar as Python values.
        The module can be later imported as usual. Alternatively
        you can also use the return value which is the newly created module.
        """
        ...

    @property
    def categories(self) -> list[str]:
        """
        Returns a list with the names of all categories in the abstract syntax.
        """
        ...

    def categoryContext(self, category_name: str) -> Any:
        """
        Given a category name, returns a tuple with elements
        of type tuple[bool,str,Type]. Each element represents
        one of the category's argument.
        """
        ...

    def categoryProbability(self, category_name: str) -> float:
        """
        Given a category name, returns its unconditional log-probability: - log P(C).
        This is useful to combine with the conditional probability of
        a function f : ... -> C. Its probability is P(f | C), and
        from that you get P(f) = P(C) * P(f | C).
        """
        ...

    @property
    def startCat(self) -> Type:
        """
        Returns the type that represents the start category of the grammar.
        This is the parsed representation of what was specified as a value
        to the start_cat flag.
        """
        ...

    @property
    def functions(self) -> list[str]:
        """
        Returns a list with the names of all functions in the abstract syntax.
        """
        ...

    def functionsByCat(self, cat_name: str) -> list[str]:
        """
        Returns a list with the names of all functions whose return value is of
        the given category.
        """
        ...

    def functionType(self, fun_name: str) -> Type:
        """
        Looks up the type of a function of with the given name.
        Throws a KeyError if the function is not defined.
        """
        ...

    def functionIsConstructor(self, fun_name: str) -> bool:
        """
        Returns True if the function with that name is a constructor,
        and False in all other cases.
        """
        ...

    def functionProbability(self, fun_name: str) -> float:
        """
        For a function of type f : ... -> C returns the conditional
        log-probability: - log P(f | C).
        """
        ...

    def exprProbability(self, e: Expr) -> float:
        """
        Computes the conditional log-probability - log P(e | C)
        by summing the probabilities of all functions appearing
        in the tree.
        """
        ...

    def generateRandom(cat : Type, depth : int = 5) -> tuple[Expr,float]:
        """
        Generates a random abstract syntax trees of the given type.
        The depth parameter specifies the maximal distance between
        the root and a leaf in the tree. The float value in
        the result is the probability of the tree.
        """
        ...

    def newTransaction(self) -> Transaction:
        """
        Starts a new transaction which makes it possible to update
        the grammar dynamically. The method is supposed to be used
        in the with statements:

        with gr.newTransaction() as t:
            ...

        If an exception occurs in the with statement then
        the transaction is rolled back. If all operations
        succeed, the transaction is automatically commited.
        """
        ...

    def checkoutBranch(self):
        """
        If another process has changed the database in the background,
        the current process might be still using an old revision of
        the database. By checking out regularly you make sure that
        you have an up-to-date version.
        """
        ...

    @property
    def languages(self) -> dict[str, Concr]:
        """All concrete syntaxes as a dictionary"""
        ...

    def writePGF(self, file_path: str, langs=list[str]) -> None:
        """
        Writes the currently loaded grammar in a new PGF file.
        By providing the langs argument you can select which
        languages to serialize.
        """
        ...


def readPGF(file_name: str) -> PGF:
    """
    Reads a grammar from a .pgf file in the main memory.
    The PGF format is portable accross different machines 
    but is slower to read for large grammars. The size is
    also limited by the size of the memory and the size
    of the swap file.

    For large grammars, you should first boot the grammar
    to a native file format and then load it quickly by
    calling readNGF. In this way you are also not limited
    by the size of the swap file.
    """
    ...

def bootNGF(pgf_file_path: str, ngf_file_path: str) -> PGF:
    """
    Reads a .pgf file and creates a .ngf file in a native format.
    Files in a native format should not be moved accross machines
    since there is no guarantee that this will work.
    On the other hand they can be loaded immediately.

    Grammars in a native format can also be shared accross processes
    without allocating extra resources. Furthermore,
    changes in one process are visible by all other processes.
    """
    ...

def readNGF(file_name: str) -> PGF:
    """
    Opens a grammar in a native format. If several processes,
    open the same grammar, it is stored in shared memory.
    If one and the same process opens a grammar twice,
    it will create two difference views of the same shared
    memory. This means that you don't waste additional physical
    memory but you will allocate twice as much of your virtual
    memory space.
    """
    ...


class Concr:
    @property
    def name(self) -> str:
        """The name of the concrete syntax"""
        ...

    def linearize(e: Expr) -> str:
        """Linearizes the abstract expression and returns as string"""
        ...

    def bracketedLinearize(e: Expr) -> list[Any]:
        """
        Produces a bracketed linearization where syntactic phrases
        are represented as objects of type Bracket and terminal tokens
        are represented as string. When two tokens have to be glued
        together, an object of type BIND is inserted.
        """
        ...

class PGFError:
    """
    This is the exception that several functions throw
    when something unexpected happens.
    """
    ...

class Transaction:
    def setAbstractFlag(self, flag_name: str, value: Any) -> None:
        """
        Updates/sets an abstract flag with the given name.
        The value can only have types int, string, float.
        """
        ...

    def setGlobalFlag(self, flag_name: str, value: Any) -> None:
        """
        Updates/sets a flag global for the whole grammar
        with the given name. The value can only have types int, string, float.
        """
        ...

    def createCategory(self, cat_name: str, hypos: list[tuple[bool,str,Type]], prob: float) -> None:
        """
        Creates a new abstract category with the given name, list of arguments (hypos) and
        a log-probability.
        """
        ...

    def dropCategory(self, cat_name: str) -> None:
        """
        Removes the category with the given name from the abstract syntax as well as from
        all concrete syntaxes. This will also trigger as cascade removal of all functions
        that use that category.
        """
        ...

    def createFunction(self, fun_name: str, type: Type, arity: int, prob: float) -> None:
        """
        Creates a new abstract function with the given name, type, arity and log-probability.
        """
        ...

    def dropFunction(self, fun_name: str) -> None:
        """
        Removes the function with the given name from the abstract syntax as well as from
        all concrete syntaxes.
        """
        ...

    def commit(self) -> None:
        """
        This method is only useful if you don't use the transaction in the with statement
        in Python. In that case call commit when the transaction is complete. If you
        don't call it the transaction will be rolled back when the Transaction object
        is rolled back.
        """
        ...

class Bracket:
    """
    The bracketed linearization represents syntactic phrases
    with objects of this type.
    """

    @property
    def fid(self) -> int:
        """
        Look at an abstract expression, count the functions
        that you see from left to right starting from one. The number
        that you assign to every function is its fid. This makes it
        possible to link the abstract syntax with the syntactic phrase
        that it generates.
        
        For example:

           AdjCN (PositA red_1_A) (UseN apple_1_N)

        linearizes to:

           (CN:1 (AP:2 (A:3 red)) (CN:4 (N:5 apple)))

        and red_1_A is the third function in the abstract syntax.
        """
        ...

    @property
    def fun(self) -> str:
        """
        The name of the function that generates the phrase
        """
        ...

    @property
    def cat(self) -> str:
        """
        The name of the category for this phrase
        """
        ...

    @property
    def ann(self) -> str:
        """
        The name of the field in the category that contains the phrase.
        For discontinuous phrases you can have several brackets with
        the same fid but potentially different fields.
        """
        ...

    @property
    def children(self) -> list[Any]:
        """
        The list of nested phrases. Every child is represented as
        either string, Bracket or BIND.
        """
        ...

class BIND:
    """
    The bracketed linearization produces an object of this type to indicate
    that the two surrounding words must be glued together.
    """
    ...

BIND_TYPE_EXPLICIT : bool = True
BIND_TYPE_IMPLICIT : bool = False

class Expr:
    """
    A common class for all subclasses which represent
    an abstract expression.
    """

    def __init__(self, *args):
        """
        Depending on the arguments, the constructor for Expr will construct
        objects of the different subclasses as follows:

        Expr()      ==> ExprMeta(0);
        Expr(val)   ==> ExprLit(val);
        Expr(f, []) ==> ExprFun(f);
        Expr(f, [a,b,...]) ==> ExprApp(... ExprApp(ExprApp(ExprFun(f), a), b), ...)
        """
        ...

    def unpack(self) -> Any:
        """
        Depending on the expression, this method returns values of different types:

        - for a function application, e.g. (f x1 x2 ...), it returns the tuple ("f", [x1,x2,...])
        - for a literal. e.g. "John", 42, 3.14, it returns the value of the literal itself.
        - for a lambda abstraction/application, e.g. (\x -> e) x1 x2 ..., it returns (True,"x",e,[x1,x2,...])
          Here True means that the argument is explicit, you get False for (\{x} -> e).
        """
        ...

    def visit(self, visitor: Any) -> None:
        """
        This is an implementation of the visitor pattern.
        The visit method will look at the name of the top-level function
        in the expression and will call a method with name on_f, if
        the function is f. It will also pass as arguments the subexpressions
        to which f is applied. If the method on_f is not available, the
        method default(e) will be called with the entire expression.
        If default doesn't exist either, nothing will be executed.
        """
        ...

class ExprAbs(Expr):
    """A class to represent a lambda abstraction"""

    def __init__(self, bind_type: bool, name: str, body: Expr):
        """
        Creates an expression representing a lambda abstraction with
        the given variable name and body. When the first argument
        is False the result will be a function with an implicit argument.
        """
        ...

    @property
    def bind_type(self) -> bool:
        """True if the bound variable is an implicit argument."""
        ...

    @property
    def name(self) -> str:
        """The name of the variable."""
        ...

    @property
    def body(self) -> Expr:
        """An expression for the function body"""
        ...

class ExprApp(Expr):
    """A class to represent a function application"""

    def __init__(self, *args):
        """
        A function application can be constructed in one of the following ways:

          ExprApp(fun: Expr, arg: Expr)
          ExprApp(fun: Expr, args: list[Expr])
          ExprApp(fun: str, args: list[Expr])
        
        When a list of arguments is provided, several function applications will be build.
        """
        ...

    @property
    def fun(self) -> Expr:
        """The function"""
        ...

    @property
    def arg(self) -> Expr:
        """The argument"""
        ...

class ExprFun(Expr):
    """A class to represent a function used in an expression"""

    def __init__(self, fun_name: str):
        """
        Creates an expression representing a function name.
        """
        ...

    @property
    def name(self) -> str:
        """The name of the function"""
        ...

class ExprImplArg(Expr):
    """
    A class to represent an implicit argument, e.g. {e}.
    It can be used only in the right-hand of a function application.
    """

    def __init__(self, e: Expr):
        """
        Creates an expression representing an implicit argument.
        """
        ...

    @property
    def expr(self) -> Expr:
        """The argument itself."""
        ...

class ExprLit(Expr):
    """
    A class to represent a literal, i.e. a string, an integer or a floating point number.
    """

    def __init__(self, value: Any):
        """Creates an expression representing a literal with the given value."""
        ...

    @property
    def val(self) -> Any:
        """The value of the literal."""
        ...

class ExprMeta(Expr):
    """
    A class to represent a meta variable.
    """

    def __init__(self, id: int):
        """Creates an expression representing a meta variable with the given id."""
        ...

    @property
    def id(self) -> int:
        """The id of the variable ?1 is 1."""
        ...

class ExprTyped(Expr):
    """A class to represent a typed expression, e.g. <e : ty>."""

    def __init__(self, e: Expr, ty: Type):
        """Creates an expression with an explicit type annotation."""
        ...

    @property
    def expr(self) -> Expr:
        """The nested expression"""
        ...

    @property
    def type(self) -> Type:
        """The specified type"""
        ...

class ExprVar(Expr):
    """A class to represent a lambda variable encoded with its de Bruijn index."""

    def __init__(self, index: int):
        """Creates a lambda variable with the specified de Bruijn index."""
        ...

    @property
    def index(self) -> int:
        """the de Bruijn index"""
        ...

def readExpr(s : str) -> Expr:
    """
    The input string must represent an abstract expression which
    is parsed and returned as an expression object. If the input
    cannot be parsed, a PGFError is thrown.
    """
    ...

def showExpr(vars: list[str], e : Expr) -> str:
    """
    Renders an expression as a string. If the expression
    contains unbound variables, their names must be provided
    in the list vars.
    """
    ...

class Type:
    """A class to represent a type."""

    def __init__(self, *args):
        """
        A type can be constructed in one of the following ways:

          Type(cat_name: str)
          Type(hypos: list[Any], cat_name: str)
          Type(cat_name: str, args: list[Expr])
          Type(hypos: list[Any], cat_name: str, args: list[Expr])

        where the elements of hypos must be of type 
        either str, Type or tuple[bool,str,Type].
        """
        ...

    @property
    def hypos(self) -> list[tuple[bool,str,Type]]:
        """The list of hypotheses, i.e. the types of the function arguments."""
        ...

    @property
    def cat(self) -> str:
        """The name of the category."""
        ...

    @property
    def exprs(self) -> list[Expr]:
        """The arguments of an indexed category."""
        ...

def mkHypo(type: Type) -> tuple[bool,str,Type]:
    """Constructs a hypothesis without a bound variable"""
    ...

def mkDepHypo(var: str, type: Type) -> tuple[bool,str,Type]:
    """Constructs a hypothesis with an explicit variable"""
    ...

def mkImplHypo(var: str, type: Type) -> tuple[bool,str,Type]:
    """Constructs a hypothesis with an implicit variable"""
    ...

def readType(s : str) -> Type:
    """
    The input string must represent an abstract type which
    is parsed and returned as a Type object. If the input
    cannot be parsed, a PGFError is thrown.
    """
    ...

def showType(vars: list[str], t : Type) -> str:
    """
    Renders a type as a string. If the type contains unbound variables,
    their names must be provided in the list vars.
    """
    ...

class EmbeddedGrammar:
    """An object of this type represents a grammar embedded as a Python module."""
    ...
