import pytest
from pgf import *
import math

ty = readType("(N -> N) -> P (s z)")
prob = math.pi

@pytest.fixture(scope="function")
def gr1():
    gr = readPGF("../haskell/tests/basic.pgf")
    yield gr

@pytest.fixture(scope="function")
def gr2(gr1):
    gr = gr1
    t = gr.newTransaction()
    t.createFunction("foo", ty, 0, prob)
    t.createCategory("Q", [(BIND_TYPE_EXPLICIT, "x", ty)], prob)
    t.commit()
    yield gr

@pytest.fixture(scope="function")
def gr3(gr1):
    gr = gr1
    with gr.newTransaction("bar_branch") as t:
        t.createFunction("bar", ty, 0, prob)
        t.createCategory("R", [(BIND_TYPE_EXPLICIT, "x", ty)], prob)
    yield gr

@pytest.fixture(scope="function")
def gr4(gr2):
    gr = gr2
    gr.checkoutBranch("master")
    yield gr

@pytest.fixture(scope="function")
def gr5(gr3):
    gr = gr3
    gr.checkoutBranch("bar_branch")
    yield gr

@pytest.fixture(scope="function")
def gr6(gr1):
    gr = gr1
    with gr.newTransaction() as t:
        t.dropFunction("ind")
        t.dropCategory("S")
    yield gr

# general

def test_checkout_non_existant(gr1):
    with pytest.raises(KeyError):
        gr1.checkoutBranch("abc")

# gr1

def test_original_functions(gr1):
    assert gr1.functions == ["c", "ind", "s", "z"]

def test_original_categories(gr1):
    assert gr1.categories == ["Float","Int","N","P","S","String"]

def test_original_function_type(gr1):
    with pytest.raises(KeyError):
        gr1.functionType("foo")

def test_original_function_prob(gr1):
    with pytest.raises(KeyError):
        gr1.functionProbability("foo")
    # assert gr1.functionProbability("foo") == float('inf')

def test_original_category_prob(gr1):
    with pytest.raises(KeyError):
        gr1.categoryProbability("Q")
    # assert gr1.categoryProbability("Q") == float('inf')

def test_original_expr_prob(gr1):
    # with pytest.raises(KeyError):
    #     gr1.exprProbability(ExprFun("foo"))
    assert gr1.exprProbability(ExprFun("foo")) == float('inf')

# gr2

def test_extended_functions(gr2):
    assert gr2.functions == ["c", "foo", "ind", "s", "z"]

def test_extended_categories(gr2):
    assert gr2.categories == ["Float","Int","N","P","Q","S","String"]

def test_extended_category_context(gr2):
    print(gr2.categoryContext("Q"))
    assert gr2.categoryContext("Q") == ((BIND_TYPE_EXPLICIT, "x", ty),)

def test_extended_function_type(gr2):
    assert gr2.functionType("foo") == ty

def test_extended_function_prob(gr2):
    assert math.isclose(gr2.functionProbability("foo"), prob, rel_tol=1e-06)

def test_extended_category_prob(gr2):
    assert math.isclose(gr2.categoryProbability("Q"), prob, rel_tol=1e-06)

def test_extended_expr_prob(gr2):
    assert math.isclose(gr2.exprProbability(ExprFun("foo")), prob, rel_tol=1e-06)

# gr3

def test_branched_functions(gr3):
    assert gr3.functions == ["bar", "c", "ind", "s", "z"]

def test_branched_categories(gr3):
    assert gr3.categories == ["Float","Int","N","P","R","S","String"]

def test_branched_category_context(gr3):
    assert gr3.categoryContext("R") == ((BIND_TYPE_EXPLICIT, "x", ty),)

def test_branched_function_type(gr3):
    assert gr3.functionType("bar") == ty

# gr4, 5

def test_branched_functions(gr4):
    assert gr4.functions == ["c", "foo", "ind", "s", "z"]

def test_branched_functions(gr5):
    assert gr5.functions == ["bar", "c", "ind", "s", "z"]

# gr6

def test_reduced_functions(gr6):
    assert gr6.functions == ["c", "s", "z"]

def test_reduced_categories(gr6):
    assert gr6.categories == ["Float","Int","N","P","String"]

# flags

def test_global_flag_int(gr1):
    val = 12345
    with pytest.raises(KeyError):
        gr1.getGlobalFlag("intflag")
    with gr1.newTransaction() as t:
        t.setGlobalFlag("intflag", val)
    assert gr1.getGlobalFlag("intflag") == val

def test_global_flag_float(gr1):
    val = math.e
    with pytest.raises(KeyError):
        gr1.getGlobalFlag("floatflag")
    with gr1.newTransaction() as t:
        t.setGlobalFlag("floatflag", val)
    assert gr1.getGlobalFlag("floatflag") == val

def test_global_flag_string(gr1):
    val = "S"
    with pytest.raises(KeyError):
        gr1.getGlobalFlag("stringflag")
    with gr1.newTransaction() as t:
        t.setGlobalFlag("stringflag", val)
    assert gr1.getGlobalFlag("stringflag") == val

def test_abstract_flag_int(gr1):
    val = -774763251095801167872
    with pytest.raises(KeyError):
        gr1.getAbstractFlag("intflag")
    with gr1.newTransaction() as t:
        t.setAbstractFlag("intflag", val)
    assert gr1.getAbstractFlag("intflag") == val

def test_abstract_flag_float(gr1):
    val = 0.1e-8
    with pytest.raises(KeyError):
        gr1.getAbstractFlag("floatflag")
    with gr1.newTransaction() as t:
        t.setAbstractFlag("floatflag", val)
    assert gr1.getAbstractFlag("floatflag") == val

def test_abstract_flag_string(gr1):
    val = "子曰：「學而時習之，不亦說乎？有朋自遠方來，不亦樂乎？"
    with pytest.raises(KeyError):
        gr1.getAbstractFlag("stringflag")
    with gr1.newTransaction() as t:
        t.setAbstractFlag("stringflag", val)
    assert gr1.getAbstractFlag("stringflag") == val
