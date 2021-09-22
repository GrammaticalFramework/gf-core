import pytest
from pgf import *
import math

ty = readType("(N -> N) -> P (s z)")
prob = math.pi

@pytest.fixture(scope="module")
def gr1():
    return readPGF("../haskell/tests/basic.pgf")

@pytest.fixture(scope="module")
def gr2(gr1):
    t = gr1.newTransaction()
    t.createFunction("foo", ty, 0, prob),
    t.createCategory("Q", [(BIND_TYPE_EXPLICIT, "x", ty)], prob)
    assert t.commit()
    return gr1

@pytest.fixture(scope="module")
def gr3():
    # TODO how to avoid reloading from file?
    gr1 = readPGF("../haskell/tests/basic.pgf")
    with gr1.newTransaction("bar_branch") as t:
        t.createFunction("bar", ty, 0, prob),
        t.createCategory("R", [(BIND_TYPE_EXPLICIT, "x", ty)], prob)
    return gr1

# gr1

def test_original_functions(gr1):
    assert gr1.functions == ["c", "ind", "s", "z"]

def test_original_categories(gr1):
    assert gr1.categories == ["Float","Int","N","P","S","String"]

def test_original_function_type(gr1):
    with pytest.raises(KeyError):
        gr1.functionType("foo")

def test_original_function_prob(gr1):
    # with pytest.raises(KeyError):
    #     gr1.functionProbability("foo")
    assert gr1.functionProbability("foo") == float('inf')

# gr2

def test_extended_functions(gr2):
    assert gr2.functions == ["c", "foo", "ind", "s", "z"]

def test_extended_categories(gr2):
    assert gr2.categories == ["Float","Int","N","P","Q","S","String"]

def test_extended_category_context(gr2):
    assert gr2.categoryContext("Q") == [(BIND_TYPE_EXPLICIT, "x", ty)]

def test_extended_function_type(gr2):
    assert gr2.functionType("foo") == ty

def test_extended_function_prob(gr2):
    # TODO: can't we get higher precision?
    # assert gr2.functionProbability("foo") == prob
    assert math.isclose(gr2.functionProbability("foo"), prob, rel_tol=1e-06)

# gr3

def test_branched_functions(gr3):
    assert gr3.functions == ["bar", "c", "ind", "s", "z"]

def test_branched_function_type(gr3):
    assert gr3.functionType("bar") == ty
