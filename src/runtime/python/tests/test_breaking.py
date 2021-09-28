import os
import pytest
from pgf import *

@pytest.fixture(scope="module")
def PGF():
    return readPGF("../haskell/tests/basic.pgf")


def test_init_Type_tuples():
    hypos = [mkDepHypo("x", Type([], "N", []))]
    exprs = [ExprVar(0)]
    ty = Type(tuple(hypos), "P", tuple(exprs))
    assert str(ty) == "(x : N) -> P x"

def test_init_Type_lists():
    hypos = [mkDepHypo("x", Type([], "N", []))]
    exprs = [ExprVar(0)]
    ty = Type(hypos, "P", exprs)
    assert str(ty) == "(x : N) -> P x"

def test_Type_modify_shallow():
    hypos = [(BIND_TYPE_EXPLICIT, "x", Type([], "N", []))]
    exprs = [ExprVar(0)]
    ty = Type(hypos, "P", exprs)
    hypos.append(None)
    assert str(ty) == "(x : N) -> P x"

def test_Type_modify_deep():
    hypos = [(BIND_TYPE_EXPLICIT, "x", Type([], "N", []))]
    exprs = [ExprVar(0)]
    ty = Type(hypos, "P", exprs)
    with pytest.raises(AttributeError):
        hypos[0][2].exprs.append(None)
        assert str(ty) == "(x : N) -> P x"
