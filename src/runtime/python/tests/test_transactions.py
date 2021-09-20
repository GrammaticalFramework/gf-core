import pytest
from pgf import *

ty = readType("(N -> N) -> P (s z)")
pi = 3.142

# @pytest.fixture(scope="module")
# def gr1():
#     return pgf.readPGF("../haskell/tests/basic.pgf")
#
# @pytest.fixture(scope="module")
# def gr2(gr1):
#     transaction = [
#         createFunction("foo", ty, 0, pi),
#         createCategory("Q", [(BIND_TYPE_EXPLICIT, "x", ty)], pi)
#     ]
#     return gr1.modifyPGF(transaction)
#
# @pytest.fixture(scope="module")
# def gr3(gr1):
#     transaction = [
#         createFunction("bar", ty, 0, pi),
#         createCategory("R", [(BIND_TYPE_EXPLICIT, "x", ty)], pi)
#     ]
#     return gr1.branchPGF("bar_branch", transaction)
#
# def original_functions(gr1):
#     assert gr1.functions == ["c", "ind", "s", "z"]
#
# def extended_functions(gr2):
#     assert gr2.functions == ["c", "foo", "ind", "s", "z"]
#
# def branched_functions(gr3):
#     assert gr3.functions == ["bar", "c", "ind", "s", "z"]

def test_non_transactional():
    gr1 = readPGF("../haskell/tests/basic.pgf")
    assert gr1.functions == ["c", "ind", "s", "z"]

    gr1.createFunction("foo", ty, 0, pi)
    assert gr1.functions == ["c", "foo", "ind", "s", "z"]
