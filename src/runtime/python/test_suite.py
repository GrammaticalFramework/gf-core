import os
import pytest
import pgf

# readPGF

@pytest.fixture(scope="module")
def PGF():
    return pgf.readPGF("../haskell/tests/basic.pgf")

def test_readPGF_non_existant():
    with pytest.raises(FileNotFoundError):
        pgf.readPGF("../haskell/tests/abc.pgf")

def test_readPGF_GF():
    with pytest.raises(pgf.PGFError):
        pgf.readPGF("../haskell/tests/basic.gf")

def test_readPGF_NGF(NGF):
    with pytest.raises(pgf.PGFError):
        pgf.readPGF("./basic.ngf")

# bootNGF

@pytest.fixture(scope="module")
def NGF():
    ngf = pgf.bootNGF("../haskell/tests/basic.pgf", "./basic.ngf")
    yield ngf
    os.remove("./basic.ngf")

def test_bootNGF_non_existant():
    with pytest.raises(FileNotFoundError):
        pgf.bootNGF("../haskell/tests/abc.pgf", "./abc.ngf")

def test_bootNGF_GF():
    with pytest.raises(pgf.PGFError):
        pgf.bootNGF("../haskell/tests/basic.gf", "./abc.ngf")

def test_bootNGF_NGF(NGF):
    with pytest.raises(pgf.PGFError):
        pgf.bootNGF("./basic.ngf", "./abc.ngf")

def test_bootNGF_existing(NGF):
    with pytest.raises(FileExistsError):
        pgf.bootNGF("../haskell/tests/basic.pgf", "./basic.ngf")

# readNGF

def test_readNGF_non_existant():
    PGF = pgf.readNGF("./abc.ngf") # create empty grammar
    assert PGF.categories == []
    os.remove("./abc.ngf") # cleanup

def test_readNGF_GF():
    with pytest.raises(pgf.PGFError):
        pgf.readNGF("../haskell/tests/basic.gf")

def test_readNGF_PGF():
    with pytest.raises(pgf.PGFError):
        pgf.readNGF("../haskell/tests/basic.pgf")

def test_readNGF(NGF):
    PGF = pgf.readNGF("./basic.ngf")
    assert len(PGF.categories) > 0

# abstract syntax

def test_abstractName(PGF):
    assert PGF.abstractName == "basic"

def test_categories(PGF):
    assert PGF.categories == ["Float","Int","N","P","S","String"]

def test_functions(PGF):
    assert PGF.functions == ["c","ind","s","z"]

def test_functionsByCat_1(PGF):
    assert PGF.functionsByCat("N") == ["s","z"]

def test_functionsByCat_2(PGF):
    assert PGF.functionsByCat("S") == ["c"]

def test_functionsByCat_non_existant(PGF):
    assert PGF.functionsByCat("X") == []

def test_categoryContext_1(PGF):
    assert PGF.categoryContext("N") == []

def test_categoryContext_2(PGF):
    assert PGF.categoryContext("S") == []

def test_categoryContext_3(PGF):
    cxt = PGF.categoryContext("P")
    assert len(cxt) == 1
    tup = cxt[0]
    assert tup[0] == 0 # explicit
    assert tup[1] == "_" # cid
    assert tup[2] == pgf.readType("N")

def test_categoryContext_4(PGF):
    assert PGF.categoryContext("X") == []

def test_functionIsConstructor_1(PGF):
    assert PGF.functionIsConstructor("s") == True

def test_functionIsConstructor_2(PGF):
    assert PGF.functionIsConstructor("z") == True

def test_functionIsConstructor_3(PGF):
    assert PGF.functionIsConstructor("c") == True

def test_functionIsConstructor_4(PGF):
    assert PGF.functionIsConstructor("ind") == False

# types

def test_readType_invalid():
    with pytest.raises(pgf.PGFError):
        pgf.readType("->")

def test_readType_equality_1():
    assert pgf.readType("A") == pgf.readType("A")

def test_readType_equality_2():
    assert pgf.readType("A -> B") == pgf.readType("A->B")

def test_readType_equality_3():
    assert pgf.readType("A -> B -> C") == pgf.readType("A->B   ->   C")

def test_readType_inequality_1():
    assert pgf.readType("A") != pgf.readType("B")

def test_readType_inequality_2():
    assert pgf.readType("A -> B") != pgf.readType("B->B")

def test_readType_str():
    assert str(pgf.readType("A->   BÄ->C")) == "A -> BÄ -> C"

def test_functionType_1(PGF):
    assert PGF.functionType("z") == pgf.readType("N")

def test_functionType_2(PGF):
    assert PGF.functionType("s") == pgf.readType("N->N")

def test_functionType_3(PGF):
    assert PGF.functionType("c") == pgf.readType("N -> S")

def test_functionType_non_existant(PGF):
    with pytest.raises(KeyError):
        assert PGF.functionType("cbx")

def test_functionType_wrong(PGF):
    assert PGF.functionType("c") != pgf.readType("N -> S -> X")

def test_startCat(PGF):
    assert PGF.startCat == pgf.readType("S")

# expressions

def test_readExpr_invalid():
    with pytest.raises(pgf.PGFError):
        pgf.readExpr("->")

def test_readExpr_equality_int():
    assert pgf.readExpr("123") == pgf.readExpr("123")

def test_readExpr_equality_int_neg():
    assert pgf.readExpr("-123") == pgf.readExpr("-123")

def test_readExpr_equality_int_big():
    assert pgf.readExpr("774763251095801167872") == pgf.readExpr("774763251095801167872")

def test_readExpr_equality_int_big_neg():
    assert pgf.readExpr("-774763251095801167872") == pgf.readExpr("-774763251095801167872")

def test_readExpr_inequality_int():
    assert pgf.readExpr("123") != pgf.readExpr("456")

def test_readExpr_equality_float():
    assert pgf.readExpr("3.142") == pgf.readExpr("3.142")

def test_readExpr_inequality_float():
    assert pgf.readExpr("3.142") != pgf.readExpr("3")

def test_readExpr_equality_string():
    assert pgf.readExpr("\"abc\"") == pgf.readExpr("\"abc\"")

def test_readExpr_inequality_string():
    assert pgf.readExpr("\"abc\"") != pgf.readExpr("\"def\"")

def test_readExpr_str_int():
    assert str(pgf.readExpr("123")) == "123"

def test_readExpr_str_int_neg():
    assert str(pgf.readExpr("-123")) == "-123"

def test_readExpr_str_int_big_2():
    assert str(pgf.readExpr("774763251095801167872")) == "774763251095801167872"

def test_readExpr_str_int_big_3():
    assert str(pgf.readExpr("7747632510958011678729003251095801167999")) == "7747632510958011678729003251095801167999"

def test_readExpr_str_int_big_2_neg():
    assert str(pgf.readExpr("-774763251095801167872")) == "-774763251095801167872"

def test_readExpr_str_int_big_3_neg():
    assert str(pgf.readExpr("-7747632510958011678729003251095801167999")) == "-7747632510958011678729003251095801167999"

def test_readExpr_str_float():
    assert str(pgf.readExpr("3.142")) == "3.142"

def test_readExpr_str_string():
    assert str(pgf.readExpr("\"açġħ\"")) == "\"açġħ\""
