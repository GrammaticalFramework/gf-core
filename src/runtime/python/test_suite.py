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

@pytest.mark.skip(reason="Unhandled case in runtime")
def test_readPGF_NGF(NGF):
    with pytest.raises(pgf.PGFError):
        pgf.readPGF("./basic.ngf")

# bootNGF

@pytest.fixture(scope="module")
def NGF():
    return pgf.bootNGF("../haskell/tests/basic.pgf", "./basic.ngf")

def test_bootNGF_non_existant():
    with pytest.raises(FileNotFoundError):
        pgf.bootNGF("../haskell/tests/abc.pgf", "./abc.ngf")

def test_bootNGF_GF():
    with pytest.raises(pgf.PGFError):
        pgf.bootNGF("../haskell/tests/basic.gf", "./abc.ngf")

@pytest.mark.skip(reason="Unhandled case in runtime")
def test_bootNGF_NGF(NGF):
    with pytest.raises(pgf.PGFError):
        pgf.bootNGF("./basic.ngf", "./abc.ngf")

def test_bootNGF_existing(NGF):
    with pytest.raises(FileExistsError):
        pgf.bootNGF("../haskell/tests/basicp.gf", "./basic.ngf")

# readNGF

@pytest.mark.skip(reason="Unhandled case in runtime")
def test_readNGF_non_existant():
    with pytest.raises(FileNotFoundError):
        pgf.readNGF("./abc.ngf")

@pytest.mark.skip(reason="Unhandled case in runtime")
def test_readNGF_GF():
    with pytest.raises(pgf.PGFError):
        pgf.readNGF("../haskell/tests/basic.gf")

@pytest.mark.skip(reason="Unhandled case in runtime")
def test_readNGF_PGF():
    with pytest.raises(pgf.PGFError):
        pgf.readNGF("../haskell/tests/basic.pgf")

def test_readNGF(NGF):
    pgf.readNGF("./basic.ngf")
    # TODO assert read actually worked

# abstract expressions

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

def test_readType_invalid():
    with pytest.raises(pgf.PGFError):
        pgf.readType("->")

def test_readType_equality_1():
    assert pgf.readType("A") == pgf.readType("A")

def test_readType_equality_2():
    assert pgf.readType("A -> B") == pgf.readType("A->B")

def test_readType_inequality_1():
    assert pgf.readType("A") != pgf.readType("B")

def test_readType_inequality_2():
    assert pgf.readType("A -> B") != pgf.readType("B->B")

def test_functionType_1(PGF):
    assert PGF.functionType("z") == pgf.readType("N")

def test_functionType_2(PGF):
    assert PGF.functionType("s") == pgf.readType("N->N")

def test_functionType_3(PGF):
    assert PGF.functionType("c") == pgf.readType("N -> S")

def test_startCat(PGF):
    with pytest.raises(pgf.PGFError):
        PGF.startCat()
