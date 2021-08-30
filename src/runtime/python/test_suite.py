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

@pytest.mark.skip(reason="Bug in runtime")
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

@pytest.mark.skip(reason="Bug in runtime")
def test_bootNGF_NGF(NGF):
    with pytest.raises(pgf.PGFError):
        pgf.bootNGF("./basic.ngf", "./abc.ngf")

def test_bootNGF_existing(NGF):
    with pytest.raises(FileExistsError):
        pgf.bootNGF("../haskell/tests/basicp.gf", "./basic.ngf")

# readNGF

@pytest.mark.skip(reason="Bug in runtime")
def test_readNGF_non_existant():
    with pytest.raises(FileNotFoundError):
        pgf.readNGF("./abc.ngf")

@pytest.mark.skip(reason="Bug in runtime")
def test_readNGF_GF():
    with pytest.raises(pgf.PGFError):
        pgf.readNGF("../haskell/tests/basic.gf")

@pytest.mark.skip(reason="Bug in runtime")
def test_readNGF_PGF():
    with pytest.raises(pgf.PGFError):
        pgf.readNGF("../haskell/tests/basic.pgf")

def test_readNGF(NGF):
    pgf.readNGF("./basic.ngf")
    # TODO assert read actually worked

# abstract expressions

def test_abstractName(PGF):
    assert PGF.abstractName == "basic"
