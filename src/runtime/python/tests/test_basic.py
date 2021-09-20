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
    with pytest.raises(FileNotFoundError):
        pgf.readNGF("./abc.ngf")

def test_readNGF_GF():
    with pytest.raises(pgf.PGFError):
        pgf.readNGF("../haskell/tests/basic.gf")

def test_readNGF_PGF():
    with pytest.raises(pgf.PGFError):
        pgf.readNGF("../haskell/tests/basic.pgf")

def test_readNGF(NGF):
    PGF = pgf.readNGF("./basic.ngf")
    assert len(PGF.categories) > 0

# newNGF

def test_newNGF_file(NGF):
    PGF = pgf.newNGF("empty", "./empty.ngf")
    assert len(PGF.categories) == 0
    os.remove("./empty.ngf") # cleanup

def test_newNGF_memory(NGF):
    PGF = pgf.newNGF("empty")
    assert len(PGF.categories) == 0

def test_newNGF_existing(NGF):
    with pytest.raises(FileExistsError):
        pgf.newNGF("empty", "./basic.ngf")

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
    assert PGF.categoryContext("X") == None

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

def test_showType_1(PGF):
    type = pgf.Type([], "N", [])
    assert pgf.showType([], type) == "N"

def test_showType_2(PGF):
    type = pgf.Type([pgf.mkHypo(pgf.Type([], "N", []))], "N", [])
    assert pgf.showType([], type) == "N -> N"

def test_showType_3(PGF):
    type = pgf.Type([pgf.mkHypo(pgf.Type([pgf.mkHypo(pgf.Type([], "N", []))], "N", []))], "N", [])
    assert pgf.showType([], type) == "(N -> N) -> N"

def test_showType_4(PGF):
    type = pgf.Type([pgf.mkDepHypo("x", pgf.Type([], "N", []))], "P", [pgf.ExprVar(0)])
    assert pgf.showType([], type) == "(x : N) -> P x"

def test_showType_5(PGF):
    type = pgf.Type([pgf.mkDepHypo("f", pgf.Type([pgf.mkHypo(pgf.Type([], "N", []))], "N", []))], "P", [pgf.ExprApp(pgf.ExprVar(0), pgf.ExprFun("z"))])
    assert pgf.showType([], type) == "(f : N -> N) -> P (f z)"

def test_showType_6(PGF):
    type = pgf.Type([pgf.mkDepHypo("f", pgf.Type([pgf.mkHypo(pgf.Type([], "N", []))], "N", []))], "P", [pgf.ExprApp(pgf.ExprVar(0), pgf.ExprVar(1))])
    assert pgf.showType(["n"], type) == "(f : N -> N) -> P (f n)"

def test_showType_7(PGF):
    type = pgf.Type([pgf.mkImplHypo("f", pgf.Type([pgf.mkHypo(pgf.Type([], "N", []))], "N", []))], "P", [pgf.ExprApp(pgf.ExprVar(0), pgf.ExprVar(1))])
    assert pgf.showType(["n"], type) == "({f} : N -> N) -> P (f n)"

def test_showType_8(PGF):
    type = pgf.Type([pgf.mkDepHypo("x", pgf.Type([], "N", [])), pgf.mkHypo(pgf.Type([], "P", [pgf.ExprVar(0)]))], "S", [])
    assert pgf.showType(["n"], type) == "(x : N) -> P x -> S"

def test_showType_9(PGF):
    type = pgf.Type([pgf.mkDepHypo("x", pgf.Type([], "N", [])), pgf.mkDepHypo("y", pgf.Type([], "P", [pgf.ExprVar(0)]))], "S", [])
    assert pgf.showType(["n"], type) == "(x : N) -> (y : P x) -> S"

# expressions

def test_readExpr_invalid():
    with pytest.raises(pgf.PGFError):
        pgf.readExpr("->")

# expressions: literals

def test_readExpr_lint_equality():
    assert pgf.readExpr("123") == pgf.ExprLit(123)

def test_readExpr_lint_equality_neg():
    assert pgf.readExpr("-123") == pgf.ExprLit(-123)

def test_readExpr_lint_equality_big2():
    assert pgf.readExpr("774763251095801167872") == pgf.ExprLit(774763251095801167872)

def test_readExpr_lint_equality_big2_neg():
    assert pgf.readExpr("-774763251095801167872") == pgf.ExprLit(-774763251095801167872)

def test_readExpr_lint_inequality():
    assert pgf.readExpr("123") != pgf.ExprLit(456)

def test_readExpr_lflt_equality():
    assert pgf.readExpr("3.142") == pgf.ExprLit(3.142)

def test_readExpr_lflt_inequality():
    assert pgf.readExpr("3.142") != pgf.ExprLit(3)

def test_readExpr_lstr_equality():
    assert pgf.readExpr("\"abc\"") == pgf.ExprLit("abc")

def test_readExpr_lstr_inequality():
    assert pgf.readExpr("\"abc\"") != pgf.ExprLit("def")

def test_readExpr_lint_str():
    assert str(pgf.readExpr("123")) == "123"

def test_readExpr_lint_str_neg():
    assert str(pgf.readExpr("-123")) == "-123"

def test_readExpr_lint_str_big2():
    assert str(pgf.readExpr("774763251095801167872")) == "774763251095801167872"

def test_readExpr_lint_str_big3():
    assert str(pgf.readExpr("7747632510958011678729003251095801167999")) == "7747632510958011678729003251095801167999"

def test_readExpr_lint_str_big2_neg():
    assert str(pgf.readExpr("-774763251095801167872")) == "-774763251095801167872"

def test_readExpr_lint_str_big3_neg():
    assert str(pgf.readExpr("-7747632510958011678729003251095801167999")) == "-7747632510958011678729003251095801167999"

def test_readExpr_lflt_str():
    assert str(pgf.readExpr("3.142")) == "3.142"

def test_readExpr_lstr_str_unicode():
    assert str(pgf.readExpr("\"açġħ\"")) == "\"açġħ\""

def test_readExpr_lstr_null():
    assert str(pgf.ExprLit("ab\0c")) == "\"ab\\0c\""

def test_readExpr_lstr_newline():
    assert str(pgf.ExprLit("ab\nc")) == "\"ab\\nc\""

# expressions: functions

def test_readExpr_efun_equality_1():
    assert pgf.readExpr("f") == pgf.ExprFun("f")

def test_readExpr_efun_equality_2():
    assert \
        pgf.readExpr("f x y") == \
        pgf.ExprApp(
            pgf.ExprApp(
                pgf.ExprFun("f"),
                pgf.ExprFun("x")
            ),
            pgf.ExprFun("y")
        )

def test_readExpr_efun_equality_3():
    assert \
        pgf.readExpr("f (g x)") == \
        pgf.ExprApp(
            pgf.ExprFun("f"),
            pgf.ExprApp(
                pgf.ExprFun("g"),
                pgf.ExprFun("x")
            )
        )

def test_readExpr_efun_equality_4():
    assert \
        pgf.readExpr("f {g x}") == \
        pgf.ExprApp(
            pgf.ExprFun("f"),
            pgf.ExprImplArg(
                pgf.ExprApp(
                    pgf.ExprFun("g"),
                    pgf.ExprFun("x")
                )
            )
        )

def test_readExpr_efun_str_unicode_1():
    assert str(pgf.readExpr("'абв'")) == "'абв'"

@pytest.mark.skip(reason="failing")
def test_readExpr_efun_str_unicode_2():
    assert str(pgf.readExpr("'аb'")) == "аb"

@pytest.mark.skip(reason="failing")
def test_readExpr_efun_str_unicode_3():
    assert str(pgf.readExpr("'а\\'b'")) == "а'b"

def test_readExpr_efun_str_unicode_4():
    assert str(pgf.readExpr("'а\\'б'")) == "'а\\'б'"

# expressions: variables

# def test_readExpr_evar_equality_1():
#     assert pgf.readExpr("#0") == pgf.ExprVar()
#     assert pgf.readExpr("#0") == pgf.ExprVar(0)

# def test_readExpr_evar_equality_2():
#     assert pgf.readExpr("#42") == pgf.ExprVar(42)

def test_readExpr_evar_str_1():
    assert str(pgf.ExprVar(0)) == "#0"

def test_readExpr_evar_str_2():
    assert str(pgf.ExprVar(42)) == "#42"

def test_showExpr_evar_1():
    assert pgf.showExpr(["x"], pgf.ExprVar(0)) == "x"

def test_showExpr_evar_2():
    assert pgf.showExpr(["x"], pgf.ExprVar(1)) == "#1"

def test_showExpr_evar_3():
    assert pgf.showExpr(["z", "y", "x"], pgf.ExprVar(0)) == "z"

def test_showExpr_evar_4():
    assert pgf.showExpr(["z", "y", "x"], pgf.ExprVar(1)) == "y"

# expressions: lambda abstractions

def test_showExpr_eabs_1():
    expr = pgf.ExprAbs(pgf.BIND_TYPE_EXPLICIT, "w", pgf.ExprVar(0))
    assert pgf.showExpr(["z", "y", "x"], expr) == "\\w->w"

def test_showExpr_eabs_2():
    expr = pgf.ExprAbs(pgf.BIND_TYPE_EXPLICIT, "v", pgf.ExprAbs(pgf.BIND_TYPE_EXPLICIT, "w", pgf.ExprVar(2)))
    assert pgf.showExpr(["z", "y", "x"], expr) == "\\v,w->z"

def test_showExpr_eabs_3():
    expr = pgf.ExprAbs(pgf.BIND_TYPE_EXPLICIT, "v", pgf.ExprAbs(pgf.BIND_TYPE_IMPLICIT, "w", pgf.ExprVar(2)))
    assert pgf.showExpr(["z", "y", "x"], expr) == "\\v,{w}->z"

def test_showExpr_eabs_4():
    expr = pgf.ExprAbs(pgf.BIND_TYPE_EXPLICIT, "v", pgf.ExprAbs(pgf.BIND_TYPE_IMPLICIT, "w", pgf.ExprAbs(pgf.BIND_TYPE_EXPLICIT, "z", pgf.ExprVar(0))))
    assert pgf.showExpr(["y", "x"], expr) == "\\v,{w},z->z"

def test_showExpr_eabs_5():
    expr = pgf.ExprAbs(pgf.BIND_TYPE_EXPLICIT, "v", pgf.ExprAbs(pgf.BIND_TYPE_IMPLICIT, "w", pgf.ExprAbs(pgf.BIND_TYPE_IMPLICIT, "z", pgf.ExprVar(2))))
    assert pgf.showExpr(["y", "x"], expr) == "\\v,{w,z}->v"

def test_showExpr_eabs_6():
    expr = pgf.ExprAbs(pgf.BIND_TYPE_EXPLICIT, "v", pgf.ExprAbs(pgf.BIND_TYPE_IMPLICIT, "w", pgf.ExprAbs(pgf.BIND_TYPE_IMPLICIT, "z", pgf.ExprAbs(pgf.BIND_TYPE_EXPLICIT, "t", pgf.ExprVar(3)))))
    assert pgf.showExpr(["y", "x"], expr) == "\\v,{w,z},t->v"

def test_showExpr_eabs_7():
    expr = pgf.ExprAbs(pgf.BIND_TYPE_EXPLICIT, "u", pgf.ExprAbs(pgf.BIND_TYPE_EXPLICIT, "v", pgf.ExprAbs(pgf.BIND_TYPE_IMPLICIT, "w", pgf.ExprAbs(pgf.BIND_TYPE_IMPLICIT, "z", pgf.ExprAbs(pgf.BIND_TYPE_EXPLICIT, "t", pgf.ExprVar(3))))))
    assert pgf.showExpr(["y", "x"], expr) == "\\u,v,{w,z},t->v"

def test_showExpr_eabs_8():
    expr = pgf.ExprApp(pgf.ExprFun("f"), pgf.ExprAbs(pgf.BIND_TYPE_EXPLICIT, "x", pgf.ExprVar(0)))
    assert pgf.showExpr([], expr) == "f (\\x->x)"

def test_showExpr_eabs_freshvars_1():
    expr = pgf.ExprAbs(pgf.BIND_TYPE_EXPLICIT, "v", pgf.ExprAbs(pgf.BIND_TYPE_EXPLICIT, "v", pgf.ExprVar(0)))
    assert pgf.showExpr([], expr) == "\\v,v1->v1"

def test_showExpr_eabs_freshvars_2():
    expr = pgf.ExprAbs(pgf.BIND_TYPE_EXPLICIT, "v", pgf.ExprAbs(pgf.BIND_TYPE_EXPLICIT, "v", pgf.ExprVar(1)))
    assert pgf.showExpr([], expr) == "\\v,v1->v"

def test_showExpr_eabs_freshvars_3():
    expr = pgf.ExprAbs(pgf.BIND_TYPE_EXPLICIT, "v", pgf.ExprAbs(pgf.BIND_TYPE_EXPLICIT, "v", pgf.ExprAbs(pgf.BIND_TYPE_EXPLICIT, "v", pgf.ExprVar(1))))
    assert pgf.showExpr([], expr) == "\\v,v1,v2->v1"

# expressions: meta variables

def test_readExpr_emeta_1():
    assert pgf.readExpr("?") == pgf.ExprMeta()
    assert pgf.readExpr("?") == pgf.ExprMeta(0)

def test_readExpr_emeta_2():
    assert pgf.readExpr("?42") == pgf.ExprMeta(42)

def test_readExpr_emeta_str_1():
    assert str(pgf.readExpr("?")) == "?"

def test_readExpr_emeta_str_2():
    assert str(pgf.readExpr("?42")) == "?42"

# expressions: typed expressions

def test_readExpr_emeta_equality():
    assert pgf.readExpr("<z : N>") == pgf.ExprTyped(pgf.ExprFun("z"), pgf.readType("N"))

def test_readExpr_emeta_str():
    assert str(pgf.readExpr("<z : N>")) == "<z : N>"
