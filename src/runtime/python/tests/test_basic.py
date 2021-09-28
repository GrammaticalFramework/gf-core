import os
import pytest
from pgf import *

# readPGF

@pytest.fixture(scope="module")
def PGF():
    return readPGF("../haskell/tests/basic.pgf")

def test_readPGF_non_existant():
    with pytest.raises(FileNotFoundError):
        readPGF("../haskell/tests/abc.pgf")

def test_readPGF_GF():
    with pytest.raises(PGFError):
        readPGF("../haskell/tests/basic.gf")

def test_readPGF_NGF(NGF):
    with pytest.raises(PGFError):
        readPGF("./basic.ngf")

# bootNGF

@pytest.fixture(scope="module")
def NGF():
    ngf = bootNGF("../haskell/tests/basic.pgf", "./basic.ngf")
    yield ngf
    os.remove("./basic.ngf")

def test_bootNGF_non_existant():
    with pytest.raises(FileNotFoundError):
        bootNGF("../haskell/tests/abc.pgf", "./abc.ngf")

def test_bootNGF_GF():
    with pytest.raises(PGFError):
        bootNGF("../haskell/tests/basic.gf", "./abc.ngf")

def test_bootNGF_NGF(NGF):
    with pytest.raises(PGFError):
        bootNGF("./basic.ngf", "./abc.ngf")

def test_bootNGF_existing(NGF):
    with pytest.raises(FileExistsError):
        bootNGF("../haskell/tests/basic.pgf", "./basic.ngf")

# readNGF

def test_readNGF_non_existant():
    with pytest.raises(FileNotFoundError):
        readNGF("./abc.ngf")

def test_readNGF_GF():
    with pytest.raises(PGFError):
        readNGF("../haskell/tests/basic.gf")

def test_readNGF_PGF():
    with pytest.raises(PGFError):
        readNGF("../haskell/tests/basic.pgf")

def test_readNGF(NGF):
    PGF = readNGF("./basic.ngf")
    assert len(PGF.categories) > 0

# newNGF

def test_newNGF_file(NGF):
    PGF = newNGF("empty", "./empty.ngf")
    assert len(PGF.categories) == 0
    os.remove("./empty.ngf") # cleanup

def test_newNGF_memory(NGF):
    PGF = newNGF("empty")
    assert len(PGF.categories) == 0

def test_newNGF_existing(NGF):
    with pytest.raises(FileExistsError):
        newNGF("empty", "./basic.ngf")

# writePGF

def test_writePGF(PGF):
    PGF.writeToFile("./copy.pgf")
    os.remove("./copy.pgf") # cleanup

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
    hypo = cxt[0]
    # assert isinstance(hypo, Hypo)
    # assert hypo.bind_type == BIND_TYPE_EXPLICIT
    # assert hypo.cid == "_"
    # assert hypo.type == readType("N")
    assert hypo[0] == BIND_TYPE_EXPLICIT
    assert hypo[1] == "_"
    assert hypo[2] == readType("N")

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
    with pytest.raises(PGFError):
        readType("->")

def test_readType_equality_1():
    assert readType("A") == readType("A")

def test_readType_equality_2():
    assert readType("A -> B") == readType("A->B")

def test_readType_equality_3():
    assert readType("A -> B -> C") == readType("A->B   ->   C")

def test_readType_inequality_1():
    assert readType("A") != readType("B")

def test_readType_inequality_2():
    assert readType("A -> B") != readType("B->B")

def test_readType_str():
    assert str(readType("A->   BÄ->C")) == "A -> BÄ -> C"

def test_functionType_1(PGF):
    assert PGF.functionType("z") == readType("N")

def test_functionType_2(PGF):
    assert PGF.functionType("s") == readType("N->N")

def test_functionType_3(PGF):
    assert PGF.functionType("c") == readType("N -> S")

def test_functionType_non_existant(PGF):
    with pytest.raises(KeyError):
        assert PGF.functionType("cbx")

def test_functionType_wrong(PGF):
    assert PGF.functionType("c") != readType("N -> S -> X")

def test_startCat(PGF):
    assert PGF.startCat == readType("S")

def test_showType_1(PGF):
    type = Type([], "N", [])
    assert showType([], type) == "N"

def test_showType_2(PGF):
    type = Type([mkHypo(Type([], "N", []))], "N", [])
    assert showType([], type) == "N -> N"

def test_showType_3(PGF):
    type = Type([mkHypo(Type([mkHypo(Type([], "N", []))], "N", []))], "N", [])
    assert showType([], type) == "(N -> N) -> N"

def test_showType_4(PGF):
    type = Type([mkDepHypo("x", Type([], "N", []))], "P", [ExprVar(0)])
    assert showType([], type) == "(x : N) -> P x"

def test_showType_5(PGF):
    type = Type([mkDepHypo("f", Type([mkHypo(Type([], "N", []))], "N", []))], "P", [ExprApp(ExprVar(0), ExprFun("z"))])
    assert showType([], type) == "(f : N -> N) -> P (f z)"

def test_showType_6(PGF):
    type = Type([mkDepHypo("f", Type([mkHypo(Type([], "N", []))], "N", []))], "P", [ExprApp(ExprVar(0), ExprVar(1))])
    assert showType(["n"], type) == "(f : N -> N) -> P (f n)"

def test_showType_7(PGF):
    type = Type([mkImplHypo("f", Type([mkHypo(Type([], "N", []))], "N", []))], "P", [ExprApp(ExprVar(0), ExprVar(1))])
    assert showType(["n"], type) == "({f} : N -> N) -> P (f n)"

def test_showType_8(PGF):
    type = Type([mkDepHypo("x", Type([], "N", [])), mkHypo(Type([], "P", [ExprVar(0)]))], "S", [])
    assert showType(["n"], type) == "(x : N) -> P x -> S"

def test_showType_9(PGF):
    type = Type([mkDepHypo("x", Type([], "N", [])), mkDepHypo("y", Type([], "P", [ExprVar(0)]))], "S", [])
    assert showType(["n"], type) == "(x : N) -> (y : P x) -> S"

def test_Type_getters():
    h0 = mkDepHypo("x", Type([], "N", []))
    e0 = ExprVar(0)
    type = Type([h0], "N", [e0])
    assert type.hypos == [h0]
    assert type.cat == "N"
    assert type.exprs == [e0]
    with pytest.raises(AttributeError):
        type.fake

# expressions

def test_readExpr_invalid():
    with pytest.raises(PGFError):
        readExpr("->")

# expressions: literals

def test_readExpr_lint_equality():
    assert readExpr("123") == ExprLit(123)

def test_readExpr_lint_equality_neg():
    assert readExpr("-123") == ExprLit(-123)

def test_readExpr_lint_equality_big2():
    assert readExpr("774763251095801167872") == ExprLit(774763251095801167872)

def test_readExpr_lint_equality_big2_neg():
    assert readExpr("-774763251095801167872") == ExprLit(-774763251095801167872)

def test_readExpr_lint_inequality():
    assert readExpr("123") != ExprLit(456)

def test_readExpr_lflt_equality():
    assert readExpr("3.142") == ExprLit(3.142)

def test_readExpr_lflt_inequality():
    assert readExpr("3.142") != ExprLit(3)

def test_readExpr_lstr_equality():
    assert readExpr("\"abc\"") == ExprLit("abc")

def test_readExpr_lstr_inequality():
    assert readExpr("\"abc\"") != ExprLit("def")

def test_readExpr_lint_str():
    assert str(readExpr("123")) == "123"

def test_readExpr_lint_str_neg():
    assert str(readExpr("-123")) == "-123"

def test_readExpr_lint_str_big2():
    assert str(readExpr("774763251095801167872")) == "774763251095801167872"

def test_readExpr_lint_str_big3():
    assert str(readExpr("7747632510958011678729003251095801167999")) == "7747632510958011678729003251095801167999"

def test_readExpr_lint_str_big2_neg():
    assert str(readExpr("-774763251095801167872")) == "-774763251095801167872"

def test_readExpr_lint_str_big3_neg():
    assert str(readExpr("-7747632510958011678729003251095801167999")) == "-7747632510958011678729003251095801167999"

def test_readExpr_lflt_str():
    assert str(readExpr("3.142")) == "3.142"

def test_readExpr_lstr_str_unicode():
    assert str(readExpr("\"açġħ\"")) == "\"açġħ\""

def test_readExpr_lstr_null():
    assert str(ExprLit("ab\0c")) == "\"ab\\0c\""

def test_readExpr_lstr_newline():
    assert str(ExprLit("ab\nc")) == "\"ab\\nc\""

def test_ExprLit_getters():
    assert ExprLit(123).val == 123
    assert ExprLit("123").val == "123"
    assert ExprLit(1.23).val == 1.23
    with pytest.raises(AttributeError):
        ExprLit(1.23).fake

# expressions: functions

def test_readExpr_efun_equality_1():
    assert readExpr("f") == ExprFun("f")

def test_readExpr_efun_equality_2():
    assert \
        readExpr("f x y") == \
        ExprApp(
            ExprApp(
                ExprFun("f"),
                ExprFun("x")
            ),
            ExprFun("y")
        )

def test_readExpr_efun_equality_3():
    assert \
        readExpr("f (g x)") == \
        ExprApp(
            ExprFun("f"),
            ExprApp(
                ExprFun("g"),
                ExprFun("x")
            )
        )

def test_readExpr_efun_equality_4():
    assert \
        readExpr("f {g x}") == \
        ExprApp(
            ExprFun("f"),
            ExprImplArg(
                ExprApp(
                    ExprFun("g"),
                    ExprFun("x")
                )
            )
        )

def test_readExpr_efun_str_unicode_1():
    assert str(readExpr("'абв'")) == "'абв'"

@pytest.mark.skip(reason="failing")
def test_readExpr_efun_str_unicode_2():
    assert str(readExpr("'аb'")) == "аb"

@pytest.mark.skip(reason="failing")
def test_readExpr_efun_str_unicode_3():
    assert str(readExpr("'а\\'b'")) == "а'b"

def test_readExpr_efun_str_unicode_4():
    assert str(readExpr("'а\\'б'")) == "'а\\'б'"

def test_ExprApp_getters():
    e1 = ExprFun("f")
    e2 = ExprFun("x")
    expr = ExprApp(e1, e2)
    assert expr.fun == e1
    assert expr.arg == e2
    with pytest.raises(AttributeError):
        expr.fake

def test_ExprFun_getters():
    expr = ExprFun("f")
    assert expr.name == "f"
    with pytest.raises(AttributeError):
        expr.fake

# expressions: variables

# def test_readExpr_evar_equality_1():
#     assert readExpr("#0") == ExprVar()
#     assert readExpr("#0") == ExprVar(0)

# def test_readExpr_evar_equality_2():
#     assert readExpr("#42") == ExprVar(42)

def test_readExpr_evar_str_1():
    assert str(ExprVar(0)) == "#0"

def test_readExpr_evar_str_2():
    assert str(ExprVar(42)) == "#42"

def test_showExpr_evar_1():
    assert showExpr(["x"], ExprVar(0)) == "x"

def test_showExpr_evar_2():
    assert showExpr(["x"], ExprVar(1)) == "#1"

def test_showExpr_evar_3():
    assert showExpr(["z", "y", "x"], ExprVar(0)) == "z"

def test_showExpr_evar_4():
    assert showExpr(["z", "y", "x"], ExprVar(1)) == "y"

def test_ExprVar_getters():
    expr = ExprVar(456)
    assert expr.index == 456
    with pytest.raises(AttributeError):
        expr.fake

# expressions: lambda abstractions

def test_showExpr_eabs_1():
    expr = ExprAbs(BIND_TYPE_EXPLICIT, "w", ExprVar(0))
    assert showExpr(["z", "y", "x"], expr) == "\\w->w"

def test_showExpr_eabs_2():
    expr = ExprAbs(BIND_TYPE_EXPLICIT, "v", ExprAbs(BIND_TYPE_EXPLICIT, "w", ExprVar(2)))
    assert showExpr(["z", "y", "x"], expr) == "\\v,w->z"

def test_showExpr_eabs_3():
    expr = ExprAbs(BIND_TYPE_EXPLICIT, "v", ExprAbs(BIND_TYPE_IMPLICIT, "w", ExprVar(2)))
    assert showExpr(["z", "y", "x"], expr) == "\\v,{w}->z"

def test_showExpr_eabs_4():
    expr = ExprAbs(BIND_TYPE_EXPLICIT, "v", ExprAbs(BIND_TYPE_IMPLICIT, "w", ExprAbs(BIND_TYPE_EXPLICIT, "z", ExprVar(0))))
    assert showExpr(["y", "x"], expr) == "\\v,{w},z->z"

def test_showExpr_eabs_5():
    expr = ExprAbs(BIND_TYPE_EXPLICIT, "v", ExprAbs(BIND_TYPE_IMPLICIT, "w", ExprAbs(BIND_TYPE_IMPLICIT, "z", ExprVar(2))))
    assert showExpr(["y", "x"], expr) == "\\v,{w,z}->v"

def test_showExpr_eabs_6():
    expr = ExprAbs(BIND_TYPE_EXPLICIT, "v", ExprAbs(BIND_TYPE_IMPLICIT, "w", ExprAbs(BIND_TYPE_IMPLICIT, "z", ExprAbs(BIND_TYPE_EXPLICIT, "t", ExprVar(3)))))
    assert showExpr(["y", "x"], expr) == "\\v,{w,z},t->v"

def test_showExpr_eabs_7():
    expr = ExprAbs(BIND_TYPE_EXPLICIT, "u", ExprAbs(BIND_TYPE_EXPLICIT, "v", ExprAbs(BIND_TYPE_IMPLICIT, "w", ExprAbs(BIND_TYPE_IMPLICIT, "z", ExprAbs(BIND_TYPE_EXPLICIT, "t", ExprVar(3))))))
    assert showExpr(["y", "x"], expr) == "\\u,v,{w,z},t->v"

def test_showExpr_eabs_8():
    expr = ExprApp(ExprFun("f"), ExprAbs(BIND_TYPE_EXPLICIT, "x", ExprVar(0)))
    assert showExpr([], expr) == "f (\\x->x)"

def test_showExpr_eabs_freshvars_1():
    expr = ExprAbs(BIND_TYPE_EXPLICIT, "v", ExprAbs(BIND_TYPE_EXPLICIT, "v", ExprVar(0)))
    assert showExpr([], expr) == "\\v,v1->v1"

def test_showExpr_eabs_freshvars_2():
    expr = ExprAbs(BIND_TYPE_EXPLICIT, "v", ExprAbs(BIND_TYPE_EXPLICIT, "v", ExprVar(1)))
    assert showExpr([], expr) == "\\v,v1->v"

def test_showExpr_eabs_freshvars_3():
    expr = ExprAbs(BIND_TYPE_EXPLICIT, "v", ExprAbs(BIND_TYPE_EXPLICIT, "v", ExprAbs(BIND_TYPE_EXPLICIT, "v", ExprVar(1))))
    assert showExpr([], expr) == "\\v,v1,v2->v1"

def test_ExprAbs_getters():
    e0 = ExprAbs(BIND_TYPE_EXPLICIT, "v", ExprVar(1))
    expr = ExprAbs(BIND_TYPE_EXPLICIT, "v", e0)
    assert expr.bind_type == BIND_TYPE_EXPLICIT
    assert expr.name == "v"
    assert expr.body == e0
    with pytest.raises(AttributeError):
        expr.fake

# expressions: meta variables

def test_readExpr_emeta_1():
    assert readExpr("?") == ExprMeta()
    assert readExpr("?") == ExprMeta(0)

def test_readExpr_emeta_2():
    assert readExpr("?42") == ExprMeta(42)

def test_readExpr_emeta_str_1():
    assert str(readExpr("?")) == "?"

def test_readExpr_emeta_str_2():
    assert str(readExpr("?42")) == "?42"

def test_ExprMeta_getters():
    expr = ExprMeta(123)
    assert expr.id == 123
    with pytest.raises(AttributeError):
        expr.fake

# expressions: typed expressions

def test_readExpr_emeta_equality():
    assert readExpr("<z : N>") == ExprTyped(ExprFun("z"), readType("N"))

def test_readExpr_emeta_str():
    assert str(readExpr("<z : N>")) == "<z : N>"

def test_ExprTyped_getters():
    e = ExprFun("z")
    ty = readType("N")
    expr = ExprTyped(e, ty)
    assert expr.expr == e
    assert expr.type == ty
    with pytest.raises(AttributeError):
        expr.fake

def test_ExprImplArg_getters():
    e = ExprFun("z")
    expr = ExprImplArg(e)
    assert expr.expr == e
    with pytest.raises(AttributeError):
        expr.fake
