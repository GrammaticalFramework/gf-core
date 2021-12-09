import PGF, {
  PGFError,
  PGFGrammar,
  // Type,
  mkType,
  // Hypo,
  mkHypo,
  mkDepHypo,
  mkImplHypo,
  // Expr,
  // ExprAbs,
  ExprApp,
  ExprLit,
  ExprMeta,
  ExprFun,
  ExprVar,
  // ExprTyped,
  ExprImplArg
  // Literal
} from '../index'
import fs from 'fs'

// ----------------------------------------------------------------------------

describe('readPGF', () => {
  test('valid', () => {
    PGF.readPGF('../haskell/tests/basic.pgf')
  })

  test('non-existent', () => {
    expect(() => {
      PGF.readPGF('../haskell/tests/abc.pgf')
    }).toThrow(Error)
  })

  test('GF', () => {
    expect(() => {
      PGF.readPGF('../haskell/tests/basic.gf')
    }).toThrow(PGFError)
  })

  test('NGF', () => {
    try {
      fs.unlinkSync('./basic.ngf')
    } catch {
      // empty
    }
    PGF.bootNGF('../haskell/tests/basic.pgf', './basic.ngf')
    expect(() => {
      PGF.readPGF('./basic.ngf')
    }).toThrow(PGFError)
  })
})

// ----------------------------------------------------------------------------

describe('bootNGF', () => {
  beforeAll(() => {
    try {
      fs.unlinkSync('./basic.ngf')
    } catch {
      // empty
    }
  })

  test('valid', () => {
    PGF.bootNGF('../haskell/tests/basic.pgf', './basic.ngf')
  })

  test('non-existent', () => {
    expect(() => {
      PGF.bootNGF('../haskell/tests/abc.pgf', './abc.ngf')
    }).toThrow(Error)
  })

  test('GF', () => {
    expect(() => {
      PGF.bootNGF('../haskell/tests/basic.gf', './abc.ngf')
    }).toThrow(PGFError)
  })

  test('NGF', () => {
    expect(() => {
      PGF.bootNGF('./basic.ngf', './abc.ngf')
    }).toThrow(PGFError)
  })

  test('existing', () => {
    expect(() => {
      PGF.bootNGF('../haskell/tests/basic.pgf', './basic.ngf')
    }).toThrow(Error)
  })
})

// ----------------------------------------------------------------------------

describe('readNGF', () => {
  // beforeAll(() => {
  //   try {
  //     fs.unlinkSync('./basic.ngf')
  //   } catch {
  //     // empty
  //   }
  // })

  test('valid', () => {
    const gr = PGF.readNGF('./basic.ngf')
    expect(gr.getCategories().length).toBeGreaterThan(0)
  })

  test('non-existent', () => {
    expect(() => {
      PGF.readNGF('./abc.ngf')
    }).toThrow(Error)
  })

  test('GF', () => {
    expect(() => {
      PGF.readNGF('../haskell/tests/basic.gf')
    }).toThrow(PGFError)
  })

  test('PGF', () => {
    expect(() => {
      PGF.readNGF('../haskell/tests/basic.pgf')
    }).toThrow(PGFError)
  })
})

// ----------------------------------------------------------------------------

describe('newNGF', () => {
  beforeAll(() => {
    try {
      fs.unlinkSync('./empty.ngf')
    } catch {
      // empty
    }
  })

  test('file', () => {
    const gr = PGF.newNGF('empty', './empty.ngf')
    expect(gr.getCategories().length).toBe(0)
  })

  test('memory', () => {
    const gr = PGF.newNGF('empty')
    expect(gr.getCategories().length).toBe(0)
  })

  test('existing', () => {
    expect(() => {
      PGF.newNGF('empty', './basic.ngf')
    }).toThrow(Error)
  })
})

// ----------------------------------------------------------------------------

describe('abstract syntax', () => {
  let gr: PGFGrammar

  beforeAll(() => {
    gr = PGF.readPGF('../haskell/tests/basic.pgf')
  })

  afterAll(() => {
    gr.release()
  })

  test('abstract name', () => {
    expect(gr.getAbstractName()).toBe('basic')
  })

  test('categories', () => {
    expect(gr.getCategories()).toEqual(['Float', 'Int', 'N', 'P', 'S', 'String'])
  })

  test('functions', () => {
    expect(gr.getFunctions()).toEqual(['c', 'floatLit', 'ind', 'intLit', 'nat', 's', 'stringLit', 'z'])
  })

  describe('function is constructor', () => {
    test('s', () => {
      expect(gr.functionIsConstructor('s')).toBe(true)
    })
    test('z', () => {
      expect(gr.functionIsConstructor('z')).toBe(true)
    })
    test('c', () => {
      expect(gr.functionIsConstructor('c')).toBe(true)
    })
    test('ind', () => {
      expect(gr.functionIsConstructor('ind')).toBe(false)
    })
  })

  describe('functions by category', () => {
    test('N', () => {
      expect(gr.functionsByCategory('N')).toEqual(['s', 'z'])
    })
    test('S', () => {
      expect(gr.functionsByCategory('S')).toEqual(['c', 'floatLit', 'intLit', 'stringLit'])
    })
    test('X', () => {
      expect(gr.functionsByCategory('X')).toEqual([])
    })
  })

  describe('probabilities', () => {
    test.skip('category existing', () => {
      expect(gr.categoryProbability('S')).toBeDefined()
    })

    test('category non-existent', () => {
      expect(gr.categoryProbability('NP')).toBeUndefined()
    })

    test('function existing', () => {
      expect(gr.functionProbability('c')).toBeDefined() // returns -0 (!)
    })

    test('function non-existent', () => {
      expect(gr.functionProbability('mkC')).toBeUndefined()
    })
  })
})

// ----------------------------------------------------------------------------

describe('types', () => {
  describe('read', () => {
    test('invalid', () => {
      expect(() => {
        PGF.readType('->')
      }).toThrow(PGFError)
    })

    test('1', () => {
      const t1 = PGF.readType('A')
      const t2 = PGF.readType('A')
      const t3 = PGF.readType('B')
      expect(t1).toEqual(t2)
      expect(t1).not.toEqual(t3)
    })

    test('2', () => {
      const t1 = PGF.readType('A -> B')
      const t2 = PGF.readType('A->B')
      const t3 = PGF.readType('B->B')
      expect(t1).toEqual(t2)
      expect(t1).not.toEqual(t3)
    })

    test('3', () => {
      const t1 = PGF.readType('A -> B -> C')
      const t2 = PGF.readType('A->B   ->    C')
      expect(t1).toEqual(t2)
    })

    test('4', () => {
      const t1 = PGF.readType('(x : N) -> P x')
      const t2 = mkType([mkDepHypo('x', mkType([], 'N', []))], 'P', [new ExprFun('x')])
      expect(t1).toEqual(t2)
    })
  })

  describe.skip('show', () => {
    test('1', () => {
      const type = mkType([], 'N', [])
      expect(PGF.showType([], type)).toEqual('N')
    })

    test('2', () => {
      const type = mkType([mkHypo(mkType([], 'N', []))], 'N', [])
      expect(PGF.showType([], type)).toEqual('N -> N')
    })

    test('3', () => {
      const type = mkType([mkHypo(mkType([mkHypo(mkType([], 'N', []))], 'N', []))], 'N', [])
      expect(PGF.showType([], type)).toEqual('(N -> N) -> N')
    })

    test('4', () => {
      const type = mkType([mkDepHypo('x', mkType([], 'N', []))], 'P', [new ExprVar(0)])
      expect(PGF.showType([], type)).toEqual('(x : N) -> P x')
    })

    test('5', () => {
      const type = mkType([mkDepHypo('f', mkType([mkHypo(mkType([], 'N', []))], 'N', []))], 'P', [new ExprApp(new ExprVar(0), new ExprFun('z'))])
      expect(PGF.showType([], type)).toEqual('(f : N -> N) -> P (f z)')
    })

    test('6', () => {
      const type = mkType([mkDepHypo('f', mkType([mkHypo(mkType([], 'N', []))], 'N', []))], 'P', [new ExprApp(new ExprVar(0), new ExprVar(1))])
      expect(PGF.showType(['n'], type)).toEqual('(f : N -> N) -> P (f n)')
    })

    test('7', () => {
      const type = mkType([mkImplHypo('f', mkType([mkHypo(mkType([], 'N', []))], 'N', []))], 'P', [new ExprApp(new ExprVar(0), new ExprVar(1))])
      expect(PGF.showType(['n'], type)).toEqual('({f} : N -> N) -> P (f n)')
    })

    test('8', () => {
      const type = mkType([mkDepHypo('x', mkType([], 'N', [])), mkHypo(mkType([], 'P', [new ExprVar(0)]))], 'S', [])
      expect(PGF.showType(['n'], type)).toEqual('(x : N) -> P x -> S')
    })

    test('9', () => {
      const type = mkType([mkDepHypo('x', mkType([], 'N', [])), mkDepHypo('y', mkType([], 'P', [new ExprVar(0)]))], 'S', [])
      expect(PGF.showType(['n'], type)).toEqual('(x : N) -> (y : P x) -> S')
    })
  })
})

// ----------------------------------------------------------------------------

describe('expressions', () => {
  test('invalid', () => {
    expect(() => {
      PGF.readExpr('->')
    }).toThrow(PGFError)
  })

  describe('literals', () => {
    test('small integer', () => {
      const e1 = PGF.readExpr('123')
      const e2 = new ExprLit(123)
      const e3 = new ExprLit(456)
      expect(e1).toEqual(e2)
      expect(e1).not.toEqual(e3)
    })

    test('negative integer', () => {
      const e1 = PGF.readExpr('-123')
      const e2 = new ExprLit(-123)
      const e3 = new ExprLit(-456)
      expect(e1).toEqual(e2)
      expect(e1).not.toEqual(e3)
    })

    test('big integer', () => {
      const e1 = PGF.readExpr('774763251095801167872')
      const e2 = new ExprLit(BigInt('774763251095801167872'))
      expect(e1).toEqual(e2)
    })

    test('negative big integer', () => {
      const e1 = PGF.readExpr('-774763251095801167872')
      const e2 = new ExprLit(BigInt('-774763251095801167872'))
      expect(e1).toEqual(e2)
    })

    test('really big integer', () => {
      const e1 = PGF.readExpr('7747632510958011678729003251095801167999')
      const e2 = new ExprLit(BigInt('7747632510958011678729003251095801167999'))
      const e3 = new ExprLit(BigInt('7747632510958011678729003251095801167990'))
      expect(e1).toEqual(e2)
      expect(e1).not.toEqual(e3)
    })

    test('negative really big integer', () => {
      const e1 = PGF.readExpr('-7747632510958011678729003251095801167999')
      const e2 = new ExprLit(BigInt('-7747632510958011678729003251095801167999'))
      const e3 = new ExprLit(BigInt('-7747632510958011678729003251095801167990'))
      expect(e1).toEqual(e2)
      expect(e1).not.toEqual(e3)
    })

    test('float', () => {
      const e1 = PGF.readExpr('3.142')
      const e2 = new ExprLit(3.142)
      const e3 = new ExprLit(2.014)
      expect(e1).toEqual(e2)
      expect(e1).not.toEqual(e3)
    })

    test('negative float', () => {
      const e1 = PGF.readExpr('-3.142')
      const e2 = new ExprLit(-3.142)
      const e3 = new ExprLit(-2.014)
      expect(e1).toEqual(e2)
      expect(e1).not.toEqual(e3)
    })

    test('string', () => {
      const e1 = PGF.readExpr('"abc"')
      const e2 = new ExprLit('abc')
      const e3 = new ExprLit('def')
      expect(e1).toEqual(e2)
      expect(e1).not.toEqual(e3)
    })

    test('string unicode', () => {
      const e1 = PGF.readExpr('"açġħ"')
      const e2 = new ExprLit('açġħ')
      const e3 = new ExprLit('acgh')
      expect(e1).toEqual(e2)
      expect(e1).not.toEqual(e3)
    })
  })

  describe('functions', () => {
    test('simple', () => {
      const e1 = PGF.readExpr('f')
      expect(e1).toBeInstanceOf(ExprFun)
      expect((e1 as ExprFun).name).toEqual('f')

      const e2 = new ExprFun('f')
      const e3 = new ExprFun('g')
      expect(e1).toEqual(e2)
      expect(e1).not.toEqual(e3)
    })

    test('application 1', () => {
      const e1 = PGF.readExpr('f x y')
      expect(e1).toBeInstanceOf(ExprApp)
      expect((e1 as ExprApp).arg).toEqual(new ExprFun('y'))

      const e2 = new ExprApp(
        new ExprApp(
          new ExprFun('f'),
          new ExprFun('x')
        ), new ExprFun('y')
      )
      expect(e1).toEqual(e2)
    })

    test('application 2', () => {
      const e1 = PGF.readExpr('f (g x)')
      const e2 = new ExprApp(
        new ExprFun('f'),
        new ExprApp(
          new ExprFun('g'),
          new ExprFun('x')
        )
      )
      expect(e1).toEqual(e2)
    })

    test('application 3', () => {
      const e1 = PGF.readExpr('f {g x}')
      const e2 = new ExprApp(
        new ExprFun('f'),
        new ExprImplArg(
          new ExprApp(
            new ExprFun('g'),
            new ExprFun('x')
          )
        )
      )
      expect(e1).toEqual(e2)
    })
  })

  describe('variables', () => {
    test('meta 1', () => {
      const e1 = PGF.readExpr('?')
      expect(e1).toBeInstanceOf(ExprMeta)
      const e2 = new ExprMeta()
      const e3 = new ExprMeta(0)
      const e4 = new ExprMeta(1)
      expect(e1).toEqual(e2)
      expect(e1).toEqual(e3)
      expect(e1).not.toEqual(e4)
    })

    test('meta 2', () => {
      const e1 = PGF.readExpr('?42')
      const e2 = new ExprMeta(42)
      expect(e1).toEqual(e2)
    })
  })
})
