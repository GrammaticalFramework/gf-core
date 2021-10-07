import PGF, { PGFGrammar } from '../index'
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
    }).toThrow(PGF.PGFError)
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
    }).toThrow(PGF.PGFError)
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
    }).toThrow(PGF.PGFError)
  })

  test('NGF', () => {
    expect(() => {
      PGF.bootNGF('./basic.ngf', './abc.ngf')
    }).toThrow(PGF.PGFError)
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
    }).toThrow(PGF.PGFError)
  })

  test('PGF', () => {
    expect(() => {
      PGF.readNGF('../haskell/tests/basic.pgf')
    }).toThrow(PGF.PGFError)
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
    expect(gr.getFunctions()).toEqual(['c', 'ind', 's', 'z'])
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
      expect(gr.functionsByCategory('S')).toEqual(['c'])
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

describe.only('expressions', () => {
  test('small integer', () => {
    const e1 = PGF.readExpr('123')
    const e2 = new PGF.ExprLit(123)
    const e3 = new PGF.ExprLit(456)
    expect(e1).toEqual(e2)
    expect(e1).not.toEqual(e3)
  })

  test('negative integer', () => {
    const e1 = PGF.readExpr('-123')
    const e2 = new PGF.ExprLit(-123)
    const e3 = new PGF.ExprLit(-456)
    expect(e1).toEqual(e2)
    expect(e1).not.toEqual(e3)
  })

  test('big integer', () => {
    const e1 = PGF.readExpr('774763251095801167872')
    const e2 = new PGF.ExprLit(BigInt('774763251095801167872'))
    expect(e1).toEqual(e2)
  })

  test('negative big integer', () => {
    const e1 = PGF.readExpr('-774763251095801167872')
    const e2 = new PGF.ExprLit(BigInt('-774763251095801167872'))
    expect(e1).toEqual(e2)
  })

  test('really big integer', () => {
    const e1 = PGF.readExpr('7747632510958011678729003251095801167999')
    const e2 = new PGF.ExprLit(BigInt('7747632510958011678729003251095801167999'))
    const e3 = new PGF.ExprLit(BigInt('7747632510958011678729003251095801167990'))
    expect(e1).toEqual(e2)
    expect(e1).not.toEqual(e3)
  })

  test('negative really big integer', () => {
    const e1 = PGF.readExpr('-7747632510958011678729003251095801167999')
    const e2 = new PGF.ExprLit(BigInt('-7747632510958011678729003251095801167999'))
    const e3 = new PGF.ExprLit(BigInt('-7747632510958011678729003251095801167990'))
    expect(e1).toEqual(e2)
    expect(e1).not.toEqual(e3)
  })

  test('float', () => {
    const e1 = PGF.readExpr('3.142')
    const e2 = new PGF.ExprLit(3.142)
    const e3 = new PGF.ExprLit(2.014)
    expect(e1).toEqual(e2)
    expect(e1).not.toEqual(e3)
  })

  test('negative float', () => {
    const e1 = PGF.readExpr('-3.142')
    const e2 = new PGF.ExprLit(-3.142)
    const e3 = new PGF.ExprLit(-2.014)
    expect(e1).toEqual(e2)
    expect(e1).not.toEqual(e3)
  })

  test('string', () => {
    const e1 = PGF.readExpr('"abc"')
    const e2 = new PGF.ExprLit('abc')
    const e3 = new PGF.ExprLit('def')
    expect(e1).toEqual(e2)
    expect(e1).not.toEqual(e3)
  })

  test('string unicode', () => {
    const e1 = PGF.readExpr('"açġħ"')
    const e2 = new PGF.ExprLit('açġħ')
    const e3 = new PGF.ExprLit('acgh')
    expect(e1).toEqual(e2)
    expect(e1).not.toEqual(e3)
  })
})
