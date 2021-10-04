import PGF, { PGFGrammar } from '../index'
import fs from 'fs'

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

  // test('NGF', () => {
  //   expect(() => {
  //     PGF.readPGF('basic.ngf')
  //   }).toThrow(PGF.PGFError)
  // })
})

describe('bootNGF', () => {
  beforeAll(() => {
    try {
      fs.unlinkSync('./basic.ngf')
    } catch { }
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
    expect(gr.getCategories()).toEqual(['Float','Int','N','P','S','String'])
  })

  test('functions', () => {
    expect(gr.getFunctions()).toEqual(['c','ind','s','z'])
  })
})
