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
    expect(gr.getCategories()).toEqual(['Float','Int','N','P','S','String'])
  })

  test('functions', () => {
    expect(gr.getFunctions()).toEqual(['c','ind','s','z'])
  })
})
