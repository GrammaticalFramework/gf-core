import PGF from '../index'

// readPGF

test('readPGF successful', () => {
  let gr = PGF.readPGF('../haskell/tests/basic.pgf')
})

test('readPGF missing file', () => {
  expect(() => {
    PGF.readPGF('abc.pgf')
  }).toThrow()
})

// abstract syntax

test('abstract name', () => {
  let gr = PGF.readPGF('../haskell/tests/basic.pgf')
  expect(gr.getAbstractName()).toBe('basic')
})

test('categories', () => {
  let gr = PGF.readPGF('../haskell/tests/basic.pgf')
  expect(gr.getCategories()).toEqual(['Float','Int','N','P','S','String'])
})

test('functions', () => {
  let gr = PGF.readPGF('../haskell/tests/basic.pgf')
  expect(gr.getFunctions()).toEqual(['c','ind','s','z'])
})
