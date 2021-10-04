import PGF from '../index'

test('readPGF successful', () => {
  let gr = PGF.readPGF('../haskell/tests/basic.pgf')
})

test('readPGF missing file', () => {
  expect(() => {
    PGF.readPGF('abc.pgf')
  }).toThrow()
})

test('abstract name', () => {
  let gr = PGF.readPGF('../haskell/tests/basic.pgf')
  expect(gr.getAbstractName()).toBe('basic')
})
