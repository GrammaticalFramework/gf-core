// Untested!
import os from 'os'

export const BE: boolean = os.endianness() === 'BE'

export let WORDSIZE_BITS: number
switch (process.arch) {
  case 'arm':
  case 'ia32':
  case 'ppc':
  case 's390':
  case 'x32':
    WORDSIZE_BITS = 32
    break

  case 'arm64':
  case 'mips':
  case 'mipsel':
  case 'ppc64':
  case 's390x':
  case 'x64':
    WORDSIZE_BITS = 64
    break

  default:
    throw Error(`Unknown architecture: ${process.arch}`)
}

export const WORDSIZE_BYTES = WORDSIZE_BITS / 8

export let LINT_BASE: bigint

switch (WORDSIZE_BITS) {
  case 64: LINT_BASE = BigInt('10000000000000000000'); break
  case 32: LINT_BASE = BigInt('1000000000'); break
  case 16: LINT_BASE = BigInt('10000'); break
  case 8: LINT_BASE = BigInt('100'); break
  default: throw Error(`Unsupported word size: ${WORDSIZE_BITS}`)
}
