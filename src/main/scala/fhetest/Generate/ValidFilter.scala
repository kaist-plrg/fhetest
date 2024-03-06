package fhetest.Generate

import fhetest.LibConfig
import fhetest.Utils.*

def mulDepthIsSmall(realMulDepth: Int, configMulDepth: Int): Boolean =
  realMulDepth < configMulDepth

def firstModSizeIsLargest(firstModSize: Int, scalingModSize: Int): Boolean =
  scalingModSize <= firstModSize

def modSizeIsUpto60bits(firstModSize: Int, scalingModSize: Int): Boolean =
  (firstModSize <= 60) && (scalingModSize <= 60)

def ringDimIsPowerOfTwo(n: Int): Boolean = (n > 0) && ((n & (n - 1)) == 0)

def plainModIsPositive(m: Int): Boolean = m > 0

def plainModEnableBatching(m: Int, n: Int): Boolean = (m % n) == 1

def lenIsLessThanRingDim(len: Int, n: Int, scheme: Scheme): Boolean =
  scheme match {
    case Scheme.CKKS => len <= (n / 2)
    case _           => len <= n
  }

def boundIsLessThanPowerOfModSize(
  bound: Int | Double,
  firstModSize: Int,
): Boolean = bound match {
  case intBound: Int       => intBound < math.pow(2, firstModSize)
  case doubleBound: Double => doubleBound < math.pow(2, firstModSize)
}

def boundIsLessThanPlainMod(
  bound: Int | Double,
  plainMod: Int,
): Boolean = bound match {
  case intBound: Int => intBound < plainMod
  case _: Double     => true
}
