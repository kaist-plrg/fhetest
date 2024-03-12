package fhetest.Generate

import fhetest.LibConfig
import fhetest.Utils.*
import scala.util.Random

def generateLibConfig(encType: ENC_TYPE, valid: Boolean): LibConfig = {
  lazy val randomScheme =
    if encType == ENC_TYPE.ENC_INT then Scheme.values(Random.nextInt(2))
    else Scheme.CKKS

  lazy val randomEncParams = {
    // TODO: Currently only MultDepth is random
    val randomRingDim = 32768
    val randomMultDepth =
      if valid then Random.nextInt(10 + 1)
      else Random.between(-10, 10 + 1)
    val randomPlainMod = 65537
    EncParams(randomRingDim, randomMultDepth, randomPlainMod)
  }
  // modSizeIsUpto60bits
  lazy val randomFirstModSize: Int =
    if valid then
      if randomScheme == Scheme.BFV then Random.between(30, 60 + 1)
      else Random.nextInt(60 + 1)
    else Random.between(-100, 100 + 1)
  // firstModSizeIsLargest
  // openFHEBFVModuli
  lazy val randomScalingModSize: Int =
    if valid then
      if randomScheme == Scheme.BFV then
        Random.between(30, randomFirstModSize + 1)
      else Random.nextInt(randomFirstModSize + 1)
    else Random.between(-100, 100 + 1)
  lazy val randomSecurityLevel =
    SecurityLevel.values(Random.nextInt(SecurityLevel.values.length))
  lazy val randomScalingTechnique =
    ScalingTechnique.values(Random.nextInt(ScalingTechnique.values.length))
  // len must be larger than 0
  // lenIsLessThanRingDim
  lazy val randomLenOpt: Option[Int] =
    if valid then
      val upper = randomScheme match {
        case Scheme.CKKS => (randomEncParams.ringDim / 2)
        case _           => randomEncParams.ringDim
      }
      Some(Random.between(1, upper + 1))
    else Some(Random.between(1, 100000 + 1))
  lazy val randomBoundOpt: Option[Int | Double] =
    if valid then
      randomScheme match {
        case Scheme.BFV | Scheme.BGV =>
          Some(Random.between(1, randomEncParams.plainMod + 1))
        case Scheme.CKKS =>
          Some(Random.between(1, math.pow(2, randomFirstModSize) + 1))
      }
    else
      randomScheme match {
        case Scheme.BFV | Scheme.BGV =>
          Some(Random.between(1, 1000 + 1))
        case Scheme.CKKS => Some(Random.between(1, math.pow(2, 64) + 1))
      }
  LibConfig(
    randomScheme,
    randomEncParams,
    randomFirstModSize,
    randomScalingModSize,
    randomSecurityLevel,
    randomScalingTechnique,
    randomLenOpt,
    randomBoundOpt,
  )
}
