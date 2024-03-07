package fhetest.Generate

import fhetest.LibConfig
import fhetest.Utils.*
import scala.util.Random

def generateLibConfig(encType: ENC_TYPE): LibConfig = {
  val randomScheme =
    if encType == ENC_TYPE.ENC_INT then Scheme.values(Random.nextInt(2))
    else Scheme.CKKS

  val randomEncParams = {
    // TODO: Currently only MultDepth is random
    val randomRingDim = 32768
    val randomMultDepth = Random.nextInt(10)
    val randomPlainMod = 65537
    EncParams(randomRingDim, randomMultDepth, randomPlainMod)
  }
  val randomFirstModSize: Int = Random.nextInt(100)
  val randomScalingModSize: Int = Random.nextInt(100)
  val randomSecurityLevel =
    SecurityLevel.values(Random.nextInt(SecurityLevel.values.length))
  val randomScalingTechnique =
    ScalingTechnique.values(Random.nextInt(ScalingTechnique.values.length))
  // len must be larger than 0
  val randomLenOpt: Option[Int] = Some(Random.between(1, 100000 + 1))
  val randomBoundOpt: Option[Int | Double] = randomScheme match {
    case Scheme.BFV | Scheme.BGV =>
      Some(Random.nextInt(100000))
    case Scheme.CKKS => Some(Random.between(0, math.pow(2, 64)))
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
