package fhetest.Generate

import fhetest.LibConfig
import fhetest.Utils.*
import scala.util.Random

def generateLibConfig(): LibConfig = {
  val randomScheme = Scheme.CKKS
  // val randomScheme = Scheme.values(Random.nextInt(Scheme.values.length))
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
  val randomLenOpt: Option[Int] = Some(Random.nextInt(100000))
  val randomBoundOpt: Option[Int | Double] = randomScheme match {
    case Scheme.BFV | Scheme.BGV =>
      Some(Random.nextInt(BigInt(2).pow(100).toInt))
    case Scheme.CKKS => Some(Random.nextDouble() * math.pow(2, 100))
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
