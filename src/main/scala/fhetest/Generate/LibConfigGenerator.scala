package fhetest.Generate

import fhetest.Utils.*
import scala.util.Random
import fhetest.Generate.Utils.combinations

val ringDimCandidates: List[Int] = // also in ValidFilter
  List(8192, 16384, 32768)
  // List(8192, 16384, 32768, 65536, 131072) // also in ValidFilter

def getLibConfigUniverse(scheme: Scheme) = LibConfigDomain(
  scheme = scheme,
  ringDim = ringDimCandidates,
  mulDepth = (realMulDepth: Int) => (-20 to 20).toList,
  plainMod = (ringDim: Int) => List(65537),
  firstModSize = (scheme: Scheme) => (-100 to 100).toList,
  scalingModSize =
    (scheme: Scheme) => (firstModSize: Int) => (-100 to 100).toList,
  securityLevel = SecurityLevel.values.toList,
  scalingTechnique = (scheme: Scheme) => ScalingTechnique.values.toList,
  lenMin = (scheme: Scheme) => (ringDim: Int) => 1,
  lenMax = (scheme: Scheme) => (ringDim: Int) => 100000,
  boundMin = (scheme: Scheme) =>
    (plainMod: Int) =>
      (firstModSize: Int) =>
        scheme match {
          case Scheme.CKKS => 1d
          case _           => 1
        },
  boundMax = (scheme: Scheme) =>
    (plainMod: Int) =>
      (firstModSize: Int) =>
        scheme match {
          case Scheme.CKKS => math.pow(2, 64)
          case _           => 1000
        },
  rotateBound = (0 to 40).toList,
)

trait LibConfigGenerator(encType: ENC_TYPE) {
  def getLibConfigGenerators(): LazyList[List[AbsStmt] => LibConfig]
  val validFilters = classOf[ValidFilter].getDeclaredClasses.toList
    .filter { cls =>
      classOf[ValidFilter]
        .isAssignableFrom(cls) && cls != classOf[ValidFilter]
    }
}

case class ValidLibConfigGenerator(encType: ENC_TYPE)
  extends LibConfigGenerator(encType) {
  def getLibConfigGenerators(): LazyList[List[AbsStmt] => LibConfig] = {
    val libConfigGeneratorFromAbsStmts = (absStmts: List[AbsStmt]) => {
      val randomScheme =
        if encType == ENC_TYPE.ENC_INT then Scheme.values(Random.nextInt(2))
        else Scheme.CKKS
      val libConfigUniverse = getLibConfigUniverse(randomScheme)
      val filteredLibConfigDomain = validFilters.foldLeft(libConfigUniverse)({
        case (curLibConfigDomain, curValidFilter) => {
          val constructor = curValidFilter.getDeclaredConstructors.head
          constructor.setAccessible(true)
          val f = constructor
            .newInstance(curLibConfigDomain, true)
            .asInstanceOf[ValidFilter]
          f.getFilteredLibConfigDomain()
        }
      })
      randomLibConfigFromDomain(
        true,
        absStmts,
        randomScheme,
        filteredLibConfigDomain,
      )
    }
    LazyList.continually(libConfigGeneratorFromAbsStmts)
  }
}

case class InvalidLibConfigGenerator(encType: ENC_TYPE)
  extends LibConfigGenerator(encType) {
  val totalNumOfFilters = validFilters.length
  val allCombinations =
    (1 to totalNumOfFilters).toList.flatMap(combinations(_, totalNumOfFilters))
  // TODO: currently generate only 1 test case for each class
  // val numOfTC = 10
  val allCombinations_lazy = LazyList.from(allCombinations)
  def getLibConfigGenerators(): LazyList[List[AbsStmt] => LibConfig] = for {
    combination <- allCombinations_lazy
  } yield {
    val libConfigGeneratorFromAbsStmts = (absStmts: List[AbsStmt]) => {
      val randomScheme =
        if encType == ENC_TYPE.ENC_INT then Scheme.values(Random.nextInt(2))
        else Scheme.CKKS
      val libConfigUniverse = getLibConfigUniverse(randomScheme)
      val filteredLibConfigDomain = validFilters.foldLeft(libConfigUniverse)({
        case (curLibConfigDomain, curValidFilter) => {
          val curValidFilterIdx = validFilters.indexOf(curValidFilter)
          val inInValid = combination.contains(curValidFilterIdx)
          val constructor = curValidFilter.getDeclaredConstructors.head
          constructor.setAccessible(true)
          val f = constructor
            .newInstance(curLibConfigDomain, !inInValid)
            .asInstanceOf[ValidFilter]
          f.getFilteredLibConfigDomain()
        }
      })
      randomLibConfigFromDomain(
        false,
        absStmts,
        randomScheme,
        filteredLibConfigDomain,
      )
    }
    libConfigGeneratorFromAbsStmts
  }
}

// TODO: No handling for empty domain
def randomLibConfigFromDomain(
  validFilter: Boolean,
  absStmts: List[AbsStmt],
  randomScheme: Scheme,
  filteredLibConfigDomain: LibConfigDomain,
): LibConfig = {
  val randomRingDim = Random.shuffle(filteredLibConfigDomain.ringDim).head
  val randomMulDepth = {
    val realMulDepth: Int = absStmts.count {
      case Mul(_, _) | MulP(_, _) => true; case _ => false
    }
    Random.shuffle((filteredLibConfigDomain.mulDepth)(realMulDepth)).head
  }
  val randomPlainMod =
    Random.shuffle((filteredLibConfigDomain.plainMod)(randomRingDim)).head
  val randomFirstModSize =
    Random
      .shuffle((filteredLibConfigDomain.firstModSize)(randomScheme))
      .head
  val randomScalingModSize = Random
    .shuffle(
      (filteredLibConfigDomain.scalingModSize)(randomScheme)(
        randomFirstModSize,
      ),
    )
    .head
  val randomSecurityLevel =
    Random.shuffle(filteredLibConfigDomain.securityLevel).head
  val randomScalingTechnique = Random
    .shuffle((filteredLibConfigDomain.scalingTechnique)(randomScheme))
    .head
  val randomLenOpt: Option[Int] = {
    val upper =
      (filteredLibConfigDomain.lenMax)(randomScheme)(randomRingDim)
    val lower =
      (filteredLibConfigDomain.lenMin)(randomScheme)(randomRingDim)
    Some(Random.between(lower, upper + 1))
  }
  val randomBoundOpt: Option[Int | Double] = {
    val upper = (filteredLibConfigDomain.boundMax)(randomScheme)(
      randomPlainMod,
    )(randomFirstModSize)
    val lower = (filteredLibConfigDomain.boundMin)(randomScheme)(
      randomPlainMod,
    )(randomFirstModSize)
    lower match {
      case li: Int =>
        upper match {
          case ui: Int => Some(Random.between(li, ui + 1))
          case _       => Some(Random.between(1, 100000 + 1)) // unreachable
        }
      case ld: Double =>
        upper match {
          case ud: Int => Some(Random.between(ld, ud))
          case _ => Some(Random.between(1, math.pow(2, 64))) // unreachable
        }
    }
  }
  val randomRotateBoundOpt: Option[Int] =
    Some(Random.shuffle(filteredLibConfigDomain.rotateBound).head)

  LibConfig(
    randomScheme,
    EncParams(randomRingDim, randomMulDepth, randomPlainMod),
    randomFirstModSize,
    randomScalingModSize,
    randomSecurityLevel,
    randomScalingTechnique,
    randomLenOpt,
    randomBoundOpt,
    randomRotateBoundOpt,
  )
}
