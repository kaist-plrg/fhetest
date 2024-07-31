package fhetest.Checker

// import fhetest.Generate.ValidFilter
// import fhetest.Generate.getValidFilterList
import fhetest.Generate.Utils.InvalidFilterIdx

// val validFilters =
//   getValidFilterList().map(filter => filter.getSimpleName.replace("$", ""))

def getCryptoParamsFromFilterIdx(
  invalidFilterIdx: InvalidFilterIdx,
): List[String] = {
  val invalidFilterName = validFilters.apply(invalidFilterIdx)
  invalidFilterName match {
    case "FilterBoundIsLessThanPlainMod"       => List("PlaintextModulus")
    case "FilterBoundIsLessThanPowerOfModSize" => List("FirstModSize")
    case "FilterFirstModSizeIsLargest" => List("FirstModSize", "ScalingModSize")
    case "FilterLenIsLessThanRingDim"  => List("RingDim")
    case "FilterModSizeIsBeteween14And60bits" =>
      List("FirstModSize", "ScalingModSize")
    case "FilterMulDepthIsEnough"      => List("MultiplicativeDepth")
    case "FilterMulDepthIsNotNegative" => List("MultiplicativeDepth")
    case "FilterOpenFHEBFVModuli"      => List("FirstModSize", "ScalingModSize")
    case "FilterPlainModEnableBatching"   => List("PlaintextModulus")
    case "FilterPlainModIsPositive"       => List("PlaintextModulus")
    case "FilterRingDimIsPowerOfTwo"      => List("RingDim")
    case "FilterRotateBoundTest"          => List()
    case "FilterScalingTechniqueByScheme" => List("ScalingTechnique")
    case s: String =>
      throw new Exception(s"Related cryptoparams for $s is undefined.")
  }
}
