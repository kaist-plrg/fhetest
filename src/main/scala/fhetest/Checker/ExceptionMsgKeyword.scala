package fhetest.Checker

import fhetest.Generate.ValidFilter
import fhetest.Generate.getValidFilterList //TODO
import fhetest.Generate.Utils.InvalidFilterIdx

type LibConfigArgumentName = String

val libConfigArguments: Set[LibConfigArgumentName] =
  Set(
    "Scheme",
    "RingDim",
    "MulDepth",
    "PlaindMod",
    "ModSize",
    "FirstModSize",
    "ScalingModSize",
    "SecurityLevel",
    "ScalingTechnique",
    "Len",
    "Bound",
    "RotateBound",
  )

// TODO: fill out
// Note: Keywords should be in lower cases!
def mapLibConfigArgument2Keywords(
  argName: LibConfigArgumentName,
): Set[String] = {
  val commonKeywords =
    Set(" parameters", "ring dimension", " ringdim", " primes", " overflow")
  val modSizeKeywords = Set(" moduli", " bit_sizes", "bit length")
  val uniqueKeywords = argName match {
    case "Scheme"  => Set("scheme")
    case "RingDim" => Set("ring dimension", " ringdim")
    case "MulDepth" =>
      Set(
        "multiplicative depth",
        " towers",
        "end of modulus switching chain",
        "removing last element of dcrtpoly",
        "scale out of bounds",
        "encoded values are too large",
      )
    case "PlainMod"         => Set(" plain_modulus", " plaintext_modulus")
    case "ModSize"          => modSizeKeywords
    case "FirstModSize"     => modSizeKeywords ++ Set()
    case "ScalingModSize"   => modSizeKeywords ++ Set()
    case "SecurityLevel"    => Set("security level", " SecurityLevel")
    case "ScalingTechnique" => Set("security mode")
    case "Len" =>
      Set(
        "values_matrix size",
        " values_size",
        // "should not be greater than ringdim", // Currently, overlap with " ringDim" in commonKeywords
      )
    case "Bound"       => Set("encoded values are too large")
    case "RotateBound" => Set("out_of_range", "evalkey for index")
    case s: String =>
      throw new Exception(s"$s is not defined as LibConfigArgument.")
  }
  commonKeywords ++ uniqueKeywords
}

val validFilters =
  getValidFilterList().map(filter => filter.getSimpleName.replace("$", ""))

def mapFilterName2LibConfigArgumentMap(
  filterName: String,
): LibConfigArgumentName =
  filterName match {
    case "FilterBoundIsLessThanPlainMod"       => "Bound"
    case "FilterBoundIsLessThanPowerOfModSize" => "Bound"
    case "FilterFirstModSizeIsLargest"         => "FirstModSize"
    case "FilterLenIsLessThanRingDim"          => "Len"
    case "FilterModSizeIsBeteween14And60bits"  => "ModSize"
    case "FilterMulDepthIsEnough"              => "MulDepth"
    case "FilterMulDepthIsNotNegative"         => "MulDepth"
    case "FilterOpenFHEBFVModuli"              => "ModSize"
    case "FilterPlainModEnableBatching"        => "PlainMod"
    case "FilterPlainModIsPositive"            => "PlainMod"
    case "FilterRingDimIsPowerOfTwo"           => "RingDim"
    case "FilterRotateBoundTest"               => "RotateBound"
    case "FilterScalingTechniqueByScheme"      => "ScalingTechnique"
    case s: String => throw new Exception(s"Keyword for $s is undifined.")
  }

def getKeywordsFromFilters(
  invalidFilterIdxList: List[InvalidFilterIdx],
): Set[String] = {
  val invalidFilterNameList = invalidFilterIdxList.map {
    case i => validFilters.apply(i)
  }
  invalidFilterNameList.foldLeft(Set[String]()) { (acc, filterName) =>
    {
      val argName = mapFilterName2LibConfigArgumentMap(filterName)
      val keywords = mapLibConfigArgument2Keywords(argName)
      acc ++ keywords
    }
  }
}
