package fhetest.Generate

import fhetest.Utils.*
import fhetest.Checker.schemeDecoder

// TODO: currently filters regarding plainMod, ringDim is commented since using fixed plaindMod, ringDim
// TODO: FilterRingDimIsPowerOfTwo just filter into fixed candidates when validFilter = true

/* Current validFilters */
// * defined & used in LibConfigGenerator
// * automatically arranged in alphabetical order
// val validFilters = List(
// 	FilterBoundIsLessThanPlainMod, // 0
// 	FilterBoundIsLessThanPowerOfModSize, // 1
// 	FilterFirstModSizeIsLargest, // 2
// 	FilterLenIsLessThanRingDim, // 3
// 	FilterModSizeIsBeteween14And60bits, // 4
// 	FilterMulDepthIsEnough, // 5
// 	FilterOpenFHEBFVModuli, // 6
// 	FilterPlainModEnableBatching, /* commented */
// 	FilterPlainModIsPositive, /* commented */
// 	FilterRingDimIsPowerOfTwo, /* commented */
// 	FilterScalingTechniqueByScheme // 7
// )

trait ValidFilter(prev: LibConfigDomain, validFilter: Boolean) {
  def getFilteredLibConfigDomain(): LibConfigDomain
}

object ValidFilter {
  // def mulDepthIsSmall(realMulDepth: Int, configMulDepth: Int): Boolean =
  //   realMulDepth < configMulDepth
  case class FilterMulDepthIsEnough(
    prev: LibConfigDomain,
    validFilter: Boolean,
  ) extends ValidFilter(prev, validFilter) {
    def getFilteredLibConfigDomain(): LibConfigDomain =
      LibConfigDomain(
        scheme = prev.scheme,
        ringDim = prev.ringDim,
        mulDepth =
          if (validFilter)
            (
              realMulDepth =>
                (prev.mulDepth)(realMulDepth).filter(_ > realMulDepth),
            )
          else
            (
              realMulDepth =>
                (prev.mulDepth)(realMulDepth).filterNot(_ > realMulDepth),
            ),
        plainMod = prev.plainMod,
        firstModSize = prev.firstModSize,
        scalingModSize = prev.scalingModSize,
        securityLevel = prev.securityLevel,
        scalingTechnique = prev.scalingTechnique,
        lenMin = prev.lenMin,
        lenMax = prev.lenMax,
        boundMin = prev.boundMin,
        boundMax = prev.boundMax,
        rotateBound = prev.rotateBound,
      )
  }

  // TODO: There are 2 options for this implementation
  // * Current implementation filters scalingModSize which is not greater than firstModSize
  // * Another option is to filter firstModSize to be not smaller than scalingModeSize
  // def firstModSizeIsLargest(firstModSize: Int, scalingModSize: Int): Boolean =
  //   scalingModSize <= firstModSize
  case class FilterFirstModSizeIsLargest(
    prev: LibConfigDomain,
    validFilter: Boolean,
  ) extends ValidFilter(prev, validFilter) {
    def getFilteredLibConfigDomain(): LibConfigDomain =
      LibConfigDomain(
        scheme = prev.scheme,
        ringDim = prev.ringDim,
        mulDepth = prev.mulDepth,
        plainMod = prev.plainMod,
        firstModSize = prev.firstModSize,
        scalingModSize =
          if (validFilter)
            (
              scheme =>
                firstModSize =>
                  (prev.scalingModSize)(scheme)(firstModSize)
                  .filter(_ <= firstModSize),
            )
          else
            (
              scheme =>
                firstModSize =>
                  (prev.scalingModSize)(scheme)(firstModSize)
                  .filterNot(_ <= firstModSize),
            ),
        securityLevel = prev.securityLevel,
        scalingTechnique = prev.scalingTechnique,
        lenMin = prev.lenMin,
        lenMax = prev.lenMax,
        boundMin = prev.boundMin,
        boundMax = prev.boundMax,
        rotateBound = prev.rotateBound,
      )
  }

  // https://github.com/openfheorg/openfhe-development/blob/main/src/pke/include/schemerns/rns-parametergeneration.h
  // Both firstModSize and scalingModSize are >= 14
  // def modSizeIsUpto60bits(firstModSize: Int, scalingModSize: Int): Boolean =
  //   (14 <= firstModSize <= 60) && (14 <= scalingModSize <= 60)
  case class FilterModSizeIsBeteween14And60bits(
    prev: LibConfigDomain,
    validFilter: Boolean,
  ) extends ValidFilter(prev, validFilter) {
    def getFilteredLibConfigDomain(): LibConfigDomain =
      LibConfigDomain(
        scheme = prev.scheme,
        ringDim = prev.ringDim,
        mulDepth = prev.mulDepth,
        plainMod = prev.plainMod,
        firstModSize =
          if (validFilter)
            (
              scheme =>
                (prev
                  .firstModSize)(scheme)
                  .filter({ case m => (m >= 14) && (m <= 60) }),
            )
          else
            (
              scheme =>
                (prev
                  .firstModSize)(scheme)
                  .filterNot({ case m => (m >= 14) && (m <= 60) }),
            ),
        scalingModSize =
          if (validFilter)
            (
              scheme =>
                firstModSize =>
                  (prev.scalingModSize)(scheme)(firstModSize)
                  .filter({ case m => (m >= 14) && (m <= 60) }),
            )
          else
            (
              scheme =>
                firstModSize =>
                  (prev.scalingModSize)(scheme)(firstModSize)
                  .filterNot({ case m => (m >= 14) && (m <= 60) }),
            ),
        securityLevel = prev.securityLevel,
        scalingTechnique = prev.scalingTechnique,
        lenMin = prev.lenMin,
        lenMax = prev.lenMax,
        boundMin = prev.boundMin,
        boundMax = prev.boundMax,
        rotateBound = prev.rotateBound,
      )
  }

  // OpenFHE/src/pke/lib/scheme/bfvrns/bfvrns-parametergeneration.cpp:53
  // BFVrns.ParamsGen: Number of bits in CRT moduli should be in the range from 30 to 60
  // def openFHEBFVModuli(
  //   scheme: Scheme,
  //   firstModSize: Int,
  //   scalingModSize: Int,
  // ): Boolean =
  //   !(scheme == Scheme.BFV) || ((30 <= firstModSize) && (30 <= scalingModSize))
  case class FilterOpenFHEBFVModuli(
    prev: LibConfigDomain,
    validFilter: Boolean,
  ) extends ValidFilter(prev, validFilter) {
    def getFilteredLibConfigDomain(): LibConfigDomain =
      LibConfigDomain(
        scheme = prev.scheme,
        ringDim = prev.ringDim,
        mulDepth = prev.mulDepth,
        plainMod = prev.plainMod,
        firstModSize =
          if (validFilter)
            (
              scheme =>
                if (scheme == Scheme.BFV)
                  (prev.firstModSize)(scheme).filter(_ >= 30)
                else (prev.firstModSize)(scheme),
            )
          else
            (
              scheme =>
                if (scheme == Scheme.BFV)
                  (prev.firstModSize)(scheme).filterNot(_ >= 30)
                else (prev.firstModSize)(scheme),
            ),
        scalingModSize =
          if (validFilter)
            (
              scheme =>
                firstModSize =>
                  if (scheme == Scheme.BFV)
                    (prev.scalingModSize)(scheme)(firstModSize).filter(_ >= 30)
                  else (prev.scalingModSize)(scheme)(firstModSize),
            )
          else
            (
              scheme =>
                firstModSize =>
                  if (scheme == Scheme.BFV)
                    (prev.scalingModSize)(scheme)(firstModSize)
                    .filterNot(_ >= 30)
                  else (prev.scalingModSize)(scheme)(firstModSize),
            ),
        securityLevel = prev.securityLevel,
        scalingTechnique = prev.scalingTechnique,
        lenMin = prev.lenMin,
        lenMax = prev.lenMax,
        boundMin = prev.boundMin,
        boundMax = prev.boundMax,
        rotateBound = prev.rotateBound,
      )
  }

  // TODO: This filter is not included since using fixed ringDim
  // TODO: FilterRingDimIsPowerOfTwo just filter into fixed candidates when validFilter = true
  // def ringDimIsPowerOfTwo(n: Int): Boolean = (n > 0) && ((n & (n - 1)) == 0)
  // case class FilterRingDimIsPowerOfTwo(
  //   prev: LibConfigDomain,
  //   validFilter: Boolean,
  // ) extends ValidFilter(prev, validFilter) {
  //   // TODO: currently use only few candidates in valid testing
  //   val ringDimCandidates: List[Int] = List(8192, 16384, 32768)
  //   def getFilteredLibConfigDomain(): LibConfigDomain =
  //     LibConfigDomain(
  //       scheme = prev.scheme,
  //       ringDim =
  //         if (validFilter)
  //           ringDimCandidates // TODO: prev.ringDim.filter({ case n => (n > 0) && ((n & (n - 1)) == 0) })
  //         else
  //           prev.ringDim.filterNot({
  //             case n => (n > 0) && ((n & (n - 1)) == 0)
  //           }),
  //       mulDepth = prev.mulDepth,
  //       plainMod = prev.plainMod,
  //       firstModSize = prev.firstModSize,
  //       scalingModSize = prev.scalingModSize,
  //       securityLevel = prev.securityLevel,
  //       scalingTechnique = prev.scalingTechnique,
  //       lenMin = prev.lenMin,
  //       lenMax = prev.lenMax,
  //       boundMin = prev.boundMin,
  //       boundMax = prev.boundMax,
  //       rotateBound = prev.rotateBound,
  //     )
  // }

  // TODO: This filter is not included since using fixed plainMod
  // def plainModIsPositive(m: Int): Boolean = m > 0
  // case class FilterPlainModIsPositive(
  //   prev: LibConfigDomain,
  //   validFilter: Boolean,
  // ) extends ValidFilter(prev, validFilter) {
  //   def getFilteredLibConfigDomain(): LibConfigDomain =
  //     LibConfigDomain(
  //       scheme = prev.scheme,
  //       ringDim = prev.ringDim,
  //       mulDepth = prev.mulDepth,
  //       plainMod =
  //         if (validFilter)
  //           (ringDim => (prev.plainMod)(ringDim).filter(_ > 0))
  //         else
  //           (ringDim => (prev.plainMod)(ringDim).filterNot(_ > 0)),
  //       firstModSize = prev.firstModSize,
  //       scalingModSize = prev.scalingModSize,
  //       securityLevel = prev.securityLevel,
  //       scalingTechnique = prev.scalingTechnique,
  //       lenMin = prev.lenMin,
  //       lenMax = prev.lenMax,
  //       boundMin = prev.boundMin,
  //       boundMax = prev.boundMax,
  //       rotateBound = prev.rotateBound,
  //     )
  // }

  // TODO: This filter is not included since using fixed plainMod
  // def plainModEnableBatching(m: Int, n: Int): Boolean = (m % (2 * n)) == 1
  // case class FilterPlainModEnableBatching(
  //   prev: LibConfigDomain,
  //   validFilter: Boolean,
  // ) extends ValidFilter(prev, validFilter) {
  //   def getFilteredLibConfigDomain(): LibConfigDomain =
  //     LibConfigDomain(
  //       scheme = prev.scheme,
  //       ringDim = prev.ringDim,
  //       mulDepth = prev.mulDepth,
  //       plainMod =
  //         if (validFilter)
  //           (
  //             ringDim =>
  //               (prev
  //                 .plainMod)(ringDim)
  //                 .filter({ case m => (m % (2 * ringDim)) == 1 }),
  //           )
  //         else
  //           (
  //             ringDim =>
  //               (prev
  //                 .plainMod)(ringDim)
  //                 .filterNot({ case m => (m % (2 * ringDim)) == 1 }),
  //           ),
  //       firstModSize = prev.firstModSize,
  //       scalingModSize = prev.scalingModSize,
  //       securityLevel = prev.securityLevel,
  //       scalingTechnique = prev.scalingTechnique,
  //       lenMin = prev.lenMin,
  //       lenMax = prev.lenMax,
  //       boundMin = prev.boundMin,
  //       boundMax = prev.boundMax,
  //       rotateBound = prev.rotateBound,
  //     )
  // }

  // Currently, exclude FLEXIBLEAUTOEXT in valid testing because of the following issue
  // https://openfhe.discourse.group/t/unexpected-behavior-with-setscalingmodsize-and-flexibleautoext-in-bgv-scheme/1111
  // And, exclude NORESCALE in CKKS scheme because it is not supported
  // https://openfhe.discourse.group/t/incorrect-result-when-using-norescale-scaling-technique-in-ckks-scheme/1119
  case class FilterScalingTechniqueByScheme(
    prev: LibConfigDomain,
    validFilter: Boolean,
  ) extends ValidFilter(prev, validFilter) {
    def getFilteredLibConfigDomain(): LibConfigDomain =
      LibConfigDomain(
        scheme = prev.scheme,
        ringDim = prev.ringDim,
        mulDepth = prev.mulDepth,
        plainMod = prev.plainMod,
        firstModSize = prev.firstModSize,
        scalingModSize = prev.scalingModSize,
        securityLevel = prev.securityLevel,
        scalingTechnique =
          if (validFilter)
            (
              scheme =>
                if (scheme == Scheme.CKKS)
                  List(
                    ScalingTechnique.FIXEDMANUAL,
                    ScalingTechnique.FIXEDAUTO,
                    ScalingTechnique.FLEXIBLEAUTO,
                  )
                else if (scheme == Scheme.BFV) List(ScalingTechnique.NORESCALE)
                else
                  List(
                    ScalingTechnique.NORESCALE,
                    ScalingTechnique.FIXEDMANUAL,
                    ScalingTechnique.FIXEDAUTO,
                    ScalingTechnique.FLEXIBLEAUTO,
                  ),
            )
          else
            (
              scheme =>
                if (scheme == Scheme.CKKS)
                  List(
                    ScalingTechnique.NORESCALE,
                    ScalingTechnique.FLEXIBLEAUTOEXT,
                  )
                else if (scheme == Scheme.BFV)
                  List(
                    ScalingTechnique.FIXEDMANUAL,
                    ScalingTechnique.FIXEDAUTO,
                    ScalingTechnique.FLEXIBLEAUTO,
                    ScalingTechnique.FLEXIBLEAUTOEXT,
                  )
                else List(ScalingTechnique.FLEXIBLEAUTOEXT),
            ),
        lenMin = prev.lenMin,
        lenMax = prev.lenMax,
        boundMin = prev.boundMin,
        boundMax = prev.boundMax,
        rotateBound = prev.rotateBound,
      )
  }

  // def lenIsLessThanRingDim(len: Int, n: Int, scheme: Scheme): Boolean =
  //   scheme match {
  //     case Scheme.CKKS => len <= (n / 2)
  //     case _           => len <= n
  //   }
  case class FilterLenIsLessThanRingDim(
    prev: LibConfigDomain,
    validFilter: Boolean,
  ) extends ValidFilter(prev, validFilter) {
    def getFilteredLibConfigDomain(): LibConfigDomain = if (validFilter)
      LibConfigDomain(
        scheme = prev.scheme,
        ringDim = prev.ringDim,
        mulDepth = prev.mulDepth,
        plainMod = prev.plainMod,
        firstModSize = prev.firstModSize,
        scalingModSize = prev.scalingModSize,
        securityLevel = prev.securityLevel,
        scalingTechnique = prev.scalingTechnique,
        lenMin = prev.lenMin,
        lenMax = (
          scheme =>
            ringDim =>
              if (scheme == Scheme.CKKS)
                (prev.lenMax)(scheme)(ringDim).min(ringDim / 2)
              else (prev.lenMax)(scheme)(ringDim).min(ringDim),
        ),
        boundMin = prev.boundMin,
        boundMax = prev.boundMax,
        rotateBound = prev.rotateBound,
      )
    else
      LibConfigDomain(
        scheme = prev.scheme,
        ringDim = prev.ringDim,
        mulDepth = prev.mulDepth,
        plainMod = prev.plainMod,
        firstModSize = prev.firstModSize,
        scalingModSize = prev.scalingModSize,
        securityLevel = prev.securityLevel,
        scalingTechnique = prev.scalingTechnique,
        lenMin = (
          scheme =>
            ringDim =>
              if (scheme == Scheme.CKKS)
                (prev.lenMax)(scheme)(ringDim).max(ringDim / 2 + 1)
              else (prev.lenMax)(scheme)(ringDim).max(ringDim + 1),
        ),
        lenMax = prev.lenMax,
        boundMin = prev.boundMin,
        boundMax = prev.boundMax,
        rotateBound = prev.rotateBound,
      )
  }

  // def boundIsLessThanPowerOfModSize(
  //   bound: Int | Double,
  //   firstModSize: Int,
  // ): Boolean = bound match {
  //   case intBound: Int       => intBound < math.pow(2, firstModSize)
  //   case doubleBound: Double => doubleBound < math.pow(2, firstModSize)
  // }
  case class FilterBoundIsLessThanPowerOfModSize(
    prev: LibConfigDomain,
    validFilter: Boolean,
  ) extends ValidFilter(prev, validFilter) {
    def getFilteredLibConfigDomain(): LibConfigDomain = if (validFilter)
      LibConfigDomain(
        scheme = prev.scheme,
        ringDim = prev.ringDim,
        mulDepth = prev.mulDepth,
        plainMod = prev.plainMod,
        firstModSize = prev.firstModSize,
        scalingModSize = prev.scalingModSize,
        securityLevel = prev.securityLevel,
        scalingTechnique = prev.scalingTechnique,
        lenMin = prev.lenMin,
        lenMax = prev.lenMax,
        boundMin = prev.boundMin,
        boundMax = (
          scheme =>
            plainMod =>
              firstModSize =>
                (prev.boundMax)(scheme)(plainMod)(firstModSize) match {
                  case intBound: Int =>
                    intBound.min(math.pow(2, firstModSize).toInt)
                  case doubleBound: Double =>
                    doubleBound.min(math.pow(2, firstModSize))
                },
        ),
        rotateBound = prev.rotateBound,
      )
    else
      LibConfigDomain(
        scheme = prev.scheme,
        ringDim = prev.ringDim,
        mulDepth = prev.mulDepth,
        plainMod = prev.plainMod,
        firstModSize = prev.firstModSize,
        scalingModSize = prev.scalingModSize,
        securityLevel = prev.securityLevel,
        scalingTechnique = prev.scalingTechnique,
        lenMin = prev.lenMin,
        lenMax = prev.lenMax,
        boundMin = (
          scheme =>
            plainMod =>
              firstModSize =>
                (prev.boundMax)(scheme)(plainMod)(firstModSize) match {
                  case intBound: Int =>
                    intBound.max(math.pow(2, firstModSize).toInt + 1)
                  case doubleBound: Double =>
                    doubleBound.max(math.pow(2, firstModSize) + 0.1)
                },
        ),
        boundMax = prev.boundMax,
        rotateBound = prev.rotateBound,
      )
  }

  // def boundIsLessThanPlainMod(
  //   bound: Int | Double,
  //   plainMod: Int,
  // ): Boolean = bound match {
  //   case intBound: Int => intBound < plainMod
  //   case _: Double     => true
  // }
  case class FilterBoundIsLessThanPlainMod(
    prev: LibConfigDomain,
    validFilter: Boolean,
  ) extends ValidFilter(prev, validFilter) {
    def getFilteredLibConfigDomain(): LibConfigDomain = if (validFilter)
      LibConfigDomain(
        scheme = prev.scheme,
        ringDim = prev.ringDim,
        mulDepth = prev.mulDepth,
        plainMod = prev.plainMod,
        firstModSize = prev.firstModSize,
        scalingModSize = prev.scalingModSize,
        securityLevel = prev.securityLevel,
        scalingTechnique = prev.scalingTechnique,
        lenMin = prev.lenMin,
        lenMax = prev.lenMax,
        boundMin = prev.boundMin,
        boundMax = (
          scheme =>
            plainMod =>
              firstModSize =>
                (prev.boundMax)(scheme)(plainMod)(firstModSize) match {
                  case intBound: Int       => intBound.min(plainMod)
                  case doubleBound: Double => doubleBound
                },
        ),
        rotateBound = prev.rotateBound,
      )
    else
      LibConfigDomain(
        scheme = prev.scheme,
        ringDim = prev.ringDim,
        mulDepth = prev.mulDepth,
        plainMod = prev.plainMod,
        firstModSize = prev.firstModSize,
        scalingModSize = prev.scalingModSize,
        securityLevel = prev.securityLevel,
        scalingTechnique = prev.scalingTechnique,
        lenMin = prev.lenMin,
        lenMax = prev.lenMax,
        boundMin = (
          scheme =>
            plainMod =>
              firstModSize =>
                (prev.boundMax)(scheme)(plainMod)(firstModSize) match {
                  case intBound: Int       => intBound.max(plainMod + 1)
                  case doubleBound: Double => doubleBound
                },
        ),
        boundMax = prev.boundMax,
        rotateBound = prev.rotateBound,
      )
  }

  // TODO: just for test -> remove later?
  // case class FilterRotateBoundTest(
  //   prev: LibConfigDomain,
  //   validFilter: Boolean,
  // ) extends ValidFilter(prev, validFilter) {
  //   def getFilteredLibConfigDomain(): LibConfigDomain =
  //     LibConfigDomain(
  //       scheme = prev.scheme,
  //       ringDim = prev.ringDim,
  //       mulDepth = prev.mulDepth,
  //       plainMod = prev.plainMod,
  //       firstModSize = prev.firstModSize,
  //       scalingModSize = prev.scalingModSize,
  //       securityLevel = prev.securityLevel,
  //       scalingTechnique = prev.scalingTechnique,
  //       lenMin = prev.lenMin,
  //       lenMax = prev.lenMax,
  //       boundMin = prev.boundMin,
  //       boundMax = prev.boundMax,
  //       rotateBound = (1 to 20).toList,
  //     )
  // }
}
