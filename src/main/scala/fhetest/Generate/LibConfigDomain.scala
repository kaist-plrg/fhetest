package fhetest.Generate

import fhetest.Utils.*

type RingDim = Int
type FirstModSize = Int
type PlainMod = Int

case class LibConfigDomain(
  scheme: Scheme,
  ringDim: List[Int],
  mulDepth: Int => List[Int],
  plainMod: RingDim => List[Int],
  firstModSize: Scheme => List[Int],
  scalingModSize: Scheme => FirstModSize => List[Int],
  securityLevel: List[SecurityLevel],
  scalingTechnique: Scheme => List[ScalingTechnique],
  lenMin: Scheme => RingDim => Int,
  lenMax: Scheme => RingDim => Int,
  boundMin: Scheme => PlainMod => FirstModSize => Int | Double,
  boundMax: Scheme => PlainMod => FirstModSize => Int | Double,
  rotateBound: List[Int],
) {
  def stringify(): String =
    s"""{scheme: ${scheme}}
{encParams: EncParams(${ringDim}, ${mulDepth}, ${plainMod})}
{(firstModSize, scalingModSize): (${firstModSize}, ${scalingModSize})}
{securityLevel: ${securityLevel}}
{scalingTechnique: ${scalingTechnique}}
{lenMax: ${lenMax}}
{boundMax: ${boundMax}}
{rotateBoundOpt: ${rotateBound}}
"""
}
