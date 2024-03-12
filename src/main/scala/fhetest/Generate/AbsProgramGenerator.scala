package fhetest.Generate

import scala.util.Random
import fhetest.LibConfig
import fhetest.Utils.ENC_TYPE

// Template Generation Strategy
enum Strategy:
  case Exhaustive, Random

extension (s: Strategy)
  def getGenerator(
    encType: ENC_TYPE,
    validCheck: Boolean,
  ): AbsProgramGenerator = s match {
    case Strategy.Exhaustive =>
      ExhaustiveGenerator(encType: ENC_TYPE, validCheck: Boolean)
    case Strategy.Random =>
      RandomGenerator(encType: ENC_TYPE, validCheck: Boolean)
  }

// AbsProgram Generator
trait AbsProgramGenerator(encType: ENC_TYPE, validCheck: Boolean) {
  def generateAbsPrograms(): LazyList[AbsProgram]
  val lcGen =
    if validCheck then ValidLibConfigGenerator(encType)
    else RandomLibConfigGenerator(encType)
}

case class ExhaustiveGenerator(encType: ENC_TYPE, validCheck: Boolean)
  extends AbsProgramGenerator(encType: ENC_TYPE, validCheck: Boolean) {
  def generateAbsPrograms(): LazyList[AbsProgram] = {
    def allAbsProgramsOfSize(n: Int): LazyList[AbsProgram] = {
      n match {
        case 1 =>
          allAbsStmts
            .map(stmt => List(stmt))
            .map(
              AbsProgram(
                _,
                lcGen.generateLibConfig(),
              ),
            )
        case _ =>
          for {
            stmt <- allAbsStmts
            program <- allAbsProgramsOfSize(n - 1)
          } yield AbsProgram(
            stmt :: program.absStmts,
            lcGen.generateLibConfig(),
          )
      }
    }
    LazyList.from(1).flatMap(allAbsProgramsOfSize)
  }

}

case class RandomGenerator(encType: ENC_TYPE, validCheck: Boolean)
  extends AbsProgramGenerator(encType: ENC_TYPE, validCheck: Boolean) {
  def generateAbsPrograms(): LazyList[AbsProgram] = {
    def randomAbsProgramOfSize(n: Int): AbsProgram = {
      val absStmts = (1 to n).map(_ => Random.shuffle(allAbsStmts).head).toList
      AbsProgram(absStmts, lcGen.generateLibConfig())
    }
    // Generate Lengths from 1 to inf
    // LazyList.from(1)

    // Generate Random Lengths from 1 to 20
    val randomLength: LazyList[Int] =
      LazyList.continually(Random.nextInt(20) + 1)

    randomLength.map(randomAbsProgramOfSize)
  }
}
