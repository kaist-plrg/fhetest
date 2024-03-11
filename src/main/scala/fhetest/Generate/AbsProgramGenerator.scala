package fhetest.Generate

import scala.util.Random
import fhetest.LibConfig
import fhetest.Utils.ENC_TYPE

// Template Generation Strategy
enum Strategy:
  case Exhaustive, Random

extension (s: Strategy)
  def getGenerator: AbsProgramGenerator = s match {
    case Strategy.Exhaustive => ExhaustiveGenerator
    case Strategy.Random     => RandomGenerator
  }

// AbsProgram Generator
trait AbsProgramGenerator {
  def generateAbsPrograms(
    encType: ENC_TYPE,
    validCheck: Boolean,
  ): LazyList[AbsProgram]
}

object ExhaustiveGenerator extends AbsProgramGenerator {
  def generateAbsPrograms(
    encType: ENC_TYPE,
    validCheck: Boolean,
  ): LazyList[AbsProgram] = {
    def allAbsProgramsOfSize(n: Int): LazyList[AbsProgram] = {
      n match {
        case 1 =>
          allAbsStmts
            .map(stmt => List(stmt))
            .map(AbsProgram(_, generateLibConfig(encType, validCheck)))
        case _ =>
          for {
            stmt <- allAbsStmts
            program <- allAbsProgramsOfSize(n - 1)
          } yield AbsProgram(
            stmt :: program.absStmts,
            generateLibConfig(encType, validCheck),
          )
      }
    }
    LazyList.from(1).flatMap(allAbsProgramsOfSize)
  }

}

object RandomGenerator extends AbsProgramGenerator {
  def generateAbsPrograms(
    encType: ENC_TYPE,
    validCheck: Boolean,
  ): LazyList[AbsProgram] = {
    def randomAbsProgramOfSize(n: Int): AbsProgram = {
      val absStmts = (1 to n).map(_ => Random.shuffle(allAbsStmts).head).toList
      AbsProgram(absStmts, generateLibConfig(encType, validCheck))
    }
    // Generate Lengths from 1 to inf
    // LazyList.from(1)

    // Generate Random Lengths from 1 to 20
    val randomLength: LazyList[Int] =
      LazyList.continually(Random.nextInt(20) + 1)

    randomLength.map(randomAbsProgramOfSize)
  }
}
