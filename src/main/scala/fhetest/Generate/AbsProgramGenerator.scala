package fhetest.Generate

import scala.util.Random
import fhetest.LibConfig

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
  def generateAbsPrograms(): LazyList[AbsProgram]
  // val libConfig: LibConfig = LibConfig()
}

object ExhaustiveGenerator extends AbsProgramGenerator {
  def generateAbsPrograms(): LazyList[AbsProgram] = {
    def allAbsProgramsOfSize(n: Int): LazyList[AbsProgram] = {
      val libConfig = generateLibConfig()
      n match {
        case 1 =>
          allAbsStmts.map(stmt => List(stmt)).map(AbsProgram(_, libConfig))
        case _ =>
          for {
            stmt <- allAbsStmts
            program <- allAbsProgramsOfSize(n - 1)
          } yield AbsProgram(stmt :: program.absStmts, libConfig)
      }
    }
    LazyList.from(1).flatMap(allAbsProgramsOfSize)
  }

}

object RandomGenerator extends AbsProgramGenerator {
  def generateAbsPrograms(): LazyList[AbsProgram] = {
    def randomAbsProgramOfSize(n: Int): AbsProgram = {
      val absStmts = (1 to n).map(_ => Random.shuffle(allAbsStmts).head).toList
      val libConfig = generateLibConfig()
      AbsProgram(absStmts, libConfig)
    }
    // Generate Lengths from 1 to inf
    // LazyList.from(1)

    // Generate Random Lengths from 1 to 20
    val randomLength: LazyList[Int] =
      LazyList.continually(Random.nextInt(20) + 1)

    randomLength.map(randomAbsProgramOfSize)
  }
}
