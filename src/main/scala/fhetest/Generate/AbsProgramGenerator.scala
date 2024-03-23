package fhetest.Generate

import scala.util.Random
import fhetest.Utils.ENC_TYPE

// Template Generation Strategy
enum Strategy:
  case Exhaustive, Random

extension (s: Strategy)
  def getGenerator(
    encType: ENC_TYPE,
    validFilter: Boolean,
  ): AbsProgramGenerator = s match {
    case Strategy.Exhaustive =>
      ExhaustiveGenerator(encType: ENC_TYPE, validFilter: Boolean)
    case Strategy.Random =>
      RandomGenerator(encType: ENC_TYPE, validFilter: Boolean)
  }

// AbsProgram Generator
trait AbsProgramGenerator(encType: ENC_TYPE, validFilter: Boolean) {
  def generateAbsPrograms(): LazyList[AbsProgram]
  val lcGen =
    if validFilter then ValidLibConfigGenerator(encType)
    else InvalidLibConfigGenerator(encType)
}

case class ExhaustiveGenerator(encType: ENC_TYPE, validFilter: Boolean)
  extends AbsProgramGenerator(encType: ENC_TYPE, validFilter: Boolean) {
  val libConfigGens = lcGen.getLibConfigGenerators()
  def generateAbsPrograms(): LazyList[AbsProgram] = {
    def allAbsProgramsOfSize(n: Int): LazyList[AbsProgram] =
      n match {
        case 1 =>
          for {
            stmt <- allAbsStmts
            libConfigGen <- libConfigGens
          } yield {
            val stmts = List(stmt)
            AbsProgram(stmts, libConfigGen(stmts))
          }
        case _ =>
          for {
            stmt <- allAbsStmts
            program <- allAbsProgramsOfSize(n - 1)
            libConfigGen <- libConfigGens
          } yield {
            val stmts = stmt :: program.absStmts
            AbsProgram(stmts, libConfigGen(stmts))
          }
      }
    LazyList.from(1).flatMap(allAbsProgramsOfSize)
  }

}

case class RandomGenerator(encType: ENC_TYPE, validFilter: Boolean)
  extends AbsProgramGenerator(encType: ENC_TYPE, validFilter: Boolean) {
  def generateAbsPrograms(): LazyList[AbsProgram] = {
    def randomAbsStmtsOfSize(n: Int): List[AbsStmt] =
      (1 to n).map(_ => Random.shuffle(allAbsStmts).head).toList
    val libConfigGens = lcGen.getLibConfigGenerators()

    // Generate Lengths from 1 to inf
    // LazyList.from(1)

    // Generate Random Lengths from 1 to 20
    val randomLength: LazyList[Int] =
      LazyList.continually(Random.nextInt(20) + 1)

    for {
      len <- randomLength
      libConfigGen <- libConfigGens
    } yield {
      val stmts = randomAbsStmtsOfSize(len)
      AbsProgram(stmts, libConfigGen(stmts))
    }
  }
}
