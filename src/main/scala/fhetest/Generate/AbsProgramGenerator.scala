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
    noFilterOpt: Boolean,
  ): AbsProgramGenerator = s match {
    case Strategy.Exhaustive =>
      ExhaustiveGenerator(
        encType: ENC_TYPE,
        validFilter: Boolean,
        noFilterOpt: Boolean,
      )
    case Strategy.Random =>
      RandomGenerator(
        encType: ENC_TYPE,
        validFilter: Boolean,
        noFilterOpt: Boolean,
      )
  }

// AbsProgram Generator
trait AbsProgramGenerator(
  encType: ENC_TYPE,
  validFilter: Boolean,
  noFilterOpt: Boolean,
) {
  // TODO: Revive if LazyList generation option is revived
  // def generateAbsPrograms(): LazyList[AbsProgram]
  def generateAbsPrograms(): Iterator[AbsProgram]
  val lcGen =
    if noFilterOpt then RandomLibConfigGenerator(encType)
    else if validFilter then ValidLibConfigGenerator(encType)
    else InvalidLibConfigGenerator(encType)
}

case class ExhaustiveGenerator(
  encType: ENC_TYPE,
  validFilter: Boolean,
  noFilterOpt: Boolean,
) extends AbsProgramGenerator(
    encType: ENC_TYPE,
    validFilter: Boolean,
    noFilterOpt: Boolean,
  ) {
  val libConfigGens = lcGen.getLibConfigGenerators()
  def generateAbsPrograms(): Iterator[AbsProgram] = {
    def allAbsProgramsOfSize(n: Int): Iterator[AbsProgram] =
      n match {
        case 1 =>
          for {
            stmt <- allAbsStmts.iterator
            libConfigGen <- libConfigGens.iterator
            stmts = List(stmt)
            libConfigOpt = libConfigGen(stmts)
            if libConfigOpt.isDefined
          } yield {
            val (libConfig, invalidFilterIdxList) = libConfigOpt.get
            AbsProgram(stmts, libConfig, invalidFilterIdxList)
          }
        case _ =>
          for {
            stmt <- allAbsStmts.iterator
            program <- allAbsProgramsOfSize(n - 1)
            libConfigGen <- libConfigGens.iterator
            stmts = stmt :: program.absStmts
            libConfigOpt = libConfigGen(stmts)
            if libConfigOpt.isDefined
          } yield {
            val (libConfig, invalidFilterIdxList) = libConfigOpt.get
            AbsProgram(stmts, libConfig, invalidFilterIdxList)
          }
      }
    Iterator.from(1).flatMap(allAbsProgramsOfSize)
  }
  // TODO: Revive if LazyList generation option is revived
  // def generateAbsPrograms(): LazyList[AbsProgram] = {
  //   def allAbsProgramsOfSize(n: Int): LazyList[AbsProgram] =
  //     n match {
  //       case 1 =>
  //         for {
  //           stmt <- allAbsStmts
  //           libConfigGen <- libConfigGens
  //           stmts = List(stmt)
  //           libConfigOpt = libConfigGen(stmts)
  //           if libConfigOpt.isDefined
  //         } yield {
  //           val (libConfig, invalidFilterIdxList) = libConfigOpt.get
  //           AbsProgram(stmts, libConfig, invalidFilterIdxList)
  //         }
  //       case _ =>
  //         for {
  //           stmt <- allAbsStmts
  //           program <- allAbsProgramsOfSize(n - 1)
  //           libConfigGen <- libConfigGens
  //           stmts = stmt :: program.absStmts
  //           libConfigOpt = libConfigGen(stmts)
  //           if libConfigOpt.isDefined
  //         } yield {
  //           val (libConfig, invalidFilterIdxList) = libConfigOpt.get
  //           AbsProgram(stmts, libConfig, invalidFilterIdxList)
  //         }
  //     }
  //   LazyList.from(1).flatMap(allAbsProgramsOfSize)
  // }

}

case class RandomGenerator(
  encType: ENC_TYPE,
  validFilter: Boolean,
  noFilterOpt: Boolean,
) extends AbsProgramGenerator(
    encType: ENC_TYPE,
    validFilter: Boolean,
    noFilterOpt: Boolean,
  ) {
  def generateAbsPrograms(): Iterator[AbsProgram] = {
    def randomAbsStmtsOfSize(n: Int): List[AbsStmt] =
      (1 to n).map(_ => Random.shuffle(allAbsStmts).head).toList
    val libConfigGens = lcGen.getLibConfigGenerators().iterator

    Iterator.continually {
      // Generate Random Lengths from 1 to 20
      val len = Random.nextInt(20) + 1
      val stmts = randomAbsStmtsOfSize(len)
      libConfigGens
        .map(libConfigGen => libConfigGen(stmts))
        .find(_.isDefined)
        .flatten
        .map { case (libConfig, invalidFilterIdxList) =>
          AbsProgram(stmts, libConfig, invalidFilterIdxList)
        }
    }.flatten
  }
  // TODO: Revive if LazyList generation option is revived
  // def generateAbsPrograms(): LazyList[AbsProgram] = {
  //   def randomAbsStmtsOfSize(n: Int): List[AbsStmt] =
  //     (1 to n).map(_ => Random.shuffle(allAbsStmts).head).toList
  //   val libConfigGens = lcGen.getLibConfigGenerators()

  //   // Generate Lengths from 1 to inf
  //   // LazyList.from(1)

  //   // Generate Random Lengths from 1 to 20
  //   val randomLength: LazyList[Int] =
  //     LazyList.continually(Random.nextInt(20) + 1)

  //   for {
  //     len <- randomLength
  //     libConfigGen <- libConfigGens
  //     stmts = randomAbsStmtsOfSize(len)
  //     libConfigOpt = libConfigGen(stmts)
  //     if libConfigOpt.isDefined
  //   } yield {
  //     val (libConfig, invalidFilterIdxList) = libConfigOpt.get
  //     AbsProgram(stmts, libConfig, invalidFilterIdxList)
  //   }
  // }
}
