package fhetest.Phase

import fhetest.Utils.*
import fhetest.Generate.*
import fhetest.Generate.Utils.*
import org.twc.terminator.SymbolTable;
import org.twc.terminator.t2dsl_compiler.*;

import org.twc.terminator.t2dsl_compiler.T2DSLparser.ParseException;
import org.twc.terminator.t2dsl_compiler.T2DSLparser.T2DSLParser;
import org.twc.terminator.t2dsl_compiler.T2DSLsyntaxtree.*;
import java.nio.file.{Files, Paths};
import scala.util.Random

import java.io.*;
import javax.print.attribute.EnumSyntax
import scala.jdk.CollectionConverters._

case class Generate(
  encType: ENC_TYPE,
  strategy: Strategy = Strategy.Random,
  validFilter: Boolean = true,
) {
  // TODO : This boilerplate code is really ugly. But I cannot find a better way to do this.
  val baseStrFront = encType match {
    case ENC_TYPE.ENC_INT =>
      "int main(void) { EncInt x, y; int yP; int c; "
    case ENC_TYPE.ENC_DOUBLE =>
      "int main(void) { EncDouble x, y; double yP; int c; "
  }
  val baseStrBack = " print_batched (x, 20); return 0; } "
  val baseStr = baseStrFront + baseStrBack

  val symbolTable = boilerplate()._2

  val absProgGen = strategy.getGenerator(encType, validFilter)

  val allAbsPrograms = absProgGen.generateAbsPrograms()

  def apply(nOpt: Option[Int]): LazyList[T2Program] = {
    println(s"Genrating Strategy: $strategy")
    val adjustedAbsPrograms: LazyList[AbsProgram] = for {
      absProgram <- allAbsPrograms
    } yield {
      val assigned = absProgram.assignRandValues()
      val adjusted = assigned.adjustScale(encType)
      adjusted
    }

    val resultAbsPrograms: LazyList[AbsProgram] = adjustedAbsPrograms
    // val resultAbsPrograms: LazyList[AbsProgram] = if (validFilter) {
    //   adjustedAbsPrograms.filter(_.isValid)
    // } else {
    //   // val numOfValidFilter = 10
    //   // val programsWithEquivClasses: LazyList[(AbsProgram, List[Boolean])] =
    //   //   adjustedAbsPrograms.map({ pgm =>
    //   //     (pgm, pgm.getInvalidEquivClassList())
    //   //   })
    //   // def filterSequencially(
    //   //   absPrograms: LazyList[(AbsProgram, List[Boolean])],
    //   //   idx: Int,
    //   // ): LazyList[AbsProgram] =
    //   //   if (absPrograms.isEmpty)
    //   //     LazyList.empty // unreachable
    //   //   else if (idx == numOfValidFilter) filterSequencially(absPrograms, 0)
    //   //   else {
    //   //     val (pgm, equivClassList) = absPrograms.head
    //   //     val equivClass = equivClassList.apply(idx)
    //   //     if (equivClass)
    //   //       pgm #:: filterSequencially(absPrograms.tail, idx + 1)
    //   //     else filterSequencially(absPrograms, idx + 1)
    //   //   }
    //   // filterSequencially(programsWithEquivClasses, 0)

    //   val equivClassIdx = LazyList.from(0)
    //   adjustedAbsPrograms
    //     .zip(equivClassIdx)
    //     .filter { case (pgm, idx) => pgm.invalidEquivClass(idx) }
    //     .map(_._1)
    // }
    val takenResultAbsPrograms = nOpt match {
      case Some(n) => resultAbsPrograms.take(n)
      case None    => resultAbsPrograms
    }
    takenResultAbsPrograms.map(toT2Program)
  }

  // This is for testing purpose
  def show(backends: List[Backend], n: Int, encType: ENC_TYPE) =
    for {
      absProgram <- allAbsPrograms.take(n)
    } {
      print("=" * 80 + "\n")
      print("<Program>\n")
      print(s"${absProgram.absStmts}\n")
      print("=" * 80 + "\n")
      val assigned = absProgram.assignRandValues()
      val adjusted = assigned.adjustScale(encType)
      print(s"$adjusted\n")
      print("-" * 80 + "\n")

      val ast: Goal = buildAbsProgram(adjusted)
      val encParams = absProgram.libConfig.encParams
      val result = Interp(ast, encParams.ringDim, encParams.plainMod)
      print("CLEAR" + " : " + result + "\n")
      for {
        backend <- backends
      } {
        withBackendTempDir(
          backend,
          { workspaceDir =>
            given DirName = workspaceDir
            val mulDepth = adjusted.mulDepth
            Print(
              ast,
              symbolTable,
              encType,
              backend,
              encParamsOpt = Some(encParams),
            )
            val result = Execute(backend, None)
            print(backend.toString + " : " + result)
          },
        )
      }
      print("-" * 80 + "\n")
    }

  def boilerplate(): (Goal, SymbolTable, _) =
    val baseStream = new ByteArrayInputStream(baseStr.getBytes("UTF-8"))
    Parse(baseStream)

  def createNewBaseAbsProgram(): Goal = boilerplate()._1

  def parseStmt(stmtStr: String): Statement =
    val input_stream: InputStream = new ByteArrayInputStream(
      stmtStr.getBytes("UTF-8"),
    )
    T2DSLParser(input_stream).Statement()

  def toT2Program(absProg: AbsProgram): T2Program =
    val programStr = baseStrFront + absProg.absStmts
      .map(_.stringify())
      .foldLeft("")(_ + _) + baseStrBack
    T2Program(programStr, absProg.libConfig)

  def buildAbsProgram(absProg: AbsProgram): Goal =
    val stmts = absProg.absStmts.map(_.stringify()).map(parseStmt)
    val base = createNewBaseAbsProgram()
    val baseStmts = base.f0.f7.nodes
    baseStmts.addAll(0, stmts.asJava)
    return base
}
