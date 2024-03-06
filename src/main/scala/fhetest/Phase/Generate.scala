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

import java.io.*;
import javax.print.attribute.EnumSyntax
import scala.jdk.CollectionConverters._

case class Generate(
  encType: ENC_TYPE,
  strategy: Strategy = Strategy.Random,
  checkValid: Boolean = true,
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

  val absProgGen = strategy.getGenerator

  val allAbsPrograms = absProgGen.generateAbsPrograms(encType)

  def apply(nOpt: Option[Int]): LazyList[T2Program] = {
    println(s"Genrating Strategy: $strategy")
    val absPrograms = nOpt match {
      case Some(n) => allAbsPrograms.take(n)
      case None    => allAbsPrograms
    }
    val adjustedAbsPrograms: LazyList[AbsProgram] = for {
      absProgram <- absPrograms
    } yield {
      val assigned = absProgram.assignRandValues()
      val adjusted = assigned.adjustScale(encType)
      adjusted
    }
    val resultAbsPrograms = if (checkValid) {
      adjustedAbsPrograms.filter(_.isValid)
    } else { adjustedAbsPrograms.filterNot(_.isValid) }
    resultAbsPrograms.map(toT2Program)
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
            val result = Execute(backend)
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
