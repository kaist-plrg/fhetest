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
  strategy: Strategy = Strategy.Exhaustive,
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

  val tempGen = strategy.getGenerator

  val allTemplates = tempGen.generateTemplates()

  def apply(nOpt: Option[Int]): LazyList[String] = {
    println(s"Genrating Strategy: $strategy")
    val templates = nOpt match {
      case Some(n) => allTemplates.take(n)
      case None    => allTemplates
    }
    for {
      template <- templates
    } yield {
      val concretized = concretizeTemplate(template)
      stringifyWithBaseStr(concretized)
    }
  }

  // This is for testing purpose
  def show(backends: List[Backend], n: Int) =
    for {
      template <- allTemplates.take(n)
    } {
      // TODO: currently concretize 5 times for each template
      // TODO: store the results into a json file and print it by parsing the json file
      print("=" * 80 + "\n")
      print("<Program>\n")
      print(s"$template\n")
      print("=" * 80 + "\n")
      val concretized = concretizeTemplate(template)
      print(s"$concretized\n")
      print("-" * 80 + "\n")

      val ast: Goal = buildTemplate(concretized)
      val result = Interp(ast, 32768, 65537)
      print("CLEAR" + " : " + result + "\n")
      for {
        backend <- backends
      } {
        withBackendTempDir(
          backend,
          { workspaceDir =>
            given DirName = workspaceDir
            val mulDepth = concretized.getMulDepth
            // Default RingDim, PlainModulus with MulDepth
            val encParams = EncParams(32768, mulDepth, 65537)
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
      // }
    }

  // TODO: current length = 100, it can be changed to larger value
  def concretizeTemplate(template: Template): Template = encType match
    case ENC_TYPE.ENC_INT    => template.assignRandValues(100, 100)
    case ENC_TYPE.ENC_DOUBLE => template.assignRandValues(100, 100.0)

  def boilerplate(): (Goal, SymbolTable, _) =
    val baseStream = new ByteArrayInputStream(baseStr.getBytes("UTF-8"))
    Parse(baseStream)

  // TODO: current print only 10 values, it can be changed to larger value
  def createNewBaseTemplate(): Goal = boilerplate()._1

  def parseStmt(stmtStr: String): Statement =
    val input_stream: InputStream = new ByteArrayInputStream(
      stmtStr.getBytes("UTF-8"),
    )
    T2DSLParser(input_stream).Statement()

  def stringifyWithBaseStr(t: Template): String =
    baseStrFront + t.stringify + baseStrBack
  def buildTemplate(temp: Template): Goal =
    val stmts = temp.map(_.stringify).map(parseStmt)
    val base = createNewBaseTemplate()
    val baseStmts = base.f0.f7.nodes
    baseStmts.addAll(0, stmts.asJava)
    return base
}
