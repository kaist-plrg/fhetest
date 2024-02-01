package fhetest.Phase

import fhetest.Utils.*
import org.twc.terminator.SymbolTable;
import org.twc.terminator.t2dsl_compiler.*;

import org.twc.terminator.t2dsl_compiler.T2DSLparser.ParseException;
import org.twc.terminator.t2dsl_compiler.T2DSLparser.T2DSLParser;
import org.twc.terminator.t2dsl_compiler.T2DSLsyntaxtree.*;
import java.nio.file.{Files, Paths};

import java.io.*;
import javax.print.attribute.EnumSyntax
import scala.jdk.CollectionConverters._
import scala.util.Random

case object Generate {

  def apply(backend: Backend, encType: ENC_TYPE, n: Int) =
    for {
      program <- allPrograms.take(n)
    } {
      // withBackendTempDir(
      //  backend,
      //  { workspaceDir =>
      //    given DirName = workspaceDir
      //    val ast = baseAst
      //    // appendToBaseAst(exampleStmt)
      //    appendToBaseAst(program)
      //    Print(ast, symbolTable, encType, backend)
      //    Execute(backend)
      //  },
      // )
      given DirName = getWorkspaceDir(backend)
      val template = buildTemplate(program)
      val ast = concretizeTemplate(template)
      Print(ast, symbolTable, encType, backend)
      Execute(backend)
    }

  def concretizeTemplate(template: Goal): Goal =
    return assignRandIntValues(template, 1000)

  // FIXME: This is just a temporary solution for making symbolTable and encType available
  val (_: Goal, symbolTable, encType) =
    val baseStr = """
        int main(void) {
          EncInt x, y;
          print (x);
          return 0;
        }
      """
    val baseStream = new ByteArrayInputStream(baseStr.getBytes("UTF-8"))
    Parse(baseStream)

  def createNewBaseTemplate(): Goal = {
    val baseStr = """
    int main(void) {
      EncInt x, y;
      print_batched (x, 5);
      return 0;
    }
  """
    val baseStream = new ByteArrayInputStream(baseStr.getBytes("UTF-8"))
    Parse(baseStream)._1
  }

  def assignIntValue(template: Goal, vx: Int, vy: Int): Goal =
    val XStr = s"x = $vx;"
    val YStr = s"y = $vy;"
    print(XStr)
    print(YStr)
    val assignments = List(XStr, YStr)
    val stmts = assignments.map(parseStmt)
    val templateStmts = template.f0.f7.nodes
    templateStmts.addAll(0, stmts.asJava)
    return template

  // vxs = [1, 2, 3], vys = [4, 5, 6] => x = { 1, 2, 3 }; y = { 4, 5, 6 };
  def assignIntValues(template: Goal, vxs: List[Int], vys: List[Int]): Goal =
    val XStr = s"x = {${vxs.mkString(",")}};"
    val YStr = s"y = {${vys.mkString(",")}};"
    val assignments = List(XStr, YStr)
    val stmts = assignments.map(parseStmt)
    val templateStmts = template.f0.f7.nodes
    templateStmts.addAll(0, stmts.asJava)
    return template

  def assignRandIntValue(template: Goal, bound: Int): Goal =
    val vx = Random.between(0, bound)
    val vy = Random.between(0, bound)
    return assignIntValue(template, vx, vy)

  // current length = 5
  def assignRandIntValues(template: Goal, bound: Int): Goal =
    val vxs = List.fill(5)(Random.between(0, bound))
    val vys = List.fill(5)(Random.between(0, bound))
    return assignIntValues(template, vxs, vys)

  def parseStmt(stmtStr: String): Statement =
    val input_stream: InputStream = new ByteArrayInputStream(
      stmtStr.getBytes("UTF-8"),
    )
    T2DSLParser(input_stream).Statement()

  def buildTemplate(stmts: List[Statement]): Goal =
    val base = createNewBaseTemplate()
    val baseStmts = base.f0.f7.nodes
    baseStmts.addAll(0, stmts.asJava)
    return base

  trait Stmt
  case class Var()
  case class Add(l: Var, r: Var) extends Stmt
  case class Sub(l: Var, r: Var) extends Stmt
  case class Mul(l: Var, r: Var) extends Stmt
  case class Rot(l: Var, r: Var) extends Stmt

  def concretize(s: Stmt) = s match
    case Add(l, r) => parseStmt("x += y;")
    case Sub(l, r) => parseStmt("x -= y;")
    case Mul(l, r) => parseStmt("x *= y;")
    // case Rot(l, r) => parseStmt("rotate_left(x, c);")

  type Program = List[Stmt]
  val V = Var()

// 가능한 모든 Stmt를 생성하는 함수
  def allStmts: LazyList[Stmt] = LazyList(
    Add(V, V),
    Sub(V, V),
    Mul(V, V),
  )

// 주어진 길이에 대해 가능한 모든 프로그램을 생성하는 함수
  def allProgramsOfSize(n: Int): LazyList[Program] = n match {
    case 1 => allStmts.map(stmt => List(stmt))
    case _ =>
      for {
        stmt <- allStmts
        program <- allProgramsOfSize(n - 1)
      } yield stmt :: program
  }

// 모든 길이에 대해 가능한 모든 프로그램을 생성하는 LazyList
  val allPrograms: LazyList[List[Statement]] =
    LazyList.from(1).flatMap(allProgramsOfSize).map(_.map(concretize))

  def generateTemplate() = ???
}
