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

  def apply(backends: List[Backend], encType: ENC_TYPE, n: Int) =
    for {
      template <- allTempletes.take(n)
    } {
      // TODO: currently concretize 5 times for each template
      // TODO: store the results into a json file and print it by parsing the json file
      print("=" * 80 + "\n")
      print("<Program>\n")
      print(toString(template) + "\n")
      print("=" * 80 + "\n")
      for _ <- 0 until 5 do {
        val concretized = concretizeTemplate(template)
        print(toString(concretized) + "\n")
        print("-" * 80 + "\n")

        val ast = buildTemplate(concretized)
        val result = Interp(ast, 32768, 65537)
        print("CLEAR" + " : " + result + "\n")
        for {
          backend <- backends
        } {
          withBackendTempDir(
            backend,
            { workspaceDir =>
              given DirName = workspaceDir
              Print(ast, symbolTable, encType, backend)
              val result = Execute(backend)
              print(backend.toString + " : " + result)
            },
          )
        }
        print("-" * 80 + "\n")
      }
    }

  def concretizeTemplate(template: Templete): Templete =
    return assignRandIntValues(template, 100)

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

  def assignIntValue(template: Templete, vx: Int, vy: Int): Templete =
    val assignments = List(Assign("x", vx), Assign("y", vy))
    return assignments ++ template

  // vxs = [1, 2, 3], vys = [4, 5, 6] => x = { 1, 2, 3 }; y = { 4, 5, 6 };
  def assignIntValues(
    template: Templete,
    vxs: List[Int],
    vys: List[Int],
  ): Templete =
    val assignments = List(AssignVec("x", vxs), AssignVec("y", vys))
    return assignments ++ template

  def assignRandIntValue(template: Templete, bound: Int): Templete =
    val vx = Random.between(0, bound)
    val vy = Random.between(0, bound)
    return assignIntValue(template, vx, vy)

  // current length = 5
  // TODO : Currently, it only supports the length of 5
  // TODO : Currently, T2 DSL does not support negative numbers
  def assignRandIntValues(template: Templete, bound: Int): Templete =
    val vxs = List.fill(5)(Random.between(0, bound))
    val vys = List.fill(5)(Random.between(0, bound))
    return assignIntValues(template, vxs, vys)

  def parseStmt(stmtStr: String): Statement =
    val input_stream: InputStream = new ByteArrayInputStream(
      stmtStr.getBytes("UTF-8"),
    )
    T2DSLParser(input_stream).Statement()

  trait Stmt
  case class Var()
  case class Assign(l: String, r: Int) extends Stmt
  case class AssignVec(l: String, r: List[Int]) extends Stmt
  case class Add(l: Var, r: Var) extends Stmt
  case class Sub(l: Var, r: Var) extends Stmt
  case class Mul(l: Var, r: Var) extends Stmt
  case class Rot(l: Var, r: Var) extends Stmt

  val V = Var()

  def toString(s: Stmt) = s match
    case Assign(l, r)    => s"$l = $r;"
    case AssignVec(l, r) => s"$l = {${r.mkString(",")}};"
    case Add(l, r)       => "x += y;"
    case Sub(l, r)       => "x -= y;"
    case Mul(l, r)       => "x *= y;"
    // case Rot(l, r) => "rotate_left(x, c);"
  def concretize(s: Stmt) = parseStmt(toString(s))

  type Templete = List[Stmt]
  def toString(t: Templete): String = t.map(toString).mkString("\n")

  def buildTemplate(temp: Templete): Goal =
    val stmts = temp.map(toString).map(parseStmt)
    val base = createNewBaseTemplate()
    val baseStmts = base.f0.f7.nodes
    baseStmts.addAll(0, stmts.asJava)
    return base

// 가능한 모든 Stmt를 생성하는 함수
  def allStmts: LazyList[Stmt] = LazyList(
    Add(V, V),
    Sub(V, V),
    Mul(V, V),
  )

// 주어진 길이에 대해 가능한 모든 템플릿을 생성하는 함수
  def allTempletesOfSize(n: Int): LazyList[Templete] = n match {
    case 1 => allStmts.map(stmt => List(stmt))
    case _ =>
      for {
        stmt <- allStmts
        program <- allTempletesOfSize(n - 1)
      } yield stmt :: program
  }

// 모든 길이에 대해 가능한 모든 템플릿을 생성하는 LazyList
  val allTempletes: LazyList[Templete] =
    LazyList.from(1).flatMap(allTempletesOfSize)
}
