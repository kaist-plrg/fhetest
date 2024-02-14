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
              val mulDepth = getMulDepth(concretized)
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
      }
    }

  // TODO: current length = 100, it can be changed to larger value
  def concretizeTemplate(template: Templete): Templete =
    return assignRandIntValues(template, 100, 100)

  // TODO: This is just a temporary solution for making symbolTable and encType available
  val (_: Goal, symbolTable, encType) =
    val baseStr = """
        int main(void) {
          EncInt x, y;
          int c;
          print (x);
          return 0;
        }
      """
    val baseStream = new ByteArrayInputStream(baseStr.getBytes("UTF-8"))
    Parse(baseStream)

  // TODO: current print only 10 values, it can be changed to larger value
  def createNewBaseTemplate(): Goal = {
    val baseStr = """
    int main(void) {
      EncInt x, y;
      int c;
      print_batched (x, 10);
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
    vc: Int,
  ): Templete =
    val assignments =
      List(AssignVec("x", vxs), AssignVec("y", vys), Assign("c", vc))
    return assignments ++ template

  def assignRandIntValue(template: Templete, bound: Int): Templete =
    val vx = Random.between(0, bound)
    val vy = Random.between(0, bound)
    return assignIntValue(template, vx, vy)

  // TODO: Currently, T2 DSL does not support negative numbers
  def assignRandIntValues(template: Templete, len: Int, bound: Int): Templete =
    val l = Random.between(1, len)
    val vxs = List.fill(l)(Random.between(0, bound))
    val vys = List.fill(l)(Random.between(0, bound))
    // TODO: Currently, c is bounded by 10. It can be changed to larger value
    val vc = Random.between(0, 10)
    return assignIntValues(template, vxs, vys, vc)

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
    case Rot(l, r)       => "rotate_left(x, c);"
  def concretize(s: Stmt) = parseStmt(toString(s))

  type Templete = List[Stmt]
  def toString(t: Templete): String = t.map(toString).mkString("\n")
  def getMulDepth(t: Templete): Int = t.count {
    case Mul(_, _) => true; case _ => false
  }

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
    Rot(V, V),
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
