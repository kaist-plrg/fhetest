package fhetest.Phase

import fhetest.Utils.*
import org.twc.terminator.t2dsl_compiler.T2DSLsyntaxtree.*;
import org.twc.terminator.SymbolTable;

import java.nio.file.{Files, Paths};
import java.io.{File, InputStream, ByteArrayInputStream}
import scala.jdk.CollectionConverters._

case object Check {
  def apply(
    program: T2Program,
    backends: List[Backend],
    encParams: EncParams,
  ): CheckResult = {
    val pgm = program.content
    val inputStream = new ByteArrayInputStream(pgm.getBytes("UTF-8"))
    try {
      val (ast, symbolTable, encType) = Parse(inputStream)
      val (interpResult, executeResults) =
        getResults(ast, symbolTable, encType, backends, encParams)
      diffResults(interpResult, executeResults)
    } catch {
      case _ => ParseError(List(BackendResultPair("Parser", ParseError)))
    }
  }

  def apply(
    programs: LazyList[T2Program],
    backends: List[Backend],
    encParams: EncParams,
  ): LazyList[CheckResult] = {
    for {
      program <- programs
    } yield apply(program, backends, encParams)
  }

  def apply(
    directory: String,
    backends: List[Backend],
    encParams: EncParams,
  ): LazyList[String] = {
    val dir = new File(directory)
    if (dir.exists() && dir.isDirectory) {
      val files = Files.list(Paths.get(directory))
      val fileList = files.iterator().asScala.toList
      for {
        filePath <- fileList.to(LazyList)
      } yield {
        val fileStr = Files.readAllLines(filePath).asScala.mkString("")
        val checkResult = apply(T2Program(fileStr), backends, encParams)
        val pgmStr = "-" * 10 + " Program " + "-" * 10 + "\n" + fileStr + "\n"
        val reportStr = checkResult.toString + "\n"
        pgmStr + reportStr

      }
    } else {
      throw new Exception("Directory does not exist or is not a directory")
    }
  }

  trait CheckResult {
    val results: List[BackendResultPair]
    override def toString: String = {
      val className = this.getClass.getSimpleName.replace("$", "")
      val resultStrings = results.map {
        case BackendResultPair(backendName, executeResult) =>
          s"\n- $backendName:\n ${executeResult.toString}"
      }
      s"[$className] ${resultStrings.mkString("")}"
    }
  }
  case class Same(results: List[BackendResultPair]) extends CheckResult
  case class Diff(results: List[BackendResultPair]) extends CheckResult
  case class ParseError(results: List[BackendResultPair]) extends CheckResult

  def isDiff(
    expected: BackendResultPair,
    obtained: BackendResultPair,
  ): Boolean =
    (obtained.result, expected.result) match {
      case (Normal(obtained), Normal(expected)) =>
        try { compare(obtained, expected); false }
        catch { case _ => true }
      case _ => true
    }

  def diffResults(
    expected: BackendResultPair,
    obtained: List[BackendResultPair],
  ): CheckResult = {
    val results = expected :: obtained
    if (obtained.forall(!isDiff(expected, _))) Same(results) else Diff(results)
  }

  trait ExecuteResult
  case class Normal(res: String) extends ExecuteResult {
    override def toString: String = res
  }
  case object InterpError extends ExecuteResult
  case object PrintError extends ExecuteResult
  case class LibraryError(msg: String) extends ExecuteResult {
    override def toString: String = s"LibraryError: $msg"
  }
  case object ParseError extends ExecuteResult
  // case object TimeoutError extends ExecuteResult //TODO: development
  // case object Throw extends ExecuteResult

  case class BackendResultPair(backend: String, result: ExecuteResult)

  def getResults(
    ast: Goal,
    symbolTable: SymbolTable,
    encType: ENC_TYPE,
    backends: List[Backend],
    encParams: EncParams,
  ): (BackendResultPair, List[BackendResultPair]) = {
    val interpResult = BackendResultPair(
      "CLEAR",
      try {
        val res = Interp(ast, encParams.ringDim, encParams.plainMod)
        Normal(res.trim)
      } catch { case _ => InterpError },
    )
    val executeResults: List[BackendResultPair] =
      backends.map(backend =>
        BackendResultPair(
          backend.toString,
          execute(backend, ast, encParams, encType, symbolTable),
        ),
      )
    (interpResult, executeResults)
  }

  def execute(
    backend: Backend,
    ast: Goal,
    encParams: EncParams,
    encType: ENC_TYPE,
    symbolTable: SymbolTable,
  ): ExecuteResult =
    withBackendTempDir(
      backend,
      { workspaceDir =>
        given DirName = workspaceDir
        try {
          Print(
            ast,
            symbolTable,
            encType,
            backend,
            encParamsOpt = Some(encParams),
          )
          try {
            val res = Execute(backend)
            Normal(res.trim)
          } catch {
            // TODO?: classify exception related with parmeters?
            case ex: Exception => LibraryError(ex.getMessage)
          }
        } catch {
          case _ => PrintError
        }
      },
    )
}
