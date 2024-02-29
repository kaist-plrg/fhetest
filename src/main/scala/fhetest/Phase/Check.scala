package fhetest.Phase

import fhetest.Utils.*
import fhetest.Checker.*
import org.twc.terminator.t2dsl_compiler.T2DSLsyntaxtree.*;
import org.twc.terminator.SymbolTable;

import java.nio.file.{Files, Paths};
import java.io.{File, InputStream, ByteArrayInputStream}
import scala.jdk.CollectionConverters._
import spray.json._

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
      diffResults(interpResult, executeResults, encType, encParams.plainMod)
    } catch {
      case _ =>
        ParserError(List(BackendResultPair("Parser", ParseError)))
    }
  }

  def apply(
    programs: LazyList[T2Program],
    backends: List[Backend],
    encParams: EncParams,
    toJson: Boolean,
    sealVersion: String,
    openfheVersion: String,
  ): LazyList[(T2Program, CheckResult)] = {
    setTestDir()
    val checkResults = for {
      (program, i) <- programs.zipWithIndex
    } yield {
      val checkResult = apply(program, backends, encParams)
      if (toJson)
        dumpResult(program, i, checkResult, sealVersion, openfheVersion)
      (program, checkResult)
    }
    checkResults
  }

  // TODO: Do we need this function?
  def apply(
    directory: String,
    backends: List[Backend],
    encParams: EncParams,
    toJson: Boolean,
    sealVersion: String,
    openfheVersion: String,
  ): LazyList[String] = {
    val dir = new File(directory)
    if (dir.exists() && dir.isDirectory) {
      val files = Files.list(Paths.get(directory))
      val fileList = files.iterator().asScala.toList
      setTestDir()
      val checkResults = for {
        (filePath, i) <- fileList.to(LazyList).zipWithIndex
      } yield {
        val fileStr = Files.readAllLines(filePath).asScala.mkString("")
        val program = T2Program(fileStr)
        val checkResult = apply(program, backends, encParams)
        if (toJson)
          dumpResult(program, i, checkResult, sealVersion, openfheVersion)
        val pgmStr = "-" * 10 + " Program " + "-" * 10 + "\n" + fileStr + "\n"
        val reportStr = checkResult.toString + "\n"
        pgmStr + reportStr

      }
      checkResults
    } else {
      throw new Exception("Directory does not exist or is not a directory")
    }
  }

  def diffResults(
    expected: BackendResultPair,
    obtained: List[BackendResultPair],
    encType: ENC_TYPE,
    plainMod: Int,
  ): CheckResult = {
    val results = expected :: obtained
    val is_mod = (encType == ENC_TYPE.ENC_INT)
    val fails = obtained.filter(isDiff(expected, _, is_mod, plainMod))
    if (fails.isEmpty) Same(results)
    else Diff(results, fails)
  }

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
