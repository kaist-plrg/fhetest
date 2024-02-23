package fhetest.Phase

import fhetest.Utils.*
import fhetest.Checker.*
import org.twc.terminator.t2dsl_compiler.T2DSLsyntaxtree.*;
import org.twc.terminator.SymbolTable;

import java.nio.file.{Files, Paths};
import java.io.{File, InputStream, ByteArrayInputStream}
import scala.jdk.CollectionConverters._
import spray.json._
import spray.json.DefaultJsonProtocol._

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
      case _ =>
        ParserError(List(BackendResultPair("Parser", ParseError)))
    }
  }

  def apply(
    programs: LazyList[T2Program],
    backends: List[Backend],
    encParams: EncParams,
  ): LazyList[(String, CheckResult)] = {
    setTestDir()
    var i = 0
    val checkResults = for {
      program <- programs
    } yield {
      val checkResult = getAndWriteResult(i, program, backends, encParams)
      i = i + 1
      (program.content, checkResult)
    }
    checkResults
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
      setTestDir()
      var i = 0
      val checkResults = for {
        filePath <- fileList.to(LazyList)
      } yield {
        val fileStr = Files.readAllLines(filePath).asScala.mkString("")
        val program = T2Program(fileStr)
        val checkResult = getAndWriteResult(i, program, backends, encParams)
        i = i + 1
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
  ): CheckResult = {
    val results = expected :: obtained
    if (obtained.forall(!isDiff(expected, _))) Same(results) else Diff(results)
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

  def getAndWriteResult(
    i: Int,
    program: T2Program,
    backends: List[Backend],
    encParams: EncParams,
  ): CheckResult = {
    val pgm_info = Map(
      ("programId" -> JsString(i.toString)),
      ("program" -> JsString(program.content)),
    )
    val checkResult = apply(program, backends, encParams)
    checkResult match {
      case Same(res) => {
        val (expectedLst, obtainedLst) = res.partition(_.backend == "CLEAR")
        val expected_res = expectedLst.apply(0).result
        val result = pgm_info ++ Map(
          "result" -> JsString("Success"),
          "failedLibraires" -> JsString("0"),
          "failures" -> JsArray(),
          "expected" -> JsString(expected_res.toString),
        )
        val succFilename = s"$succDir/$i.json"
        dumpJson(result, succFilename)
      }
      case Diff(res) => {
        val (expectedLst, obtainedLst) = res.partition(_.backend == "CLEAR")
        val expected = expectedLst.apply(0)
        val diffResults = obtainedLst.filter(isDiff(expected, _))
        val failures = diffResults.map(r =>
          Map(
            ("library" -> r.backend),
            ("failedResult" -> r.result.toString),
          ),
        )
        val result = pgm_info ++ Map(
          "result" -> JsString("Fail"),
          "failedLibraires" -> JsString(diffResults.size.toString),
          "failures" -> failures.toJson,
          "expected" -> JsString(expected._2.toString),
        )
        val failFilename = s"$failDir/$i.json"
        dumpJson(result, failFilename)
      }
      case ParserError(_) => {
        val result = pgm_info ++ Map(
          "result" -> JsString("ParseError"),
          "failedLibraires" -> JsString("NaN"),
          "failures" -> JsArray(),
          "expected" -> JsString(""),
        )
        val psrErrFilename = s"$psrErrDir/$i.json"
        dumpJson(result, psrErrFilename)
      }
    }
    checkResult
  }
}
