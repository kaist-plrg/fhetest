package fhetest.Phase

import fhetest.Checker.*
import fhetest.Generate.T2Program
import fhetest.LibConfig
import fhetest.Utils.*

import org.twc.terminator.t2dsl_compiler.T2DSLsyntaxtree.*;
import org.twc.terminator.SymbolTable;

import java.nio.file.{Files, Paths};
import java.io.{File, InputStream, ByteArrayInputStream}
import scala.jdk.CollectionConverters._
import spray.json._

import scala.util.{Try, Success, Failure}

case object Check {
  def apply(
    program: T2Program,
    backends: List[Backend],
    encParams: EncParams,
  ): CheckResult = {
    val result = for {
      parsed <- parse(program)
    } yield {
      val encType = parsed._3
      val interpreted = interp(parsed, encParams)
      val interpResult = interpreted match {
        case Success(value) => value
        case Failure(e) =>
          e match {
            case _ => InterpError
          }
      }
      val interpResPair = BackendResultPair("CLEAR", interpResult)

      val executeResPairs = backends.map(backend =>
        BackendResultPair(
          backend.toString,
          execute(backend, encParams, parsed),
        ),
      )
      diffResults(interpResPair, executeResPairs, encType, encParams.plainMod)
    }
    result.getOrElse(
      ParserError(List(BackendResultPair("Parser", ParseError))),
    )
  }

  def apply(
    programs: LazyList[T2Program],
    backends: List[Backend],
    encParams: EncParams,
    toJson: Boolean,
    sealVersion: String,
    openfheVersion: String,
    validCheck: Boolean,
  ): LazyList[(T2Program, CheckResult)] = {
    setTestDir()
    val checkResults = for {
      (program, i) <- programs.zipWithIndex
      parsed <- parse(program).toOption
      interpResult <- interp(parsed, encParams).toOption
      overflowBound = program.libConfig.firstModSize
      if !validCheck || notOverflow(interpResult, overflowBound)
    } yield {
      val encType = parsed._3
      val interpResPair = BackendResultPair("CLEAR", interpResult)
      val executeResPairs = backends.map(backend =>
        BackendResultPair(
          backend.toString,
          execute(backend, encParams, parsed),
        ),
      )
      val checkResult =
        diffResults(interpResPair, executeResPairs, encType, encParams.plainMod)
      if (toJson)
        dumpResult(program, i, checkResult, sealVersion, openfheVersion)
      (program, checkResult)
    }
    checkResults
  }

  // TODO: Need to be revised
  def notOverflow(interpResult: Normal, overflowBound: Int): Boolean =
    val limit = math.pow(2, overflowBound)
    val lines = interpResult.res.split("\n")
    lines.forall { line =>
      val max = line.split(" ").map(_.toDouble).max
      max < limit
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
        val program = T2Program(fileStr, LibConfig())
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

  // parse wrapper
  def parse(program: T2Program) = {
    val pgm = program.content
    val inputStream = new ByteArrayInputStream(pgm.getBytes("UTF-8"))
    Try(Parse(inputStream))
  }

  // interp wrapper
  def interp(
    parsed: (Goal, SymbolTable, ENC_TYPE),
    encParams: EncParams,
  ) = {
    val (ast, symbolTable, encType) = parsed
    Try(Interp(ast, encParams.ringDim, encParams.plainMod).trim).map(Normal(_))
  }

  // execute wrapper
  def execute(
    backend: Backend,
    encParams: EncParams,
    parsed: (Goal, SymbolTable, ENC_TYPE),
  ): ExecuteResult = {
    val (ast, symbolTable, encType) = parsed
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

}
