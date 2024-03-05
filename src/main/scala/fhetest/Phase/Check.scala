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
    encParamsOpt: Option[EncParams],
  ): CheckResult = {
    val encParams = encParamsOpt match {
      case Some(encParams) => encParams
      case None            => program.libConfig.encParams
    }
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
          execute(backend, encParams, parsed, program.libConfig),
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
    encParamsOpt: Option[EncParams],
    toJson: Boolean,
    sealVersion: String,
    openfheVersion: String,
    validCheck: Boolean,
    debug: Boolean,
  ): LazyList[(T2Program, CheckResult)] = {
    setTestDir()
    val checkResults: LazyList[Option[(T2Program, CheckResult)]] = for {
      (program, i) <- programs.zipWithIndex
      encParams = encParamsOpt.getOrElse(program.libConfig.encParams)
      parsed <- parse(program).toOption
    } yield {
      val result: ExecuteResult = interp(parsed, encParams) match {
        case Success(interpValue) => interpValue
        case Failure(_)           => InterpError
      }
      val overflowBound = program.libConfig.firstModSize
      if !validCheck || notOverflow(result, overflowBound) then {
        val encType = parsed._3
        val interpResPair = BackendResultPair("CLEAR", result)
        val executeResPairs = backends.map(backend =>
          BackendResultPair(
            backend.toString,
            execute(backend, encParams, parsed, program.libConfig),
          ),
        )
        val checkResult =
          diffResults(
            interpResPair,
            executeResPairs,
            encType,
            encParams.plainMod,
          )
        if (toJson)
          dumpResult(program, i, checkResult, sealVersion, openfheVersion)
        Some(program, checkResult)
      } else {
        None
      }
    }
    checkResults.flatten
  }

  // TODO: Need to be revised
  def notOverflow(interpResult: ExecuteResult, overflowBound: Int): Boolean =
    val limit = math.pow(2, overflowBound)
    interpResult match {
      case Normal(res) =>
        res.split("\n").forall { line =>
          val max = line.split(" ").map(_.toDouble).max
          max < limit
        }
      case _ => true
    }

  def apply(
    directory: String,
    backends: List[Backend],
    encParamsOpt: Option[EncParams],
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
        val libConfig = LibConfig() // default libConfig for dir testing
        val program = T2Program(fileStr, libConfig)
        val checkResult = apply(program, backends, encParamsOpt)
        if (toJson)
          dumpResult(program, i, checkResult, sealVersion, openfheVersion)
        val pgmStr = "-" * 10 + " Program " + "-" * 10 + "\n" + fileStr + "\n"
        val libConfigStr =
          "-" * 10 + " LibConfig " + "-" * 10 + "\n" + libConfig
            .stringify() + "\n"
        val reportStr =
          "-" * 10 + " CheckResult " + "-" * 10 + "\n" + checkResult.toString + "\n"
        pgmStr + libConfigStr + reportStr
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
    libConfig: LibConfig,
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
            libConfigOpt = Some(libConfig),
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
