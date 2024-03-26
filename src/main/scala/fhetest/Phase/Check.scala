package fhetest.Phase

import fhetest.Checker.*
import fhetest.Generate.T2Program
import fhetest.Generate.LibConfig
import fhetest.Generate.Utils.InvalidFilterIdx
import fhetest.Utils.*
import fhetest.Checker.DumpUtil

import org.twc.terminator.t2dsl_compiler.T2DSLsyntaxtree.*;
import org.twc.terminator.SymbolTable;

import java.nio.file.{Files, Paths};
import java.io.{File, InputStream, ByteArrayInputStream}
import scala.jdk.CollectionConverters._

import scala.util.{Try, Success, Failure}

case object Check {
  def apply(
    program: T2Program,
    backends: List[Backend],
    encParamsOpt: Option[EncParams],
    timeLimit: Option[Int],
  ): CheckResult = {
    val encParams = encParamsOpt match {
      case Some(encParams) => encParams
      case None            => program.libConfig.encParams
    }
    val result = for {
      parsed <- parse(program)
    } yield {
      val encType = parsed._3
      val interpResult: ExecuteResult = interp(parsed, encParams) match {
        case Success(interpValue) => interpValue
        case Failure(_)           => InterpError
      }
      val interpResPair = BackendResultPair("CLEAR", interpResult)
      val executeResPairs = backends.map(backend =>
        BackendResultPair(
          backend.toString,
          execute(backend, encParams, parsed, program.libConfig, timeLimit),
        ),
      )
      diffResults(interpResPair, executeResPairs, encType, encParams.plainMod)
    }
    result.getOrElse(
      ParserError(List(BackendResultPair("Parser", ParseError))),
    )
  }

  def apply(
    directory: String,
    backends: List[Backend],
    encParamsOpt: Option[EncParams],
    toJson: Boolean,
    sealVersion: String,
    openfheVersion: String,
    timeLimit: Option[Int],
  ): LazyList[String] = {
    val dir = new File(directory)
    if (dir.exists() && dir.isDirectory) {
      val files = Files.list(Paths.get(directory))
      val fileList = files.iterator().asScala.toList
      setValidTestDir()
      val checkResults = for {
        (filePath, i) <- fileList.to(LazyList).zipWithIndex
      } yield {
        val fileStr = Files.readAllLines(filePath).asScala.mkString("")
        val libConfig = LibConfig() // default libConfig for dir testing
        val program = T2Program(fileStr, libConfig, List[InvalidFilterIdx]())
        val checkResult = apply(program, backends, encParamsOpt, timeLimit)
        if (toJson)
          DumpUtil.dumpResult(
            program,
            i,
            checkResult,
            sealVersion,
            openfheVersion,
          )
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

  def apply(
    programs: LazyList[T2Program],
    backends: List[Backend],
    encParamsOpt: Option[EncParams],
    toJson: Boolean,
    sealVersion: String,
    openfheVersion: String,
    validFilter: Boolean,
    debug: Boolean,
    timeLimit: Option[Int],
  ): LazyList[(T2Program, CheckResult)] =
    if (validFilter) {
      setValidTestDir()
      val checkResults: LazyList[Option[(T2Program, CheckResult)]] = for {
        (program, i) <- programs.zipWithIndex
        encParams = encParamsOpt.getOrElse(program.libConfig.encParams)
        parsed <- parse(program).toOption
      } yield {
        val interpResult: ExecuteResult = interp(parsed, encParams) match {
          case Success(interpValue) => interpValue
          case Failure(_)           => InterpError
        }
        val overflowBound =
          if program.libConfig.scheme == Scheme.CKKS then
            math.pow(2, program.libConfig.firstModSize)
          else program.libConfig.encParams.plainMod.toDouble
        if notOverflow(interpResult, overflowBound) then {
          val encType = parsed._3
          val interpResPair = BackendResultPair("CLEAR", interpResult)
          val executeResPairs = backends.map(backend =>
            BackendResultPair(
              backend.toString,
              execute(backend, encParams, parsed, program.libConfig, timeLimit),
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
            DumpUtil.dumpResult(
              program,
              i,
              checkResult,
              sealVersion,
              openfheVersion,
            )
          if (debug) {
            println(s"Program $i:")
          }
          Some(program, checkResult)
        } else {
          if (debug) {
            println(
              s"Program $i is skipped due to HE overflow check: $overflowBound",
            )
          }
          None
        }
      }
      checkResults.flatten
    } else {
      setInvalidTestDir()
      val checkResults: LazyList[Option[(T2Program, CheckResult)]] = for {
        (program, i) <- programs.zipWithIndex
        encParams = encParamsOpt.getOrElse(program.libConfig.encParams)
        parsed <- parse(program).toOption
      } yield {
        val encType = parsed._3
        val executeResPairs = backends.map(backend =>
          BackendResultPair(
            backend.toString,
            execute(backend, encParams, parsed, program.libConfig, timeLimit),
          ),
        )
        val checkResult: CheckResult = InvalidResults(executeResPairs)
        if (toJson)
          DumpUtil.dumpResult(
            program,
            i,
            checkResult,
            sealVersion,
            openfheVersion,
          )
        if (debug) {
          println(s"Program $i:")
        }
        Some(program, checkResult)
      }
      checkResults.flatten
    }

  // TODO: Need to be revised
  def notOverflow(interpResult: ExecuteResult, overflowBound: Double): Boolean =
    interpResult match {
      case Normal(res) =>
        res.split("\n").forall { line =>
          val max = line.split(" ").map(_.toDouble).max
          max < overflowBound
        }
      case _ => true
    }

  // Get CheckResult from results of valid programs
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
    timeLimit: Option[Int],
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
            val res = Execute(backend, timeLimit)
            Normal(res.trim)
          } catch {
            case _: java.util.concurrent.TimeoutException => TimeoutError
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
