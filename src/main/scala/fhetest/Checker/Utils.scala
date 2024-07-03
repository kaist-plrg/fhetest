package fhetest.Checker

import fhetest.Utils.*
import fhetest.Generate.{T2Program, ValidFilter, getValidFilterList}
import fhetest.TEST_DIR
import fhetest.Generate.LibConfig

import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._
import io.circe.parser._

import scala.io.Source

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Path, Paths, StandardCopyOption}

case class ResultReport(library: String, failedResult: String)

trait ResultInfo {
  val programId: Int
  val program: T2Program
  val resultType: String
  val SEAL: String
  val OpenFHE: String
}
case class ResultValidInfo(
  programId: Int,
  program: T2Program,
  resultType: String,
  failures: List[ResultReport],
  expected: String,
  SEAL: String,
  OpenFHE: String,
) extends ResultInfo
case class ResultInvalidInfo(
  programId: Int,
  program: T2Program,
  resultType: String,
  results: List[ResultReport],
  others: List[ResultReport],
  invalidFilters: List[String],
  SEAL: String,
  OpenFHE: String,
) extends ResultInfo

// Define en/decoders using Circe
// Scheme
implicit val schemeEncoder: Encoder[Scheme] = Encoder.instance {
  case Scheme.BFV  => Json.fromString("BFV")
  case Scheme.BGV  => Json.fromString("BGV")
  case Scheme.CKKS => Json.fromString("CKKS")
}
implicit val schemeDecoder: Decoder[Scheme] = Decoder.decodeString.emap {
  case "BFV"  => Right(Scheme.BFV)
  case "BGV"  => Right(Scheme.BGV)
  case "CKKS" => Right(Scheme.CKKS)
  case other  => Left(s"Unknown scheme: $other")
}

// SecurityLevel
implicit val securityLevelEncoder: Encoder[SecurityLevel] = Encoder.instance {
  case SecurityLevel.HEStd_128_classic => Json.fromString("HEStd_128_classic")
  case SecurityLevel.HEStd_192_classic => Json.fromString("HEStd_192_classic")
  case SecurityLevel.HEStd_256_classic => Json.fromString("HEStd_256_classic")
  case SecurityLevel.HEStd_NotSet      => Json.fromString("HEStd_NotSet")
}
implicit val securityLevelDecoder: Decoder[SecurityLevel] =
  Decoder.decodeString.emap {
    case "HEStd_128_classic" => Right(SecurityLevel.HEStd_128_classic)
    case "HEStd_192_classic" => Right(SecurityLevel.HEStd_192_classic)
    case "HEStd_256_classic" => Right(SecurityLevel.HEStd_256_classic)
    case "HEStd_NotSet"      => Right(SecurityLevel.HEStd_NotSet)
    case other               => Left(s"Unknown security level: $other")
  }

// ScalingTechnique
implicit val scalingTechniqueEncoder: Encoder[ScalingTechnique] =
  Encoder.instance {
    case ScalingTechnique.FIXEDMANUAL     => Json.fromString("FIXEDMANUAL")
    case ScalingTechnique.FIXEDAUTO       => Json.fromString("FIXEDAUTO")
    case ScalingTechnique.FLEXIBLEAUTO    => Json.fromString("FLEXIBLEAUTO")
    case ScalingTechnique.FLEXIBLEAUTOEXT => Json.fromString("FLEXIBLEAUTOEXT")
    case ScalingTechnique.NORESCALE       => Json.fromString("NORESCALE")
  }
implicit val scalingTechniqueDecoder: Decoder[ScalingTechnique] =
  Decoder.decodeString.emap {
    case "FIXEDMANUAL"     => Right(ScalingTechnique.FIXEDMANUAL)
    case "FIXEDAUTO"       => Right(ScalingTechnique.FIXEDAUTO)
    case "FLEXIBLEAUTO"    => Right(ScalingTechnique.FLEXIBLEAUTO)
    case "FLEXIBLEAUTOEXT" => Right(ScalingTechnique.FLEXIBLEAUTOEXT)
    case "NORESCALE"       => Right(ScalingTechnique.NORESCALE)
    case other             => Left(s"Unknown scaling technique: $other")
  }
implicit val encParamsEncoder: Encoder[EncParams] = deriveEncoder
implicit val encParamsDecoder: Decoder[EncParams] = deriveDecoder
implicit val libConfigEncoder: Encoder[LibConfig] = deriveEncoder
implicit val libConfigDecoder: Decoder[LibConfig] = Decoder.instance { cursor =>
  for {
    scheme <- cursor.downField("scheme").as[Scheme]
    encParams <- cursor.downField("encParams").as[EncParams]
    firstModSize <- cursor.downField("firstModSize").as[Int]
    scalingModSize <- cursor.downField("scalingModSize").as[Int]
    securityLevel <- cursor
      .downField("securityLevel")
      .as[SecurityLevel]
    scalingTechnique <- cursor
      .downField("scalingTechnique")
      .as[ScalingTechnique]
    lenOpt <- cursor.downField("lenOpt").as[Option[Int]]
    boundOpt <- scheme match {
      case Scheme.BGV | Scheme.BFV =>
        cursor
          .downField("boundOpt")
          .as[Option[Int]]
          .map(_.map(_.asInstanceOf[Int | Double]))
      case Scheme.CKKS =>
        cursor
          .downField("boundOpt")
          .as[Option[Double]]
          .map(_.map(_.asInstanceOf[Int | Double]))
    }
    rotateBoundOpt <- cursor.downField("rotateBoundOpt").as[Option[Int]]
  } yield LibConfig(
    scheme,
    encParams,
    firstModSize,
    scalingModSize,
    securityLevel,
    scalingTechnique,
    lenOpt,
    boundOpt,
    rotateBoundOpt,
  )
}
implicit val t2ProgramEncoder: Encoder[T2Program] = deriveEncoder
implicit val t2ProgramDecoder: Decoder[T2Program] = deriveDecoder
implicit val resultReportEncoder: Encoder[ResultReport] = deriveEncoder
implicit val resultReportDecoder: Decoder[ResultReport] = deriveDecoder

implicit val resultValidInfoEncoder: Encoder[ResultValidInfo] =
  Encoder.forProduct7(
    "programId",
    "program",
    "resultType",
    "failures",
    "expected",
    "SEAL",
    "OpenFHE",
  )(ri =>
    (
      ri.programId,
      ri.program,
      ri.resultType,
      ri.failures,
      ri.expected,
      ri.SEAL,
      ri.OpenFHE,
    ),
  )
implicit val resultValidInfoDecoder: Decoder[ResultValidInfo] =
  Decoder.forProduct7(
    "programId",
    "program",
    "resultType",
    "failures",
    "expected",
    "SEAL",
    "OpenFHE",
  )(ResultValidInfo.apply)

implicit val resultInvalidInfoEncoder: Encoder[ResultInvalidInfo] =
  Encoder.forProduct8(
    "programId",
    "program",
    "resultType",
    "results",
    "others",
    "invalidFilters",
    "SEAL",
    "OpenFHE",
  )(ri =>
    (
      ri.programId,
      ri.program,
      ri.resultType,
      ri.results,
      ri.others,
      ri.invalidFilters,
      ri.SEAL,
      ri.OpenFHE,
    ),
  )
implicit val resultInvalidInfoDecoder: Decoder[ResultInvalidInfo] =
  Decoder.forProduct8(
    "programId",
    "program",
    "resultType",
    "results",
    "others",
    "invalidFilters",
    "SEAL",
    "OpenFHE",
  )(ResultInvalidInfo.apply)

implicit val encodeIntOrDouble: Encoder[Int | Double] = Encoder.instance {
  case i: Int    => Json.fromInt(i)
  case d: Double => Json.fromDoubleOrNull(d)
}

implicit val mapEncoder: Encoder[Map[List[Int], Int]] =
  new Encoder[Map[List[Int], Int]] {
    override def apply(a: Map[List[Int], Int]): Json = {
      val encodedPairs = a.map {
        case (key, value) =>
          key.toString() -> value.asJson
      }
      Json.obj(encodedPairs.toSeq: _*)
    }
  }

object DumpUtil {
  def dumpFile(data: String, filename: String): Unit = {
    val writer = new PrintWriter(filename)
    try writer.write(data)
    finally writer.close()
  }

  def readFile(filePath: String): String =
    Source.fromFile(filePath).getLines.mkString

  def dumpResult(
    program: T2Program,
    i: Int,
    res: CheckResult,
    sealVersion: String,
    openfheVersion: String,
  ): Unit = {
    val resultString = res match {
      case Same(_)        => "Success"
      case Diff(_, _)     => "Fail"
      case ParserError(_) => "ParseError"
    }
    val failures = res match {
      case Diff(_, fails) =>
        fails.map(fail => ResultReport(fail.backend, fail.result.toString()))
      case _ => List.empty[ResultReport]
    }
    val expected = res match {
      case Same(results) =>
        results.headOption.map(_.result.toString()).getOrElse("")
      case Diff(results, _) =>
        results
          .partition(_.backend == "CLEAR")
          ._1
          .headOption
          .map(_.result.toString())
          .getOrElse("")
    }
    val resultValidInfo = ResultValidInfo(
      i,
      program,
      resultString,
      failures,
      expected,
      sealVersion,
      openfheVersion,
    )
    val filename = res match
      case Same(_)        => s"$succDir/$i.json"
      case Diff(_, fails) => s"$failDir/$i.json"
      case ParserError(_) => s"$psrErrDir/$i.json"
    dumpFile(resultValidInfo.asJson.spaces2, filename)
  }

  def dumpInvalidResult(
    program: T2Program,
    i: Int,
    resLst: List[CheckResult],
    sealVersion: String,
    openfheVersion: String,
    invalidFilters: List[String] = List[String](),
  ): Unit = resLst match {
    case head :: next => {
      val (resultString, results, interested, filename) = head match {
        case InvalidNormalResults(results, interested) =>
          (
            "InvalidNormal",
            results,
            interested,
            s"$invalidNormalDir/$i.json",
          )
        case InvalidExpectedExceptions(results, interested) =>
          (
            "InvalidExpectedException",
            results,
            interested,
            s"$invalidExpectedExceptionDir/$i.json",
          )
        case InvalidUnexpectedExceptions(results, interested) =>
          (
            "InvalidUnexpectedException",
            results,
            interested,
            s"$invalidUnexpectedExceptionDir/$i.json",
          )
        case InvalidErrors(results, interested) =>
          ("InvalidError", results, interested, s"$invalidErrorDir/$i.json")
      }
      val interestedResults = interested.map { backendResultPair =>
        ResultReport(
          backendResultPair.backend,
          backendResultPair.result.toString(),
        )
      }
      val interestedLibrariesNames = interested.map(_.backend)
      val others = results.foldLeft(List[ResultReport]()) {
        (acc, backendResultPair) =>
          if (interestedLibrariesNames.contains(backendResultPair.backend))
            acc
          else {
            acc :+ ResultReport(
              backendResultPair.backend,
              backendResultPair.result.toString(),
            )
          }
      }
      val resultInvalidInfo = ResultInvalidInfo(
        i,
        program,
        resultString,
        interestedResults,
        others,
        invalidFilters,
        sealVersion,
        openfheVersion,
      )
      dumpFile(resultInvalidInfo.asJson.spaces2, filename)
      dumpInvalidResult(
        program,
        i,
        next,
        sealVersion,
        openfheVersion,
        invalidFilters,
      )
    }
    case Nil => ()
  }

  def dumpCount(dir: String, countMap: Map[List[Int], Int]): Unit = {
    val jsonString =
      countMap.asJson.spaces2
    val outputFileName = s"$dir/count.json"
    dumpFile(jsonString, outputFileName)
  }

  def readResult(filePath: String): ResultInfo = {
    val fileContents = readFile(filePath)
    val resultInfo =
      if (filePath contains "invalid") decode[ResultInvalidInfo](fileContents)
      else decode[ResultValidInfo](fileContents)
    resultInfo match {
      case Right(info) => info
      case Left(error) => throw new Exception(s"Error: $error")
    }
  }
}

val testDir = s"$TEST_DIR-$formattedDateTime"
val testInvalidDir = s"$TEST_DIR-invalid-$formattedDateTime"
val succDir = s"$testDir/succ"
val failDir = s"$testDir/fail"
val psrErrDir = s"$testDir/psr_err"
val invalidNormalDir = s"$testInvalidDir/normal"
val invalidExceptionDir = s"$testInvalidDir/exception"
val invalidExpectedExceptionDir = s"$invalidExceptionDir/expected"
val invalidUnexpectedExceptionDir = s"$invalidExceptionDir/unexpected"
val invalidErrorDir = s"$testInvalidDir/error"

val testDirPath = Paths.get(testDir)
val testInvalidDirPath = Paths.get(testInvalidDir)
val succDirPath = Paths.get(succDir)
val failDirPath = Paths.get(failDir)
val psrErrDirPath = Paths.get(psrErrDir)
val invalidNormalDirPath = Paths.get(invalidNormalDir)
val invalidExceptionDirPath = Paths.get(invalidExceptionDir)
val invalidExpectedExceptionDirPath = Paths.get(invalidExpectedExceptionDir)
val invalidUnexpectedExceptionDirPath = Paths.get(invalidUnexpectedExceptionDir)
val invalidErrorDirPath = Paths.get(invalidErrorDir)

def setValidTestDir(): Unit = setTestDirs(
  List(testDirPath, succDirPath, failDirPath, psrErrDirPath),
)
def setInvalidTestDir(): Unit = setTestDirs(
  List(
    testInvalidDirPath,
    invalidNormalDirPath,
    invalidExceptionDirPath,
    invalidExpectedExceptionDirPath,
    invalidUnexpectedExceptionDirPath,
    invalidErrorDirPath,
  ),
)

def setTestDirs(paths: List[Path]): Unit =
  paths.foreach(path =>
    if (!Files.exists(path)) {
      Files.createDirectories(path)
    },
  )
