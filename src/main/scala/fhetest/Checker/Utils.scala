package fhetest.Checker

import fhetest.Utils.*
import fhetest.Generate.T2Program
import fhetest.TEST_DIR
import fhetest.LibConfig

import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Path, Paths, StandardCopyOption}

case class Failure(library: String, failedResult: String)
case class ResultInfo(
  programId: Int,
  program: T2Program,
  result: String,
  failures: List[Failure],
  expected: String,
  SEAL: String,
  OpenFHE: String,
)

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
implicit val libConfigEncoder: Encoder[LibConfig] = deriveEncoder
implicit val t2ProgramEncoder: Encoder[T2Program] = deriveEncoder
implicit val failureEncoder: Encoder[Failure] = deriveEncoder
implicit val resultInfoEncoder: Encoder[ResultInfo] = Encoder.forProduct7(
  "programId",
  "program",
  "result",
  "failures",
  "expected",
  "SEAL",
  "OpenFHE",
)(ri =>
  (
    ri.programId,
    ri.program,
    ri.result,
    ri.failures,
    ri.expected,
    ri.SEAL,
    ri.OpenFHE,
  ),
)
implicit val encodeIntOrDouble: Encoder[Int | Double] = Encoder.instance {
  case i: Int    => Json.fromInt(i)
  case d: Double => Json.fromDoubleOrNull(d)
}

object DumpUtil {
  // 파일에 문자열 데이터 쓰기 함수
  def dumpFile(data: String, filename: String): Unit = {
    val writer = new PrintWriter(filename)
    try writer.write(data)
    finally writer.close()
  }

  // dumpResult 함수 구현
  def dumpResult(
    program: T2Program, // 가정: 이미 정의되어 있음
    i: Int,
    res: CheckResult,
    sealVersion: String,
    openfheVersion: String,
  ): Unit = {
    val resultString = res match {
      case Same(_)        => "Success"
      case Diff(_, fails) => "Fail"
      case ParserError(_) => "ParseError"
    }

    val failures = res match {
      case Diff(_, fails) =>
        fails.map(fail => Failure(fail.backend, fail.result.toString()))
      case _ => List.empty[Failure]
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

    val resultInfo = ResultInfo(
      i,
      program,
      resultString,
      failures,
      expected,
      sealVersion,
      openfheVersion,
    )

    val filename = res match
      case Same(_)        => s"$succDir$i.json"
      case Diff(_, fails) => s"$failDir/$i.json"
      case ParserError(_) => s"$psrErrDir/$i.json"
    dumpFile(resultInfo.asJson.spaces2, filename)
  }
}

val testDir = s"$TEST_DIR-$formattedDateTime"
val succDir = s"$testDir/succ"
val failDir = s"$testDir/fail"
val psrErrDir = s"$testDir/psr_err"
val testDirPath = Paths.get(testDir)
val succDirPath = Paths.get(succDir)
val failDirPath = Paths.get(failDir)
val psrErrDirPath = Paths.get(psrErrDir)

def setTestDir(): Unit = {
  val pathLst = List(testDirPath, succDirPath, failDirPath, psrErrDirPath)
  pathLst.foreach(path =>
    if (!Files.exists(path)) {
      Files.createDirectories(path)
    },
  )
}
