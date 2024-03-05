package fhetest.Checker

import fhetest.Utils.*
import fhetest.Generate.T2Program

import fhetest.TEST_DIR

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import spray.json._
import spray.json.DefaultJsonProtocol._

// file writer
def getPrintWriter(filename: String): PrintWriter =
  new PrintWriter(new File(filename))

// dump given data to a file
def dumpFile(data: Any, filename: String): Unit = {
  val nf = getPrintWriter(filename)
  nf.print(data)
  nf.close()
}

// dump given data as JSON
def dumpJson[T](data: T, filename: String)(implicit
  writer: JsonWriter[T],
): Unit =
  dumpFile(data.toJson.prettyPrint, filename)

// FIXME: LibConfig
def dumpResult(
  program: T2Program,
  i: Int,
  res: CheckResult,
  sealVersion: String,
  openfheVersion: String,
): Unit = {
  val backend_info = Map(
    ("SEAL" -> JsString(sealVersion)),
    ("OpenFHE" -> JsString(openfheVersion)),
  )
  val libConfig = program.libConfig
  val encParams = libConfig.encParams
  val encParams_info = JsObject(
    ("ringDim" -> JsString(encParams.ringDim.toString)),
    ("multDepth" -> JsString(encParams.mulDepth.toString)),
    ("plainMod" -> JsString(encParams.plainMod.toString)),
  )
  val libConfig_info = JsObject(
    ("scheme" -> JsString(libConfig.scheme.toString)),
    ("encParams" -> encParams_info.toJson),
    ("firstModSize" -> JsString(libConfig.firstModSize.toString)),
    ("scalingModSize" -> JsString(libConfig.scalingModSize.toString)),
    ("securityLevel" -> JsString(libConfig.securityLevel.toString)),
    ("scalingTechnique" -> JsString(libConfig.scalingTechnique.toString)),
    ("lenOpt" -> JsString(libConfig.lenOpt.getOrElse(0).toString)),
    ("boundOpt" -> JsString(libConfig.boundOpt.getOrElse(0).toString)),
  )
  val pgm_info = Map(
    ("programId" -> JsString(i.toString)),
    ("program" -> JsString(program.content)),
    ("libConfig" -> libConfig_info),
  )
  val info = pgm_info ++ backend_info
  res match {
    case Same(res) => {
      val (expectedLst, obtainedLst) = res.partition(_.backend == "CLEAR")
      val expected_res = expectedLst.apply(0).result
      val result = info ++ Map(
        "result" -> JsString("Success"),
        "failedLibraires" -> JsString("0"),
        "failures" -> JsArray(),
        "expected" -> JsString(expected_res.toString),
      )
      val succFilename = s"$succDir/$i.json"
      dumpJson(result, succFilename)
    }
    case Diff(res, fails) => {
      val (expectedLst, obtainedLst) = res.partition(_.backend == "CLEAR")
      val expected = expectedLst.apply(0)
      // val diffResults = obtainedLst.filter(isDiff(expected, _, ))
      val failures = fails.map(r =>
        Map(
          ("library" -> r.backend),
          ("failedResult" -> r.result.toString),
        ),
      )
      val result = info ++ Map(
        "result" -> JsString("Fail"),
        "failedLibraires" -> JsString(fails.size.toString),
        "failures" -> failures.toJson,
        "expected" -> JsString(expected._2.toString),
      )
      val failFilename = s"$failDir/$i.json"
      dumpJson(result, failFilename)
    }
    case ParserError(_) => {
      val result = info ++ Map(
        "result" -> JsString("ParseError"),
        "failedLibraires" -> JsString("NaN"),
        "failures" -> JsArray(),
        "expected" -> JsString(""),
      )
      val psrErrFilename = s"$psrErrDir/$i.json"
      dumpJson(result, psrErrFilename)
    }
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
