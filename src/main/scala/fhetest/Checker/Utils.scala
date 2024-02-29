package fhetest.Checker

import fhetest.Utils.*

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
  val pgm_info = Map(
    ("programId" -> JsString(i.toString)),
    ("program" -> JsString(program.content)),
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

val TEST_DIR = fhetest.TEST_DIR
val succDir = s"$TEST_DIR/succ"
val failDir = s"$TEST_DIR/fail"
val psrErrDir = s"$TEST_DIR/psr_err"
val testDirPath = Paths.get(TEST_DIR)
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
