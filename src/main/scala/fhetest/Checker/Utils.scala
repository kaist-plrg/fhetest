package fhetest.Checker

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import spray.json._

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
