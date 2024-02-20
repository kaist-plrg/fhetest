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
