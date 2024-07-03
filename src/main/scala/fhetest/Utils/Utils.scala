package fhetest.Utils

import org.twc.terminator.Main.ENC_TYPE as T2ENC_TYPE

import sys.process.*

import java.io.File
import java.nio.file.{
  Files,
  Path,
  Paths,
  StandardCopyOption,
  StandardOpenOption,
}
import java.util.Comparator
import java.util.concurrent.atomic.AtomicInteger
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import scala.collection.mutable.StringBuilder
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration.*
import scala.io.Source
import scala.util.Try
import scala.util.Using

import fhetest.Generate.Strategy

enum Backend(val name: String):
  case SEAL extends Backend("SEAL")
  case OpenFHE extends Backend("OpenFHE")

enum ENC_TYPE:
  case ENC_INT, ENC_DOUBLE

enum Scheme:
  case BFV, BGV, CKKS

extension (s: Scheme) {
  def toEncType: ENC_TYPE = s match
    case Scheme.CKKS => ENC_TYPE.ENC_DOUBLE
    case _           => ENC_TYPE.ENC_INT
}

enum SecurityLevel:
  case HEStd_128_classic, HEStd_192_classic, HEStd_256_classic, HEStd_NotSet

enum ScalingTechnique:
  case NORESCALE, FIXEDMANUAL, FIXEDAUTO, FLEXIBLEAUTO, FLEXIBLEAUTOEXT

type DirName = String

extension [T](opt: Option[T])
  def getOrElseThrow(message: => String): T = opt match {
    case Some(value) => value
    case None        => throw new Exception(message)
  }

case class EncParams(ringDim: Int, mulDepth: Int, plainMod: Int)

def translateT2EncType(enc_type: T2ENC_TYPE): ENC_TYPE = enc_type match
  case T2ENC_TYPE.NONE       => ENC_TYPE.ENC_INT
  case T2ENC_TYPE.ENC_INT    => ENC_TYPE.ENC_INT
  case T2ENC_TYPE.ENC_DOUBLE => ENC_TYPE.ENC_DOUBLE

def parsePrefixedArg(input: String): Option[String] = {
  Option(input).collect { case s if s.startsWith("--") => s.stripPrefix("--") }
}

def parseBackend(backendString: String): Option[Backend] =
  backendString.toLowerCase() match
    case seal: String if seal.toLowerCase() == "seal" => Some(Backend.SEAL)
    case openfhe: String if openfhe.toLowerCase() == "openfhe" =>
      Some(Backend.OpenFHE)
    case _ => None

def parseEncType(encTypeString: String): Option[ENC_TYPE] =
  encTypeString.toLowerCase() match
    case "int"    => Some(ENC_TYPE.ENC_INT)
    case "double" => Some(ENC_TYPE.ENC_DOUBLE)
    case _        => None

def parseStrategy(sString: String): Option[Strategy] =
  sString.toLowerCase() match
    case "exhaust" => Some(Strategy.Exhaustive)
    case "random"  => Some(Strategy.Random)
    case _         => None

def getWorkspaceDir(backend: Backend): String = backend match
  case Backend.SEAL    => fhetest.SEAL_DIR
  case Backend.OpenFHE => fhetest.OPENFHE_DIR

def withBackendTempDir[Result](
  backend: Backend,
  action: (DirName) => Result,
): Result = {
  val baseWorkspaceDirName = getWorkspaceDir(backend)
  val baseWorkspaceDir = Paths.get(baseWorkspaceDirName)

  val prefix = "fhetest"
  val tempDir: Path = Files.createTempDirectory(prefix)
  val tempDirName = tempDir.toString

  def copyWorkspace(): Unit =
    Files.walk(baseWorkspaceDir).parallel().forEach { basePath =>
      val relativePath = baseWorkspaceDir.relativize(basePath).toString()
      // 원하는 파일 및 폴더인지 확인
      val isTargetFileOrDir = relativePath match {
        case "CMakeLists.txt"                            => true
        case path if path.startsWith("functional_units") => true
        case _                                           => false
      }
      if (isTargetFileOrDir) {
        val targetPath = tempDir.resolve(relativePath)
        if (Files.isDirectory(basePath)) {
          if (!Files.exists(targetPath)) Files.createDirectory(targetPath)
        } else {
          Files.copy(basePath, targetPath, StandardCopyOption.REPLACE_EXISTING)
        }
      }
    }

  def deleteTemp(): Try[Unit] =
    Try(
      Files
        .walk(tempDir)
        .parallel()
        .sorted(Comparator.reverseOrder())
        .forEach(Files.delete),
    )

  try {
    copyWorkspace()
    action(tempDirName)
  } finally {
    deleteTemp()
  }
}

def isNumber(s: String): Boolean = {
  try {
    s.toDouble
    true
  } catch {
    case _: NumberFormatException => false
  }
}

def compare(
  obtained: String,
  expected: String,
  is_mod: Boolean = false,
  plainMod: Int = 0,
  backendName: String = "",
): Unit = {
  val obtainedLines = obtained.split("\n")
  val resultLines = expected.split("\n")
  obtainedLines
    .zip(resultLines)
    .foreach {
      case (obtainedLine, resultLine) =>
        val obtainedNumbers =
          obtainedLine.split(" ").map(_.toDouble)
        val resultNumbers = resultLine.split(" ").map(_.toDouble)
        obtainedNumbers
          .zip(resultNumbers)
          .foreach {
            case (obtained, result) =>
              val result_mod = if (is_mod) {
                backendName match {
                  case "SEAL" =>
                    if (result < 0) { result + plainMod }
                    else { result }
                  case "OpenFHE" => {
                    val half = (plainMod - 1) / 2
                    if (result > half) { result - plainMod }
                    else if (result < -half) { result + plainMod }
                    else { result }
                  }
                  case _ => result
                }
              } else { result }
              if (result_mod == 0.0) {
                assert(
                  Math.abs(obtained - result_mod) < 0.001,
                  s"$obtained and $result_mod are not close",
                )
              } else {
                // calculate relavant error
                val relError = Math.abs((obtained - result_mod) / obtained)
                assert(
                  relError < 0.001,
                  s"$obtained and $result_mod are not close",
                )
              }
          }
    }
}

// progress bar
case class ProgressBar[T](
  msg: String,
  iterable: Iterable[T],
  getName: (T, Int) => String = (_: T, idx) => s"$idx element",
  timeLimit: Option[Int] = None, // seconds
) extends Iterable[T] {

  // bar length
  val BAR_LEN = 40

  // update interval
  val term = 1000 // 1 second

  // iterators
  final def iterator: Iterator[T] = iterable.iterator

  // size
  override val size: Int = iterable.size

  // foreach function
  override def foreach[U](f: T => U): Unit = {
    val gcount = AtomicInteger(size)
    val start = System.currentTimeMillis

    def show: Future[Unit] = Future {
      val count = gcount.get
      val percent = count.toDouble / size * 100
      val len = count * BAR_LEN / size
      val bars = (BAR * len) + (" " * (BAR_LEN - len))
      val msg =
        f"[$bars] $percent%2.2f%% ($count%,d/$size%,d)"
      print("\r" + msg)
      if (count != size) { Thread.sleep(term); show }
      else println
    }

    val tests = for ((x, idx) <- iterable.zipWithIndex) yield () =>
      val name = getName(x, size + idx)
      f(x)
      gcount.incrementAndGet

    tests.foreach(_.apply)
  }

  // progress bar character
  val BAR = "#"
}
object ProgressBar {
  def defaultGetName[T](x: T, idx: Int): String =
    s"${idx} element"
}

def updateCMakeListsVersion(
  filePath: String,
  backend: Backend,
  version: String,
): Unit = {
  val path = Paths.get(filePath)
  val backendStr = backend.name
  // read file
  val fileContent = Using(Source.fromFile(filePath)) { source =>
    source
      .getLines()
      .map {
        case line if line.contains("find_package(") =>
          // change version
          s"find_package($backendStr $version EXACT REQUIRED)"
        case line => line
      }
      .mkString("\n")
  }.getOrElse(throw new RuntimeException("Failed to read the file"))

  // write file with new content
  Files.writeString(
    path,
    fileContent,
    StandardOpenOption.WRITE,
    StandardOpenOption.TRUNCATE_EXISTING,
  )
}

def updateBackendVersion(
  backend: Backend,
  version: String,
): Unit = {
  val cmakeListsPath = s"${getWorkspaceDir(backend)}/CMakeLists.txt"
  updateCMakeListsVersion(cmakeListsPath, backend, version)
}

def formatNumber(n: Int | Double): String = n match {
  case i: Int    => i.toString
  case d: Double => f"$d%f"
}

val formattedDateTime =
  val now = LocalDateTime.now()
  val formatter = DateTimeFormatter.ofPattern("MMddHHmmss")
  now.format(formatter)

def deleteDirectoryRecursively(file: File): Unit = {
  if (file.isDirectory) {
    file.listFiles.foreach(deleteDirectoryRecursively)
  }
  file.delete()
}

/** set timeout with optional limitation */
def timeout[T](f: => T, limit: Option[Int]): T =
  limit.fold(f)(l => timeout(f, l.second))

/** set timeout with limitation */
def timeout[T](f: => T, limit: Int): T =
  timeout(f, limit.seconds)

/** set timeout with duration */
def timeout[T](f: => T, duration: Duration): T =
  Await.result(Future(Try(f)), duration).get
//TODO : move this to somewhere else
