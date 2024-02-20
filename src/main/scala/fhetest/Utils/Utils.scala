package fhetest.Utils

import org.twc.terminator.Main.ENC_TYPE as T2ENC_TYPE

import sys.process.*
import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import java.util.Comparator
import scala.util.Try

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import java.util.concurrent.atomic.AtomicInteger
import fhetest.Generate.Strategy

enum Backend(val name: String):
  case SEAL extends Backend("SEAL")
  case OpenFHE extends Backend("OpenFHE")

enum ENC_TYPE:
  case None, ENC_INT, ENC_DOUBLE

type DirName = String

case class EncParams(ringDim: Int, mulDepth: Int, plainMod: Int)

def translateT2EncType(enc_type: T2ENC_TYPE): ENC_TYPE = enc_type match
  case T2ENC_TYPE.NONE       => ENC_TYPE.ENC_INT
  case T2ENC_TYPE.ENC_INT    => ENC_TYPE.ENC_INT
  case T2ENC_TYPE.ENC_DOUBLE => ENC_TYPE.ENC_DOUBLE

def parsePrefixedArg(input: String): Option[String] = {
  Option(input).collect { case s if s.startsWith("--") => s.stripPrefix("--") }
}

def parseBackend(backendString: String): Option[Backend] =
  parsePrefixedArg(backendString) flatMap {
    case seal: String if seal.toLowerCase() == "seal" => Some(Backend.SEAL)
    case openfhe: String if openfhe.toLowerCase() == "openfhe" =>
      Some(Backend.OpenFHE)
    case _ => None
  }

def parseEncType(encTypeString: String): ENC_TYPE =
  parsePrefixedArg(encTypeString) match
    case Some("INT")    => ENC_TYPE.ENC_INT
    case Some("DOUBLE") => ENC_TYPE.ENC_DOUBLE
    case _              => ENC_TYPE.None

// TODO: Refactor this function
def parseWordSizeAndEncParams(args: List[String]): (Option[Int], EncParams) = {
  val argMap = args
    .grouped(2)
    .collect {
      case List(key, value) => key -> value
    }
    .toMap

  val wordSizeOpt = argMap.get("-w").map(_.toInt)
  val ringDim = argMap.get("-n").map(_.toInt).getOrElse(0)
  val mulDepth = argMap.get("-d").map(_.toInt).getOrElse(0)
  val plainMod = argMap.get("-m").map(_.toInt).getOrElse(0)

  (wordSizeOpt, EncParams(ringDim, mulDepth, plainMod))
}
def parseStrategy(sString: String): Strategy =
  parsePrefixedArg(sString) match
    case Some("exhaust") => Strategy.Exhaustive
    case Some("random")  => Strategy.Random
    case _               => throw new Exception("Invalid strategy")

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

val silentLogger = ProcessLogger(_ => (), _ => ())

def compare(obtained: String, expected: String): Unit = {
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
              assert(
                Math.abs(obtained - result) < 0.001,
                s"$obtained and $result are not close",
              )
          }
    }
}

case class T2Program(content: String)

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
