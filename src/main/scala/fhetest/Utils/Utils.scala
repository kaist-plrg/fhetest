package fhetest.Utils

import org.twc.terminator.Main.ENC_TYPE as T2ENC_TYPE

import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import java.util.{Comparator, UUID}
import scala.util.Try

enum Backend(val name: String):
  case SEAL extends Backend("SEAL")
  case OpenFHE extends Backend("OpenFHE")

enum ENC_TYPE:
  case None, ENC_INT, ENC_DOUBLE

type DirName = String

def translateT2EncType(enc_type: T2ENC_TYPE): ENC_TYPE = enc_type match
  case T2ENC_TYPE.NONE       => ENC_TYPE.ENC_INT
  case T2ENC_TYPE.ENC_INT    => ENC_TYPE.ENC_INT
  case T2ENC_TYPE.ENC_DOUBLE => ENC_TYPE.ENC_DOUBLE

def parsePrefixedArg(input: String): Option[String] = {
  Option(input).collect { case s if s.startsWith("--") => s.stripPrefix("--") }
}

def parseBackend(backendString: String): Option[Backend] =
  parsePrefixedArg(backendString).flatMap {
    case "SEAL" | "seal"       => Some(Backend.SEAL)
    case "OpenFHE" | "openfhe" => Some(Backend.OpenFHE)
    case _                     => None
  }

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
