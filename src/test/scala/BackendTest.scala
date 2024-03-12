import fhetest.*
import fhetest.Phase.{Execute, Parse, Print, Interp}
import fhetest.Utils.*

import java.nio.file.{Files, Paths, Path}
import scala.jdk.CollectionConverters.*
import scala.concurrent.{Future, ExecutionContext}
import scala.util.{Success, Failure}

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.Assertion
import org.scalatest.Assertions.succeed

def getT2files(testSetDir: String): List[Path] =
  Files
    .newDirectoryStream(Paths.get(s"$testSetDir/t2"), "*.t2")
    .iterator()
    .asScala
    .toList

def verifyResults(obtained: String, expected: String): Assertion = {
  compare(obtained, expected)
  succeed
}

abstract class BackendTest(
  val backend: Backend,
  val testSetDir: String,
  val wordSizeOpt: Option[Int] = None,
  val encParamsOpt: Option[EncParams] = Some(EncParams(32768, 5, 65537)),
) extends AnyFunSuite {

  val t2Files = getT2files(testSetDir)
  assert(t2Files.nonEmpty, "No test files found")
  for {
    t2File <- t2Files
  } {
    val t2FileName = t2File.getFileName.toString
    val resultFileName = t2FileName.replace(".t2", ".res")
    val resultFilePath = Paths.get(s"$testSetDir/result", resultFileName)
    val testName = t2FileName.replace(".t2", "")

    val resultFileContents = new String(Files.readAllBytes(resultFilePath))
    val (ast, symTable, enc_type) = Parse(t2File.toString)

    test(testName + "/" + backend) {
      val obtained = withBackendTempDir(
        backend,
        workspaceDir => {
          given DirName = workspaceDir
          Print(ast, symTable, enc_type, backend, wordSizeOpt, encParamsOpt)
          Execute(backend, None)
        },
      )

      val testResult = verifyResults(obtained, resultFileContents)
      testResult
    }
  }
}

import Backend.*
class SEALBasicTest extends BackendTest(SEAL, BASIC_TESTSET_DIR)
class OpenFHEBasicTest extends BackendTest(OpenFHE, BASIC_TESTSET_DIR)

class SEALBinAdvancedTest
  extends BackendTest(
    SEAL,
    BIN_ADVANCED_TESTSET_DIR,
    Some(6),
    Some(EncParams(32768, 20, 65537)),
  )
class OpenFHEBinAdvancedTest
  extends BackendTest(
    OpenFHE,
    BIN_ADVANCED_TESTSET_DIR,
    Some(6),
    Some(EncParams(32768, 20, 65537)),
  )

class SEALArithAdvancedTest
  extends BackendTest(
    SEAL,
    ARITH_ADVANCED_TESTSET_DIR,
    None,
    Some(EncParams(32768, 20, 65537)),
  )
class OpenFHEArithAdvancedTest
  extends BackendTest(
    OpenFHE,
    ARITH_ADVANCED_TESTSET_DIR,
    None,
    Some(EncParams(32768, 20, 65537)),
  )
abstract class InterpTest(
  val testSetDir: String,
) extends AnyFunSuite {

  val t2Files = getT2files(testSetDir)
  assert(t2Files.nonEmpty, "No test files found")

  for {
    t2File <- t2Files
  } {
    val t2FileName = t2File.getFileName.toString
    val resultFileName = t2FileName.replace(".t2", ".res")
    val resultFilePath = Paths.get(s"$testSetDir/result", resultFileName)
    val testName = t2FileName.replace(".t2", "")

    test(testName + "/" + "interp") {
      val resultFileContents = new String(Files.readAllBytes(resultFilePath))
      val (ast, _, _) = Parse(t2File.toString)

      val obtained = Interp(ast, 32768, 65537)

      val testResult = verifyResults(obtained, resultFileContents)
      testResult
    }
  }
}

class BasicInterpTest extends InterpTest(BASIC_TESTSET_DIR)
