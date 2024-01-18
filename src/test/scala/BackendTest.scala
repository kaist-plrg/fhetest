import fhetest.*
import fhetest.Phase.{Execute, Parse, Print}
import fhetest.Utils.*

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.*
import scala.concurrent.{Future, ExecutionContext}
import scala.util.{Success, Failure}

import org.scalatest.funsuite.AsyncFunSuite
import org.scalatest.Assertion

abstract class BackendTest(
  val backends: List[Backend],
  val testSetDir: String,
  val wordSizeOpt: Option[Int] = None,
  val encParamsOpt: Option[EncParams] = None,
) extends AsyncFunSuite {
  given ExecutionContext = ExecutionContext.global

  val t2Files =
    Files
      .newDirectoryStream(Paths.get(s"$testSetDir/t2"), "*.t2")
      .iterator()
      .asScala
      .toList

  assert(t2Files.nonEmpty, "No test files found")

  // 결과 검증 로직을 별도의 함수로 분리
  def verifyResults(obtained: String, expected: String): Assertion = {
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
                  Math.abs(obtained - result) < 0.0001,
                  s"$obtained and $result are not close",
                )
            }
      }
    succeed
  }

  // 모든 테스트 결과를 담을 Future 리스트
  val allTests: List[Future[Assertion]] = t2Files.flatMap { t2File =>
    val t2FileName = t2File.getFileName.toString
    val resultFileName = t2FileName.replace(".t2", ".res")
    val resultFilePath = Paths.get(s"$testSetDir/result", resultFileName)
    val testName = t2FileName.replace(".t2", "")

    val resultFileContents = new String(Files.readAllBytes(resultFilePath))
    val (ast, symTable, enc_type) = Parse(t2File.toString)

    backends.map { backend =>
      val testFuture = Future {
        withBackendTempDir(
          backend,
          workspaceDir => {
            given DirName = workspaceDir
            Print(ast, symTable, enc_type, backend, wordSizeOpt, encParamsOpt)
            Execute(backend)
          },
        )
      }.map(obtained => verifyResults(obtained, resultFileContents),
      ) // 결과 검증 함수 호출

      test(testName + "/" + backend)(testFuture)
      testFuture
    }
  }

  // 모든 테스트가 완료될 때까지 기다림
  Future.sequence(allTests).map(_ => ())
}

import Backend.*
class SEALBasicTest extends BackendTest(List(SEAL), BASIC_TESTSET_DIR)
class OpenFHEBasicTest extends BackendTest(List(OpenFHE), BASIC_TESTSET_DIR)

class SEALAdditionalTest
  extends BackendTest(
    List(SEAL),
    ADDITIONAL_TESTSET_DIR,
    Some(8),
    Some(EncParams(32768, 20, 65537)),
  )
class OpenFHEAdditionalTest
  extends BackendTest(
    List(OpenFHE),
    ADDITIONAL_TESTSET_DIR,
    Some(8),
    Some(EncParams(32768, 20, 65537)),
  )
