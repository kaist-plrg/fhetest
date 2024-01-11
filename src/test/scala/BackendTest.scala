import fhetest.*
import fhetest.Phase.{Compile, Execute}

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters._

import munit.Clue.generate

abstract class BackendTest(val backend: String) extends munit.FunSuite {
  // T2_RESOURCE_DIR에서 .t2 확장자를 가진 모든 파일을 가져옵니다.
  val t2Files =
    Files
      .newDirectoryStream(Paths.get(T2_RESOURCE_DIR), "*.t2")
      .iterator()
      .asScala
      .toList

  // 각 .t2 파일에 대해, 해당 파일의 이름을 가져와서 .res 확장자로 변경하고,
  // 그 이름으로 RESULT_RESOURCE_DIR에서 파일을 찾아 그 내용을 읽습니다.
  for t2File <- t2Files do {
    val t2FileName = t2File.getFileName.toString
    val resultFileName = t2FileName.replace(".t2", ".res")
    val resultFilePath = Paths.get(RESULT_RESOURCE_DIR, resultFileName)
    val testName = t2FileName.replace(".t2", "")

    // 파일의 내용을 문자열로 읽습니다.
    val resultFileContents = new String(Files.readAllBytes(resultFilePath))

    test(testName) {
      // 컴파일합니다.
      Compile(t2File.toString, backend)
      // 컴파일된 파일을 실행합니다.
      val obtained = Execute(backend)

      val obtainedLines = obtained.split("\n")
      val resultLines = resultFileContents.split("\n")
      obtainedLines.zip(resultLines).foreach {
        case (obtainedLine, resultLine) =>
          val obtainedNumbers = obtainedLine.split(" ").map(_.toDouble)
          val resultNumbers = resultLine.split(" ").map(_.toDouble)
          obtainedNumbers.zip(resultNumbers).foreach {
            case (obtained, result) =>
              assert(
                Math.abs(obtained - result) < 0.0001,
                s"$obtained and $result are not close",
              )
          }
      }
    }
  }
}

class SEALbackendTest extends BackendTest("SEAL")
class OpenFHEbackendTest extends BackendTest("OpenFHE")
