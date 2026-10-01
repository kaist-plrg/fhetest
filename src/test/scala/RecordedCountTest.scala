import fhetest.{Config, CmdTest}
import fhetest.Generate.{LibConfig, T2Program}
import fhetest.Phase.Check
import org.scalatest.funsuite.AnyFunSuite

class RecordedCountTest extends AnyFunSuite {
  test("a checked-result limit skips overflow inputs and does not check the next input") {
    val config = LibConfig(firstModSize = 1)
    def program(value: Double) = T2Program(
      s"int main(void) { EncDouble x; x = $value; print_batched(x, 1); return 0; }",
      config,
      Nil,
    )
    val accepted = program(1.0)
    var visited = 0
    val inputs = Iterator(program(2.0), accepted, program(0.0)).map { input =>
      visited += 1
      input
    }
    val outputs = Check(inputs, Nil, None, false, "4.1.2", "1.4.2", true, false, None)
    assert(outputs.take(1).map(_._1).toList == List(accepted))
    assert(visited == 2)
  }

  test("resultcount requires a positive valid JSON target without a candidate cap") {
    for (config <- List(
      new Config(resultCount = Some(0), toJson = true),
      new Config(resultCount = Some(1), toJson = false),
      new Config(resultCount = Some(1), toJson = true, validFilter = false),
      new Config(resultCount = Some(1), toJson = true, genCount = Some(1)),
    )) intercept[IllegalArgumentException] { CmdTest.runJob(config) }
  }
}
