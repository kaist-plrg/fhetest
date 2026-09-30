import fhetest.Phase.{Interp, Parse}
import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import org.scalatest.funsuite.AnyFunSuite

class InterpNumericAssignmentTest extends AnyFunSuite {
  private def interpret(source: String): List[Double] = {
    val input = new ByteArrayInputStream(source.getBytes(StandardCharsets.UTF_8))
    val (ast, _, _) = try Parse(input) finally input.close()
    Interp(ast, 32768, 65537).trim.split(" +").toList.map(_.toDouble)
  }

  test("integer scalar can initialize an encrypted double") {
    val source = "int main(void) { EncDouble x; x = 2; print_batched(x, 1); return 0; }"
    assert(interpret(source) == List(2.0))
  }

  test("integer vector can initialize an encrypted double array element") {
    val source = """int main(void) {
      EncDouble[] x; x = new EncDouble[1];
      x[0] = {1, 4, 7}; print_batched(x[0], 3); return 0;
    }"""
    assert(interpret(source) == List(1.0, 4.0, 7.0))
  }

  test("paper RQ1 example matches independently computed polynomial values") {
    val (ast, _, _) = Parse("src/main/resources/paper/logistic_regression_a4_fp_paper.t2")
    // Weighted sums are 5, 14, 23; the example computes 24 + 12*x + x^3.
    val obtained = Interp(ast, 32768, 65537).trim.split(" +").toList.map(_.toDouble)
    assert(obtained == List(209.0, 2936.0, 12467.0))
  }
}
