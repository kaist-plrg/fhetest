import fhetest.*
import fhetest.Checker.*
import fhetest.Generate.{LibConfig, T2Program, getValidFilterList2str}
import fhetest.Phase.Check
import fhetest.Utils.*

object FixtureCheck {
  def main(args: Array[String]): Unit = {
    require(args.length == 3, "FixtureCheck int|double valid|invalid guided|baseline")
    require(Set("int", "double").contains(args(0)))
    require(Set("valid", "invalid").contains(args(1)))
    require(Set("guided", "baseline").contains(args(2)))
    val valid = args(1) == "valid"
    val double = args(0) == "double"
    val config = LibConfig(
      scheme = if (double) Scheme.CKKS else Scheme.BFV,
      encParams = EncParams(8192, 1, 65537),
      firstModSize = if (double) 50 else 60,
      scalingModSize = if (valid) 40 else 0,
      securityLevel = SecurityLevel.HEStd_128_classic,
      scalingTechnique = ScalingTechnique.FIXEDAUTO,
      lenOpt = Some(4),
    )
    val filter = getValidFilterList2str().indexOf("FilterModSizeIsBeteween14And60bits")
    require(filter >= 0, "Missing modulus-size filter")
    val indices = if (!valid && double && args(2) == "guided") List(filter) else Nil
    val declaration = if (double) "EncDouble x; x = {1.0, 2.0, 3.0, 4.0};"
      else "EncInt x; x = {1, 2, 3, 4};"
    val program = T2Program(
      s"int main(void) { $declaration x = x + 1; print_batched(x, 4); return 0; }",
      config, indices,
    )
    println(s"Fixed fixture: ${args.mkString(" ")}")
    val results = Check(Iterator(program), List(Backend.OpenFHE), None, true,
      "4.1.2", "1.4.2", valid, true, None).toList
    require(results.size == 1, "Fixture was skipped")
    val result = results.head._2
    println(result)
    if (valid) require(result.isInstanceOf[Same], "Valid fixture failed")
    else require(result.results.exists {
      case BackendResultPair("OpenFHE", LibraryException(message)) =>
        message.toLowerCase.contains("mod") || message.toLowerCase.contains("bit")
      case _ => false
    }, "Expected a native modulus-parameter exception")
  }
}
