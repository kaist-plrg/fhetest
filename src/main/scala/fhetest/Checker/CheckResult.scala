package fhetest.Checker

import fhetest.Phase.Check

trait CheckResult {
  val results: List[BackendResultPair]
  override def toString: String = {
    val className = this.getClass.getSimpleName.replace("$", "")
    val resultStrings = results.map {
      case BackendResultPair(backendName, executeResult) =>
        s"\n- $backendName:\n ${executeResult.toString}"
    }
    s"[$className] ${resultStrings.mkString("")}"
  }
}
case class Same(results: List[BackendResultPair]) extends CheckResult
case class Diff(
  results: List[BackendResultPair],
  fails: List[BackendResultPair],
) extends CheckResult
// InvalidResults: only for printing results in debug mode
case class InvalidResults(results: List[BackendResultPair]) extends CheckResult
case class InvalidNormalResults(
  results: List[BackendResultPair],
  normals: List[BackendResultPair],
) extends CheckResult
case class InvalidExpectedExceptions(
  results: List[BackendResultPair],
  expectedExceptions: List[BackendResultPair],
) extends CheckResult
case class InvalidUnexpectedExceptions(
  results: List[BackendResultPair],
  unexpectedExceptions: List[BackendResultPair],
) extends CheckResult
case class InvalidErrors(
  results: List[BackendResultPair],
  errors: List[BackendResultPair],
) extends CheckResult
case class InvalidCryptoContextInOpenFHE(
  results: List[BackendResultPair],
  invalidCryptoContexts: List[BackendResultPair],
) extends CheckResult
case class ParserError(results: List[BackendResultPair]) extends CheckResult
