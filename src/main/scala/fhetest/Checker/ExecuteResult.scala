package fhetest.Checker

import fhetest.Utils.*

trait ExecuteResult
case class Normal(res: String) extends ExecuteResult {
  override def toString: String = res
}
case object InterpError extends ExecuteResult
case object PrintError extends ExecuteResult
case class LibraryError(msg: String) extends ExecuteResult {
  override def toString: String = s"LibraryError: $msg"
}
case object ParseError extends ExecuteResult
// case object TimeoutError extends ExecuteResult //TODO: development
// case object Throw extends ExecuteResult

case class BackendResultPair(backend: String, result: ExecuteResult)

def isDiff(
  expected: BackendResultPair,
  obtained: BackendResultPair,
): Boolean =
  (obtained.result, expected.result) match {
    case (Normal(obtained), Normal(expected)) =>
      try { compare(obtained, expected); false }
      catch { case _ => true }
    case _ => true
  }
