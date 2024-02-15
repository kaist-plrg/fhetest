package fhetest.Phase

import fhetest.Utils.*
import org.twc.terminator.t2dsl_compiler.T2DSLsyntaxtree.*;
import org.twc.terminator.SymbolTable;

import java.nio.file.{Files, Paths};

case object Check {

  def apply(
    programs: List[(Goal, EncParams, ENC_TYPE, SymbolTable)],
    backends: List[Backend],
  ): String = {
    val bugs =
      programs.foldLeft(List[(Goal, List[(String, String)])]())((acc, pgm) => {
        val (interpResult, executeResults) = getResults(pgm, backends)
        interpResult._2 match {
          case Normal(expected) => {
            val diffs = executeResults.filter(interpResult._2 != _._2)
            val lst = diffs.map((backend, res) =>
              res match {
                case PrintError => ("- " + backend.toString, ": PrintFail")
                case LibraryError(m) =>
                  ("- " + backend.toString, ": [Exception] " + m)
                case Normal(r) => ("- " + backend.toString, ": \n" + r)
              },
            )
            if (lst.isEmpty) acc
            else { acc :+ (pgm._1, ("", "[Expected] \n" + expected) :: lst) }
          }
          case InterpError => acc :+ (pgm._1, List(("CLEAR", "InterpFail")))
        }
      })
    bugs.foldLeft("")((str, bug) => {
      // TODO: print ast
      // val ast = bug._1
      val report = bug._2
      str + report.foldLeft("")((str2, r) => str2 + r._1 + r._2 + "\n")
    })
  }

  trait ExecuteResult
  case class Normal(res: String) extends ExecuteResult
  // case object ParseError extends ExecuteResult //TODO: if receive T2 program string instead of AST
  case object InterpError extends ExecuteResult
  case object PrintError extends ExecuteResult
  case class LibraryError(msg: String) extends ExecuteResult
  // case object TimeoutError extends ExecuteResult //TODO: development
  case object Throw extends ExecuteResult

  def getResults(
    program: (Goal, EncParams, ENC_TYPE, SymbolTable),
    backends: List[Backend],
  ): ((String, ExecuteResult), List[(String, ExecuteResult)]) = {
    val ast = program._1
    val encParams = program._2
    val encType = program._3
    val symbolTable = program._4
    val interpResult = (
      "CLEAR",
      try {
        val res = Interp(ast, encParams.ringDim, encParams.plainMod)
        Normal(res.trim)
      } catch { case _ => InterpError },
    )
    val executeResults: List[(String, ExecuteResult)] =
      backends.map(backend =>
        (
          backend.toString,
          execute(backend, ast, encParams, encType, symbolTable),
        ),
      )
    (interpResult, executeResults)
  }

  def execute(
    backend: Backend,
    ast: Goal,
    encParams: EncParams,
    encType: ENC_TYPE,
    symbolTable: SymbolTable,
  ): ExecuteResult =
    withBackendTempDir(
      backend,
      { workspaceDir =>
        given DirName = workspaceDir
        try {
          Print(
            ast,
            symbolTable,
            encType,
            backend,
            encParamsOpt = Some(encParams),
          )
          try {
            val res = Execute(backend)
            Normal(res.trim)
          } catch {
            // TODO?: classify exception related with parmeters?
            case ex: Exception => LibraryError(ex.getMessage)
          }
        } catch {
          case _ => PrintError
        }
      },
    )
}
