package fhetest.Phase

import fhetest.Utils.*
import org.twc.terminator.t2dsl_compiler.T2DSLsyntaxtree.*;
import org.twc.terminator.SymbolTable;

import java.nio.file.{Files, Paths};
import java.io.{File, InputStream, ByteArrayInputStream}
import scala.jdk.CollectionConverters._

case object Check {

  def apply(
    directory: String,
    backends: List[Backend],
    encParams: EncParams,
  ): String = {
    val dir = new File(directory)
    if (dir.exists() && dir.isDirectory) {
      val files = Files.list(Paths.get(directory))
      val fileList = files.iterator().asScala.toList
      val fileStr = fileList.map(filePath =>
        Files.readAllLines(filePath).asScala.mkString("\n"),
      )
      files.close()
      apply(fileStr, backends, encParams)
    } else {
      "Argument parsing error: Directory does not exist or is not a directory"
    }
  }

  def apply(
    programs: List[String],
    backends: List[Backend],
    encParams: EncParams,
  ): String = {
    val bugs =
      programs.foldLeft(List[(String, List[(String, String)])]())(
        (acc, pgm) => {
          val pgm2inputstream = new ByteArrayInputStream(pgm.getBytes("UTF-8"))
          val (interpResult, executeResults) =
            getResults(pgm2inputstream, backends, encParams)
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
              else { acc :+ (pgm, ("", "[Expected] \n" + expected) :: lst) }
            }
            case InterpError => acc :+ (pgm, List(("CLEAR", "InterpFail")))
          }
        },
      )
    bugs.foldLeft("")((str, bug) => {
      val (pgm, report) = bug
      val printPgm = "-" * 10 + " Program " + "-" * 10 + "\n" + pgm + "\n"
      val printReport =
        "-" * 10 + " Report " + "-" * 10 + "\n" + report.foldLeft("")(
          (str2, r) => str2 + r._1 + r._2 + "\n",
        )
      str + printPgm + printReport
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
    program: InputStream,
    backends: List[Backend],
    encParams: EncParams,
  ): ((String, ExecuteResult), List[(String, ExecuteResult)]) = {
    val (ast, symbolTable, encType) = Parse(program)
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
