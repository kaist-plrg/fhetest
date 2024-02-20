package fhetest

import fhetest.Utils.*
import fhetest.Phase.{Parse, Interp, Print, Execute, Generate, Check}

sealed abstract class Command(
  /** command name */
  val name: String,
) {
  override def toString: String = name

  /** help message */
  def help: String

  /** help message */
  def examples: List[String]

  /** run command with command-line arguments */
  def apply(args: List[String]): Unit
}

/** base command */
case object CmdBase extends Command("") {
  val help = "does nothing."
  val examples = Nil
  def apply(args: List[String]) = ()
}

/** `help` command */
case object CmdHelp extends Command("help") {
  val help = "shows help messages."
  val examples = List(
    "fhetest help",
  )
  def apply(args: List[String]): Unit = {
    println("Usage: fhetest <command> [options]")
    println("Commands:")
    for cmd <- FHETest.commands do println(s"  ${cmd.name}\n\t${cmd.help}")
  }
}

/** `interp` command */
case object CmdInterp extends Command("interp") {
  val help = "Interp a T2 file."
  val examples = List(
    "fhetest interp tmp.t2",
    "fhetest interp tmp.t2 -n 4096 -m 40961",
  )
  def apply(args: List[String]): Unit = args match {
    case file :: Nil => {
      val (ast, _, _) = Parse(file)
      val result = Interp(ast, 32768, 65537)
      print(result)
    }
    case file :: remainArgs => {
      val (ast, _, _) = Parse(file)
      val (_, encParams) = parseWordSizeAndEncParams(remainArgs)
      val result = Interp(ast, encParams.ringDim, encParams.plainMod)
      print(result)
    }
    case Nil => println("No T2 file given.")
  }
}

/** `run` command */
case object CmdRun extends Command("run") {
  val help = "Run the given T2 program."
  val examples = List(
    "fhetest run tmp.t2 --SEAL",
    "fhetest run tmp.t2 --OpenFHE",
    "fhetest run tmp.t2 --SEAL -w 4 -n 4096 -d 5 -m 40961",
    "fhetest run tmp.t2",
  )
  // TODO: Refactor this function: parseWordSizeAndEncParams
  def apply(args: List[String]): Unit = args match {
    case file :: backendString :: remainArgs =>
      parseBackend(backendString) match {
        case Some(backend) =>
          given DirName = getWorkspaceDir(backend)
          val (ast, symbolTable, encType) = Parse(file)
          val (wordSizeOpt, encParams) = parseWordSizeAndEncParams(remainArgs)
          Print(
            ast,
            symbolTable,
            encType,
            backend,
            wordSizeOpt,
            Some(encParams),
          )
          val result = Execute(backend)
          print(result)
        case None => println("Argument parsing error: Invalid backend.")
      }
    case file :: Nil =>
      val (ast, _, _) = Parse(file)
      val result = Interp(ast, 32768, 65537)
      print(result)
    case Nil => println("No T2 file given.")
  }
}

/** `compile` command */
case object CmdCompile extends Command("compile") {
  val help = "compiles a T2 file to the given backend."
  val examples = List(
    "fhetest compile tmp.t2 --SEAL",
    "fhetest compile tmp.t2 --OpenFHE",
  )
  def apply(args: List[String]): Unit = args match {
    case file :: backendString :: remain =>
      parseBackend(backendString) match {
        case Some(backend) =>
          given DirName = getWorkspaceDir(backend)
          val (ast, symbolTable, encType) = Parse(file)
          remain match {
            case params :: _ => ???
            case Nil         => Print(ast, symbolTable, encType, backend)
          }
        case None => println("Argument parsing error: Invalid backend.")
      }
    case _ :: Nil => println("No backend given.")
    case Nil      => println("No T2 file given.")
  }

}

/** `execute` command */
case object CmdExecute extends Command("execute") {
  val help = "Execute the compiled code in the given backend."
  val examples = List(
    "fhetest execute --SEAL",
    "fhetest execute --OpenFHE",
  )
  def apply(args: List[String]): Unit = args match {
    case backendString :: _ =>
      parseBackend(backendString) match {
        case Some(backend) =>
          given DirName = getWorkspaceDir(backend)
          val output = Execute(backend)
          println(output)
        case None => println("Argument parsing error: Invalid backend.")
      }
    case Nil => println("No backend given.")
  }
}

// TODO : Get Strategy from the command line
/** `gen` command */
case object CmdGen extends Command("gen") {
  val help = "Generate random T2 programs."
  val examples = List(
    "fhetest gen --INT 10",
    "fhetest gen --DOUBLE 10",
  )
  def apply(args: List[String]): Unit = args match {
    case Nil => println("No argument given.")
    case encTypeString :: remain =>
      val encType = parseEncType(encTypeString)
      val generator = Generate(encType)
      val n = remain match {
        case nString :: Nil =>
          nString.toInt
        case _ => 10 // default value
      }
      generator.show(List(Backend.SEAL, Backend.OpenFHE), n)
  }
}

/** `check` command */
case object CmdCheck extends Command("check") {
  val help = "Check results of T2 program execution."
  val examples = List(
    "fhetest check tmp --SEAL --OpenFHE",
  )
  def apply(args: List[String]): Unit = args match {
    case dir :: backendStrings => {
      val backendList = backendStrings.flatMap(parseBackend(_))
      if (backendStrings.size == backendList.size) {
        // TODO: temporary encParams. Fix after having parameter genernation.
        val encParams = EncParams(32768, 5, 65537)
        val output = Check(dir, backendList, encParams)
        println(output)
      } else { println("Argument parsing error: Invalid backend.") }
    }
    case _ => println("Invalid arguments")
  }
}

/** `test` command */
case object CmdTest extends Command("test") {
  val help = "Check after Generate random T2 programs."
  val examples = List(
    "fhetest test --INT --random",
    "fhetest test --INT --random 10",
    "fhetest test --DOUBLE --exhaust 10",
  )
  def apply(args: List[String]): Unit = args match {
    case Nil => println("No argument given.")
    case encTypeString :: stgString :: remain => {
      val nOpt = remain match {
        case nString :: _ =>
          Some(nString.toInt)
        case _ => None
      }
      val encType = parseEncType(encTypeString)
      val strategy = parseStrategy(stgString)
      val generator = Generate(encType, strategy)
      val programs = generator(nOpt).map(T2Program(_))
      val backendList = List(Backend.SEAL, Backend.OpenFHE)
      // TODO: temporary encParams. Fix after having parameter genernation.
      val encParams = EncParams(32768, 5, 65537)
      for program <- programs do {
        val output = Check(program, backendList, encParams)
        println("=" * 80)
        println("Program : " + program.content)
        println("-" * 80)
        println(output)
        println("=" * 80)
      }
    }
    case _ => println("EncType and Strategy are required.")
  }
}
