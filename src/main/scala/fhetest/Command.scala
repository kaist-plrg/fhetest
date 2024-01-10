package fhetest

import org.twc.terminator.Main

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

/** `compile` command */
case object CmdCompile extends Command("compile") {
  val help = "compiles a T2 file to the given backend."
  val examples = List(
    "fhetest compile tmp.t2 --SEAL",
    "fhetest compile tmp.t2 --OpenFHE",
  )
  def apply(args: List[String]): Unit = ??? // TODO
}

/** `execute` command */
case object CmdExecute extends Command("execute") {
  val help = "Execute the compiled code in the given backend."
  val examples = List(
    "fhetest execute --SEAL",
    "fhetest execute --OpenFHE",
  )
  def apply(args: List[String]): Unit = ??? // TODO
}

/** `interp` command */
case object CmdInterp extends Command("interp") {
  val help = "Interp a T2 file."
  val examples = List(
    "fhetest interp tmp.t2",
  )
  def apply(args: List[String]): Unit = ??? // TODO
}

/** `run` command */
case object CmdRun extends Command("run") {
  val help = "Run the given T2 program."
  val examples = List(
    "fhetest run tmp.t2 --SEAL",
    "fhetest run tmp.t2 --OpenFHE",
    "fhetest run tmp.t2",
  )
  def apply(args: List[String]): Unit = ??? // TODO
}
