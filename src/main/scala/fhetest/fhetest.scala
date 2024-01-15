package fhetest

object FHETest {

  /** the main entry point */
  def main(tokens: Array[String]): Unit =
    tokens.toList match
      case Nil => throw new Error(s"No command given")
      case str :: args =>
        cmdMap.get(str) match {
          case Some(cmd) => cmd(args)
          case None      => throw new Error(s"Unknown command: $str")
        }

  /** commands */
  val commands: List[Command] = List(
    // help message
    CmdHelp,
    // T2 programs -> the given backend (SEAL, OpenFHE)
    CmdCompile,
    // Execute the compiled code in the given backend (SEAL, OpenFHE)
    CmdExecute,
    // Interp a T2 file
    CmdInterp,
    // Run the given T2 program
    // Compile -> Execute (SEAL, OpenFHE) / Interp (if no backend is given)
    CmdRun,
  )
  val cmdMap = commands.foldLeft[Map[String, Command]](Map()) {
    case (map, cmd) => map + (cmd.name -> cmd)
  }
}
