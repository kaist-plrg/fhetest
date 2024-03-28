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
    // Generate random T2 programs
    CmdGen,
    // Check results of T2 program execution
    CmdCheck,
    // Check after Generate random T2 programs.
    CmdTest,
    // Replay the given json
    CmdReplay,
    // Make a json report of invalid program testing
    // Count the number of programs tested for each combination of valid filters
    CmdCount,
  )
  val cmdMap = commands.foldLeft[Map[String, Command]](Map()) {
    case (map, cmd) => map + (cmd.name -> cmd)
  }
}
