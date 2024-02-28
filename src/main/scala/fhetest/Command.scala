package fhetest

import fhetest.Utils.*
import fhetest.Generate.*
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

  /** run command with parsed arguments */
  def runJob(config: Config): Unit

  /** run command with command-line arguments */
  def apply(args: List[String]) = {
    val config = Config(args)
    runJob(config)
  }
}
abstract class BackendCommand(
  /** command name */
  override val name: String,
) extends Command(name) {

  /** run command with command-line arguments */
  override def apply(args: List[String]) = {
    val config = Config(args)
    val sealVersion = config.sealVersion
    val openfheVersion = config.openfheVersion
    updateBackendVersion(Backend.SEAL, sealVersion)
    updateBackendVersion(Backend.OpenFHE, openfheVersion)
    runJob(config)
  }
}

/** base command */
case object CmdBase extends Command("") {
  val help = "does nothing."
  val examples = Nil
  def runJob(config: Config) = ()
}

/** `help` command */
case object CmdHelp extends Command("help") {
  val help = "shows help messages."
  val examples = List(
    "fhetest help",
  )
  def runJob(config: Config): Unit = {
    println("Usage: fhetest <command> [options]")
    println("Commands:")
    for cmd <- FHETest.commands do println(s"  ${cmd.name}\n\t${cmd.help}")
  }
}

/** `interp` command */
case object CmdInterp extends Command("interp") {
  val help = "Interp a T2 file."
  val examples = List(
    "fhetest interp -file:tmp.t2",
    "fhetest interp -file:tmp.t2 -n:4096 -m:40961",
  )
  def runJob(config: Config): Unit =
    val fname = config.fileName.getOrElseThrow("No T2 file given.")
    val (ast, _, _) = Parse(fname)
    val ringDim: Int = config.encParams.ringDim
    val plainMod: Int = config.encParams.plainMod
    val result = Interp(ast, ringDim, plainMod)
    print(result)
}

/** `run` command */
case object CmdRun extends BackendCommand("run") {
  val help = "Run the given T2 program."
  val examples = List(
    "fhetest run -file:tmp.t2 -b:SEAL",
    "fhetest run -file:tmp.t2 -b:OpenFHE",
    "fhetest run -file:tmp.t2 -b:SEAL -w:4 -n:4096 -d:5 -m:40961",
    "fhetest run -file:tmp.t2",
  )
  def runJob(config: Config): Unit =
    val fname = config.fileName.getOrElseThrow("No T2 file given.")
    val encParams: EncParams = config.encParams
    val wordSizeOpt: Option[Int] = config.wordSize
    val plainMod: Int = config.encParams.plainMod
    config.backend match {
      case Some(backend) =>
        given DirName = getWorkspaceDir(backend)
        val (ast, symbolTable, encType) = Parse(fname)
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
      case None =>
        val (ast, _, _) = Parse(fname)
        val result = Interp(ast, 32768, 65537)
        print(result)
    }
}

/** `compile` command */
case object CmdCompile extends Command("compile") {
  val help = "compiles a T2 file to the given backend."
  val examples = List(
    "fhetest compile -file:tmp.t2 -b:SEAL",
    "fhetest compile -file:tmp.t2 -b:OpenFHE",
  )
  def runJob(config: Config): Unit =
    val fname = config.fileName.getOrElseThrow("No T2 file given.")
    val backend = config.backend.getOrElseThrow("No backend given.")
    given DirName = getWorkspaceDir(backend)
    val (ast, symbolTable, encType) = Parse(fname)
    Print(ast, symbolTable, encType, backend)
}

/** `execute` command */
case object CmdExecute extends BackendCommand("execute") {
  val help = "Execute the compiled code in the given backend."
  val examples = List(
    "fhetest execute -b:SEAL",
    "fhetest execute -b:OpenFHE",
  )
  def runJob(config: Config): Unit =
    val backend = config.backend.getOrElseThrow("No backend given.")
    given DirName = getWorkspaceDir(backend)
    val output = Execute(backend)
    println(output)
}

/** `gen` command */
case object CmdGen extends Command("gen") {
  val help = "Generate random T2 programs."
  val examples = List(
    "fhetest gen -type:int -c:10",
    "fhetest gen -type:double -c:10",
    "fhetest gen -type:int -stg:exhaust -c:10",
    "fhetest gen -type:double -stg:random -c:10",
  )
  def runJob(config: Config): Unit =
    val encType = config.encType.getOrElseThrow("No encType given.")
    val genCount = config.genCount.getOrElse(10)
    val generator = Generate(encType)
    generator.show(List(Backend.SEAL, Backend.OpenFHE), genCount, encType)
}

/** `check` command */
case object CmdCheck extends BackendCommand("check") {
  val help = "Check results of T2 program execution."
  val examples = List(
    "fhetest check -dir:tmp -json:true",
  )
  // TODO: json option 추가
  def runJob(config: Config): Unit =
    val dir = config.dirName.getOrElseThrow("No directory given.")
    val encParams: EncParams = config.encParams
    val backends = List(Backend.SEAL, Backend.OpenFHE)
    val toJson = config.toJson
    val sealVersion = config.sealVersion
    val openfheVersion = config.openfheVersion
    val outputs =
      Check(dir, backends, encParams, toJson, sealVersion, openfheVersion)
    for output <- outputs do {
      println(output)
    }
}

/** `test` command */
case object CmdTest extends BackendCommand("test") {
  val help = "Check after Generate random T2 programs."
  val examples = List(
    "fhetest test -type:int -stg:random",
    "fhetest test -type:int -stg:random -count:10",
    "fhetest test -type:double -stg:exhaust -count:10",
    "fhetest test -type:double -stg:random -json:true -seal:4.0.0 -openfhe:1.0.4",
  )

  def runJob(config: Config): Unit =
    val encType = config.encType.getOrElseThrow("No encType given.")
    val genStrategy = config.genStrategy.getOrElse(Strategy.Random)
    val genCount = config.genCount
    val generator = Generate(encType, genStrategy)
    val programs = generator(genCount).map(T2Program(_))
    val backendList = List(Backend.SEAL, Backend.OpenFHE)
    val encParams = config.encParams
    val toJson = config.toJson
    val sealVersion = config.sealVersion
    val openfheVersion = config.openfheVersion
    val outputs = Check(
      programs,
      backendList,
      encParams,
      toJson,
      sealVersion,
      openfheVersion,
    )
    for (program, output) <- outputs do {
      println("=" * 80)
      println("Program : " + program.content)
      println("-" * 80)
      println(output)
      println("=" * 80)
    }
}
