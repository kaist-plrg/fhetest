package fhetest

import fhetest.Utils.*
import fhetest.Generate.*
import fhetest.Generate.Utils.combinations
import fhetest.Phase.{Parse, Interp, Print, Execute, Generate, Check}
import fhetest.Checker.{Diff, DumpUtil, ParserError, ResultValidInfo, Same}

import java.nio.file.{Files, Paths};
import java.io.File
import scala.jdk.CollectionConverters._
import scala.util.Random

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
    val sealVersion = config.sealVersion.getOrElse(SEAL_VERSIONS.head)
    val openfheVersion = config.openfheVersion.getOrElse(OPENFHE_VERSIONS.head)
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
    val encParams = config.libConfigOpt match {
      case Some(libConfig) => libConfig.encParams
      case None            => config.encParams
    }
    val wordSizeOpt: Option[Int] = config.wordSize
    val libConfigOpt: Option[LibConfig] = config.libConfigOpt
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
          libConfigOpt,
        )
        val result = Execute(backend, config.timeLimit)
        print(result)
      case None =>
        val (ast, _, _) = Parse(fname)
        val result = Interp(ast, encParams.ringDim, encParams.plainMod)
        print(result)
    }
}

/** `compile` command */
case object CmdCompile extends Command("compile") {
  val help = "Compile a T2 file to the given backend."
  val examples = List(
    "fhetest compile -file:tmp.t2 -b:SEAL",
    "fhetest compile -file:tmp.t2 -b:OpenFHE",
  )
  def runJob(config: Config): Unit =
    val fname = config.fileName.getOrElseThrow("No T2 file given.")
    val backend = config.backend.getOrElseThrow("No backend given.")
    val encParams = config.libConfigOpt match {
      case Some(libConfig) => libConfig.encParams
      case None            => config.encParams
    }
    val libConfigOpt: Option[LibConfig] = config.libConfigOpt
    given DirName = getWorkspaceDir(backend)
    val (ast, symbolTable, encType) = Parse(fname)
    Print(
      ast,
      symbolTable,
      encType,
      backend,
      None,
      Some(encParams),
      libConfigOpt,
    )
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
    val output = Execute(backend, config.timeLimit)
    println(output)
}

/** `gen` command */
case object CmdGen extends Command("gen") {
  val help = "Generate random T2 programs."
  val examples = List(
    "fhetest gen -type:int -count:10",
    "fhetest gen -type:double -count:10",
    "fhetest gen -type:int -stg:exhaust -count:10",
    "fhetest gen -type:double -stg:random -count:10",
  )
  def runJob(config: Config): Unit =
    config.seed.foreach(Random.setSeed)
    val encType = config.encType.getOrElseThrow("No encType given.")
    val genCount = config.genCount.getOrElse(10)
    val generator = Generate(encType, Strategy.Random, config.validFilter)
    // generator.show(List(Backend.SEAL, Backend.OpenFHE), genCount, encType) // temp
    generator.show(List(Backend.OpenFHE), genCount, encType)
}

/** `check` command */
case object CmdCheck extends BackendCommand("check") {
  val help = "Check results of T2 program execution."
  val examples = List(
    "fhetest check -dir:tmp -json:true",
  )
  def runJob(config: Config): Unit =
    val dir = config.dirName.getOrElseThrow("No directory given.")
    val encParamsOpt = config.libConfigOpt.map(_.encParams)
    val backends = List(Backend.SEAL, Backend.OpenFHE)
    val toJson = config.toJson
    val sealVersion = config.sealVersion.getOrElse(SEAL_VERSIONS.head)
    val openfheVersion = config.openfheVersion.getOrElse(OPENFHE_VERSIONS.head)
    val outputs =
      Check(
        dir,
        backends,
        encParamsOpt,
        toJson,
        sealVersion,
        openfheVersion,
        config.timeLimit,
      )
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
    "fhetest test -type:int -stg:random -json:true -resultcount:10",
    "fhetest test -type:double -stg:exhaust -count:10",
    "fhetest test -type:double -stg:random -json:true -seal:4.1.2 -openfhe:1.4.2",
  )

  def runJob(config: Config): Unit =
    val startedTime = getCurrentTime()

    config.resultCount.foreach { count =>
      require(count > 0, "resultcount must be positive")
      require(config.validFilter && config.toJson, "resultcount requires valid checking and -json:true")
      require(config.genCount.isEmpty, "Use either count or resultcount, not both")
    }

    config.seed.foreach { seed =>
      Random.setSeed(seed)
      println(s"Random seed: $seed")
    }

    val encType = config.encType.getOrElseThrow("No encType given.")
    val genStrategy = config.genStrategy.getOrElse(Strategy.Random)
    val genCount = config.genCount
    val validFilter = config.validFilter
    val noFilterOpt = config.noFilterOpt
    val generator = Generate(encType, genStrategy, validFilter, noFilterOpt)
    var generatedCount = 0L
    val programs = generator(genCount).map { program =>
      generatedCount += 1
      println(s"FHETEST_GENERATED=$generatedCount")
      program
    }
    // val backendList = List(Backend.SEAL, Backend.OpenFHE) // temp
    val backendList = List(Backend.OpenFHE)
    val encParamsOpt = config.libConfigOpt.map(_.encParams)
    val toJson = config.toJson
    val sealVersion = config.sealVersion.getOrElse(SEAL_VERSIONS.head)
    val openfheVersion = config.openfheVersion.getOrElse(OPENFHE_VERSIONS.head)
    if (config.debug) {
      println(s"EncType : $encType")
      println(s"SEAL version : $sealVersion")
      println(s"OpenFHE version : $openfheVersion")
    }
    val outputs = Check(
      programs,
      backendList,
      encParamsOpt,
      toJson,
      sealVersion,
      openfheVersion,
      validFilter,
      config.debug,
      config.timeLimit,
    )
    val limitedOutputs = config.resultCount match {
      case Some(count) => outputs.take(count)
      case None        => outputs
    }
    var recordedCount = 0L
    for (program, output) <- limitedOutputs do {
      recordedCount += 1
      println("=" * 80)
      if !config.silent then {
        println("Program : " + program.content)
      }
      if config.debug then {
        println("-" * 80)
        println("LibConfig : " + program.libConfig.stringify())
        println("-" * 80)
      }
      println(output)
      println("-" * 80)
      val now = getCurrentTime()
      println(s"Current Time: $now")
      println("=" * 80)
    }
    println(s"FHETEST_RECORDED=$recordedCount")

    val endedTime = getCurrentTime()
    println("========== Time Report ==========")
    println(s"Started Time: $startedTime")
    println(s"Ended Time:   $endedTime")
    println("=================================")
}

case object CmdReplay extends Command("replay") {
  val help =
    "Replay the given json with specified backend (default: interpreter)."
  val examples = List(
    "fhetest replay -fromjson:logs/test/success/2.json",
    "fhetest replay -fromjson:logs/test/success/2.json -b:OpenFHE",
    "fhetest replay -fromjson:logs/test/success/2.json -b:OpenFHE -openfhe:1.4.2",
  )
  def runJob(config: Config): Unit =
    val jsonFileName = config.fromJson.getOrElseThrow("No json file given.")
    val resultInfo = DumpUtil.readResult(jsonFileName)
    val sealVersion = config.sealVersion.getOrElse(resultInfo.SEAL)
    val openfheVersion = config.openfheVersion.getOrElse(resultInfo.OpenFHE)
    if (config.debug) {
      println(s"SEAL version : $sealVersion")
      println(s"OpenFHE version : $openfheVersion")
    }
    updateBackendVersion(Backend.SEAL, sealVersion)
    updateBackendVersion(Backend.OpenFHE, openfheVersion)
    val t2Program = resultInfo.program
    val libConfig = t2Program.libConfig
    val encParams = libConfig.encParams
    config.backend match {
      case Some(backend) =>
        given DirName = getWorkspaceDir(backend)
        val (ast, symbolTable, encType) = Parse(t2Program)
        Print(
          ast,
          symbolTable,
          encType,
          backend,
          None,
          Some(encParams),
          Some(libConfig),
        )
        val result = Execute(backend, config.timeLimit)
        print(result)
      case None =>
        val (ast, _, _) = Parse(t2Program)
        val result = Interp(ast, encParams.ringDim, encParams.plainMod)
        print(result)
    }
}

/** Recheck archived valid programs without drawing new random programs. */
case object CmdRecheck extends BackendCommand("recheck") {
  val help = "Rerun archived valid programs with OpenFHE and compare stored classifications."
  val examples = List(
    "fhetest recheck -dir:logs/test-0212023152 -openfhe:1.4.2",
  )

  def runJob(config: Config): Unit = {
    val dir = Paths.get(config.dirName.getOrElseThrow("No archive directory given."))
    val version = config.openfheVersion.getOrElse(OPENFHE_VERSIONS.head)
    val files = List("succ", "fail", "psr_err").flatMap { subdir =>
      val path = dir.resolve(subdir)
      if (!Files.isDirectory(path)) List.empty
      else {
        val stream = Files.list(path)
        try stream.iterator().asScala.filter(p => p.toString.endsWith(".json")).toList
        finally stream.close()
      }
    }.sortBy(_.toString)
    require(files.nonEmpty, s"No archived valid-program JSON files found in $dir")
    config.genCount.foreach { expected =>
      require(files.size == expected, s"Expected $expected archives, found ${files.size}")
    }

    var changed = 0
    println("RECHECK,programId,old,new,file")
    files.foreach { path =>
      val info = DumpUtil.readResult(path.toString) match {
        case valid: ResultValidInfo => valid
        case _ => throw new IllegalArgumentException(s"Not a valid-program result: $path")
      }
      require(info.OpenFHE == version,
        s"Archive $path uses OpenFHE ${info.OpenFHE}, requested $version")
      val current = Check(info.program, List(Backend.OpenFHE), None, config.timeLimit) match {
        case Same(_) => "Success"
        case Diff(_, _) => "Fail"
        case ParserError(_) => "ParseError"
      }
      if (current != info.resultType) changed += 1
      println(s"RECHECK,${info.programId},${info.resultType},$current,$path")
    }
    println(s"RECHECK_SUMMARY,total=${files.size},changed=$changed")
    require(changed == 0, s"$changed archived classifications changed")
  }
}

// Deprecated: as reimplementing Checker for invalid program testing
// case object CmdCount extends Command("count") {
//   val help =
//     "Count the number of programs tested for each combination of valid filters"
//   val examples = List(
//     "fhetest count -dir:logs/test-invalid",
//   )
//   def runJob(config: Config): Unit =
//     val dirString = config.dirName.getOrElseThrow("No directory given.")
//     if (dirString contains "invalid") {
//       val dir = new File(dirString)
//       if (dir.exists() && dir.isDirectory) {
//         val numOfValidFilters =
//           classOf[ValidFilter].getDeclaredClasses.toList.filter { cls =>
//             classOf[ValidFilter]
//               .isAssignableFrom(cls) && cls != classOf[ValidFilter]
//           }.length
//         val allCombinations = (1 to numOfValidFilters).toList.flatMap(
//           combinations(_, numOfValidFilters),
//         )
//         var countMap: Map[List[Int], Int] =
//           allCombinations.foldLeft(Map.empty[List[Int], Int]) {
//             case (acc, comb) => acc + (comb -> 0)
//           }
//         val files = Files.list(Paths.get(dirString))
//         val fileList = files.iterator().asScala.toList
//         for {
//           filePath <- fileList
//           fileName = filePath.toString()
//         } yield {
//           val resultInfo = DumpUtil.readResult(fileName)
//           val t2Program = resultInfo.program
//           val invalidFilterIdxList = t2Program.invalidFilterIdxList
//           countMap = countMap.updatedWith(invalidFilterIdxList) {
//             case Some(cnt) => Some(cnt + 1)
//             case None      => Some(1) // unreachable
//           }
//         }
//         DumpUtil.dumpCount(dirString, countMap)
//       }
//     } else println("Directrory contains test cases of VALID programs")
// }
