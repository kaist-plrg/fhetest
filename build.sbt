import sbtassembly.AssemblyPlugin.defaultUniversalScript

import java.nio.file.{Paths, Files}
import scala.sys.process._
val scala3Version = "3.3.1"

// Scala options
ThisBuild / scalacOptions := Seq(
  "-language:implicitConversions", // allow implicit conversions
  "-deprecation", // emit warning and location for usages of deprecated APIs
  "-explain", // explain errors in more detail
  "-explain-types", // explain type errors in more detail
  "-feature", // emit warning for features that should be imported explicitly
  "-unchecked" // enable warnings where generated code depends on assumptions
)

// Java options
ThisBuild / javacOptions ++= Seq(
  "-encoding",
  "UTF-8"
)

// Java options for assembly
lazy val assemblyJavaOpts = Seq(
  "-Xms1g",
  "-Xmx3g",
  "-XX:ReservedCodeCacheSize=512m",
  "-Dfile.encoding=utf8"
)

// assembly setting
ThisBuild / assemblyPrependShellScript := Some(
  assemblyJavaOpts.map("JAVA_OPTS=\"" + _ + " $JAVA_OPTS\"") ++
    defaultUniversalScript(shebang = false)
)

// automatic reload build.sbt
Global / onChangedBuildSource := ReloadOnSourceChanges

// Metals requires the semanticdb compiler plugin
Global / semanticdbEnabled := true

val buildFHElibs = taskKey[Unit]("Build FHE libraries")

buildFHElibs := {
  val s: TaskStreams = streams.value
  val log = s.log

  val baseDir = baseDirectory.value.getAbsolutePath
  val T2baseDir = s"$baseDir/src/main/java/T2-FHE-Compiler-and-Benchmarks"

  val processLogger = new ProcessLogger {
    def out(s: => String): Unit = log.info(s)
    def err(s: => String): Unit = log.error(s)
    def buffer[T](f: => T): T = f
  }

  log.info("Clone FHE libraries")
  val cloneScript = s"$T2baseDir/.circleci/clone_libs.sh"
  Process(cloneScript, new File(T2baseDir)).!(processLogger)

  log.info("Build FHE libraries")
  val buildScript = s"$T2baseDir/.circleci/build_libs.sh"
  Process(buildScript, new File(T2baseDir)).!(processLogger)
}

val mvnInitializePackage =
  taskKey[Unit]("Initializes and packages the Maven project")

mvnInitializePackage := {
  val s: TaskStreams = streams.value
  val log = s.log

  val baseDir = baseDirectory.value.getAbsolutePath
  val T2baseDir = s"$baseDir/src/main/java/T2-FHE-Compiler-and-Benchmarks"

  log.info("Running mvn initialize and package")
  Process("mvn package -Dmaven.test.skip", new File(T2baseDir)).!
}

Compile / sourceGenerators += Def.task {
  val file =
    (Compile / sourceManaged).value / "settings" / "ProjectSettings.scala"
  val content =
    s"""
       |package settings
       |
       |object ProjectSettings {
       |  val projectHome = "${(ThisBuild / baseDirectory).value.getAbsolutePath
        .replace("\\", "\\\\")}"
       |}
       """.stripMargin
  IO.write(file, content)
  Seq(file)
}.taskValue

Compile / unmanagedSourceDirectories := {
  (Compile / scalaSource).value :: (Compile / resourceDirectory).value :: Nil
}

lazy val root = project
  .in(file("."))
  .settings(
    name := "FHETest",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    // set the main class for 'sbt run'
    Compile / mainClass := Some("fhetest.FHETest"),
    // assembly setting
    assembly / test := {},
    assembly / assemblyOutputPath := file("bin/fhetest")
  )
