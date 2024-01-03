import java.nio.file.{Paths, Files}
import scala.sys.process._
val scala3Version = "3.3.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "FHETest",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
  )

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
