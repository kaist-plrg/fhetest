package fhetest.Phase

import fhetest.Utils.*
import org.twc.terminator.Main
import java.nio.file.{Files, Paths}

case object Compile {
  def apply(file: String, backend: String): Unit = {
    val workspaceDir = getWorkspaceDir(backend)
    val compiledDirPath = Paths.get(s"$workspaceDir/compiled")
    if (!Files.exists(compiledDirPath)) {
      Files.createDirectories(compiledDirPath)
    }
    val outputPath = s"$workspaceDir/compiled/test.cpp"
    Main.main(Array(file, s"--$backend", "-o", outputPath))
  }
}
