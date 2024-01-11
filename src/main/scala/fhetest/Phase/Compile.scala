package fhetest.Phase

import fhetest.Utils.*
import org.twc.terminator.Main

case object Compile {
  def apply(file: String, backend: String): Unit = {
    val workspaceDir = getWorkspaceDir(backend)
    val outputPath = s"$workspaceDir/compiled/test.cpp"
    Main.main(Array(file, s"--$backend", "-o", outputPath))
  }
}
