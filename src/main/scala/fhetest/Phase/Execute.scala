package fhetest.Phase

import fhetest.Utils.*

case object Execute {
  def apply(backend: Backend)(using workspaceDir: DirName): String = {
    val binPath = s"$workspaceDir/bin"
    val cmakeCommand = "cmake ."
    val makeCommand = "make -j"
    val executeCommand = "./test.out"
    val cmakeProcess =
      sys.process.Process(cmakeCommand, new java.io.File(workspaceDir))
    val makeProcess =
      sys.process.Process(makeCommand, new java.io.File(workspaceDir))
    val executeProcess =
      sys.process.Process(executeCommand, new java.io.File(binPath))
    cmakeProcess.!
    makeProcess.!
    executeProcess.!!
  }
}
