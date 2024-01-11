package fhetest.Phase

import fhetest.Utils.*

case object Execute {
  def apply(backend: String): String = {
    val workspaceDir = getWorkspaceDir(backend)
    val binPath = s"$workspaceDir/bin"
    val makeCommand = "make -j"
    val executeCommand = "./test.out"
    val makeProcess =
      sys.process.Process(makeCommand, new java.io.File(workspaceDir))
    val executeProcess =
      sys.process.Process(executeCommand, new java.io.File(binPath))
    makeProcess.!
    executeProcess.!!
  }
}
