package fhetest.Phase

import sys.process.*
import java.io.File

import fhetest.Utils.*

case object Execute {
  def apply(backend: Backend)(using workspaceDir: DirName): String = {
    val binPath = s"$workspaceDir/bin"
    val cmakeCommand = "cmake ."
    val makeCommand = "make -j"
    val executeCommand = "./test.out"

    // TODO : Add option silent (default true)
    val cmakeProcess =
      Process(cmakeCommand, new File(workspaceDir))
    val makeProcess =
      Process(makeCommand, new File(workspaceDir))
    val executeProcess =
      Process(executeCommand, new File(binPath))
    cmakeProcess.!(silentLogger)
    makeProcess.!(silentLogger)
    try {
      val executeResult = executeProcess.!!
      executeResult // 성공적으로 완료된 경우, 결과 반환
    } catch {
      case e: Exception => s"Execution failed: ${e.getMessage}"
    }
  }
}
