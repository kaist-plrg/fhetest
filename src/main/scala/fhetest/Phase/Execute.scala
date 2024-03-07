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

    // remove bin directory if it exists
    val binDir = new File(binPath)
    if (binDir.exists()) {
      deleteDirectoryRecursively(binDir)
    }
    // TODO : Add option silent (default true)
    val cmakeProcess =
      Process(cmakeCommand, new File(workspaceDir))
    val makeProcess =
      Process(makeCommand, new File(workspaceDir))
    val executeProcess =
      Process(executeCommand, binDir)

    val outputSB = new StringBuilder() // To capture standard output
    val errorSB = new StringBuilder() // To capture error output

    // Custom ProcessLogger to append output and errors
    val processLogger = ProcessLogger(
      (o: String) => outputSB.append(o).append("\n"),
      (e: String) => errorSB.append(e).append("\n"),
    )
    val silentLogger = ProcessLogger(_ => (), errorSB.append(_))

    cmakeProcess.!(silentLogger)
    val makeExitCode = makeProcess.!(silentLogger)

    if (makeExitCode == 0) {
      // Only proceed if make was successful
      val executeExitCode = executeProcess.!(processLogger)
      if (executeExitCode == 0) {
        // If make and execute were successful, return standard output
        return outputSB.toString()
      } else if (executeExitCode == 139) {
        // If program terminated with segmentation fault, return error message
        errorSB.append(
          "Program terminated with segmentation fault (exit code 139).\n",
        )
        return errorSB.toString()
      } else if (executeExitCode == 136) {
        // If program terminated with segmentation fault, return error message
        errorSB.append(
          "Program terminated with floating point exception (exit code 136).\n",
        )
        return errorSB.toString()
      } else {
        // If execute failed, append error message
        return errorSB.toString()
      }
    } else {
      // If make failed, append error message
      return errorSB.toString()
    }
    outputSB.toString()
  }
}
