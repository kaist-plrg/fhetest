package fhetest.Phase

import fhetest.Utils.*
import org.twc.terminator.SymbolTable;
import org.twc.terminator.t2dsl_compiler.*;
import org.twc.terminator.t2dsl_compiler.T2DSLsyntaxtree.Goal;
import java.nio.file.{Files, Paths}

import java.io.*;

case object Print {
  def apply(
    t2ast: Goal,
    symbolTable: SymbolTable,
    encType: ENC_TYPE,
    backend: Backend,
  ): Unit = {
    val workspaceDir = getWorkspaceDir(backend)
    val compiledDirPath = Paths.get(s"$workspaceDir/compiled")
    if (!Files.exists(compiledDirPath)) {
      Files.createDirectories(compiledDirPath)
    }
    val outputPath = s"$workspaceDir/compiled/test.cpp"
    val printer = createPrinter(backend, encType, symbolTable)

    t2ast.accept(printer)
    val code = printer.get_asm();
    val writer = new PrintWriter(outputPath);
    try {
      writer.print(code);
    } finally {
      writer.close();
    }
  }

  def createPrinter(
    backend: Backend,
    encType: ENC_TYPE,
    symbolTable: SymbolTable,
  ) = (backend, encType) match {
    case (Backend.SEAL, ENC_TYPE.ENC_INT) =>
      new T2_2_SEAL(symbolTable, null, 0, 16384);
    case (Backend.SEAL, ENC_TYPE.ENC_DOUBLE) =>
      new T2_2_SEAL_CKKS(symbolTable, null, 16384);
    case (Backend.OpenFHE, ENC_TYPE.ENC_INT) =>
      new T2_2_OpenFHE(symbolTable, null, 0, 16384);
    case (Backend.OpenFHE, ENC_TYPE.ENC_DOUBLE) =>
      new T2_2_OpenFHE_CKKS(symbolTable, null, 16384);
    case _ =>
      throw new Error(s"Unknown backend: $backend")
  }
}
