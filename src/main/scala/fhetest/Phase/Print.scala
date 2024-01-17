package fhetest.Phase

import fhetest.Utils.*
import org.twc.terminator.SymbolTable;
import org.twc.terminator.t2dsl_compiler.*;
import org.twc.terminator.t2dsl_compiler.T2DSLsyntaxtree.Goal;
import java.nio.file.{Files, Paths}

import java.io.*;

// TODO: Refactor optional arguments
case object Print {
  def apply(
    t2ast: Goal,
    symbolTable: SymbolTable,
    encType: ENC_TYPE,
    backend: Backend,
    wordSizeOpt: Option[Int] = None,
    encParamsOpt: Option[EncParams] = None,
  )(using workspaceDir: DirName): Unit = {
    val compiledDirPath = Paths.get(s"$workspaceDir/compiled")
    if (!Files.exists(compiledDirPath)) {
      Files.createDirectories(compiledDirPath)
    }
    val outputPath = s"$workspaceDir/compiled/test.cpp"
    val printer =
      createPrinter(backend, encType, symbolTable, wordSizeOpt, encParamsOpt)

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
    wordSizeOpt: Option[Int],
    encParamsOpt: Option[EncParams] = None,
  ) = {
    val wordSize = wordSizeOpt.getOrElse(0)
    val printer: T2_Compiler = (backend, encType) match {
      case (Backend.SEAL, ENC_TYPE.ENC_INT) =>
        new T2_2_SEAL(symbolTable, null, wordSize);
      case (Backend.SEAL, ENC_TYPE.ENC_DOUBLE) =>
        new T2_2_SEAL_CKKS(symbolTable, null);
      case (Backend.OpenFHE, ENC_TYPE.ENC_INT) =>
        new T2_2_OpenFHE(symbolTable, null, wordSize);
      case (Backend.OpenFHE, ENC_TYPE.ENC_DOUBLE) =>
        new T2_2_OpenFHE_CKKS(symbolTable, null);
      case _ =>
        throw new Error(s"Unknown backend: $backend")
    }
    encParamsOpt match
      case Some(EncParams(ringDim, mulDepth, plainMod)) =>
        printer.setEncParams(ringDim, mulDepth, plainMod)
      case None =>
    printer
  }
}
