package fhetest.Phase

import fhetest.Utils.*
import java.nio.file.{Files, Paths}

import org.twc.terminator.t2dsl_compiler.{SymbolTableVisitor, TypeCheckVisitor};
import org.twc.terminator.t2dsl_compiler.T2DSLparser.ParseException;
import org.twc.terminator.t2dsl_compiler.T2DSLparser.T2DSLParser;
import org.twc.terminator.t2dsl_compiler.T2DSLsyntaxtree.Goal;
import org.twc.terminator.SymbolTable;

import java.io.*;
import fhetest.Generate.T2Program

case object Parse {
  def apply(file: String): (Goal, SymbolTable, ENC_TYPE) = {
    val input_stream: InputStream = new FileInputStream(file);
    apply(input_stream)
  }

  def apply(t2Program: T2Program): (Goal, SymbolTable, ENC_TYPE) = {
    val input_stream: InputStream = new ByteArrayInputStream(
      t2Program.content.getBytes(),
    );
    apply(input_stream)
  }

  def apply(input_stream: InputStream): (Goal, SymbolTable, ENC_TYPE) = {
    // TODO: Refactor this ENC_TYPE
    val t2ast = T2DSLParser(input_stream).Goal()
    val symtable_visit = new SymbolTableVisitor()
    t2ast.accept(symtable_visit)
    val symbol_table = symtable_visit.getSymbolTable()
    val type_checker = new TypeCheckVisitor(symbol_table)
    t2ast.accept(type_checker);
    val enc_type: ENC_TYPE = translateT2EncType(type_checker.getScheme())
    (t2ast, symbol_table, enc_type)
  }
}
