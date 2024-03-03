package fhetest.Generate

import fhetest.Utils.*

sealed trait AbsStmt {
  def stringify(): String
}
case class Var()
case class Assign(l: String, r: (Int | Double)) extends AbsStmt {
  def stringify(): String = s"$l = ${formatNumber(r)};"
}
case class AssignVec(l: String, r: (List[Int] | List[Double])) extends AbsStmt {
  def stringify(): String = s"$l = {${r.map(formatNumber).mkString(",")}};"
}
case class Add(l: Var, r: Var) extends AbsStmt {
  def stringify(): String = "x += y;"
}
case class AddP(l: Var, r: Var) extends AbsStmt {
  def stringify(): String = "x += yP;"
}
case class Sub(l: Var, r: Var) extends AbsStmt {
  def stringify(): String = "x -= y;"
}
case class SubP(l: Var, r: Var) extends AbsStmt {
  def stringify(): String = "x -= yP;"
}
case class Mul(l: Var, r: Var) extends AbsStmt {
  def stringify(): String = "x *= y;"
}
case class MulP(l: Var, r: Var) extends AbsStmt {
  def stringify(): String = "x *= yP;"
}
case class Rot(l: Var, r: Var) extends AbsStmt {
  def stringify(): String = "rotate_left(x, c);"
}
case class Rescale(v: Var) extends AbsStmt {
  def stringify(): String = "reduce_noise(x);"
}
case class MatchParams1(v: Var) extends AbsStmt {
  def stringify(): String = "match_params(x, x);"
}
case class MatchParams2(l: Var, r: Var) extends AbsStmt {
  def stringify(): String = "match_params(y, x);"
}

val V = Var()

def allAbsStmts: LazyList[AbsStmt] = LazyList(
  Add(V, V),
  AddP(V, V),
  Sub(V, V),
  SubP(V, V),
  Mul(V, V),
  MulP(V, V),
  Rot(V, V),
)
