package fhetest.Generate

trait AbsStmt
case class Var()
case class Assign(l: String, r: (Int | Double)) extends AbsStmt
case class AssignVec(l: String, r: (List[Int] | List[Double])) extends AbsStmt
case class Add(l: Var, r: Var) extends AbsStmt
case class Sub(l: Var, r: Var) extends AbsStmt
case class Mul(l: Var, r: Var) extends AbsStmt
case class Rot(l: Var, r: Var) extends AbsStmt

val V = Var()

def formatNumber(n: Int | Double): String = n match {
  case i: Int    => i.toString
  case d: Double => f"$d%f"
}

def allAbsStmts: LazyList[AbsStmt] = LazyList(
  Add(V, V),
  Sub(V, V),
  Mul(V, V),
  Rot(V, V),
)
