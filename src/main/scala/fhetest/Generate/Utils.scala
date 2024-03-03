package fhetest.Generate

import fhetest.Utils.*

object Utils {
  def assignValue(name: String, v: (Int | Double)): AbsStmt =
    Assign(name, v)
  def assignValues(name: String, vs: (List[Int] | List[Double])): AbsStmt =
    AssignVec(name, vs)
}
