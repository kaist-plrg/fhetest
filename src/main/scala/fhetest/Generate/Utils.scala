package fhetest.Generate

import fhetest.Utils.*

object Utils {
  def assignValue(name: String, v: (Int | Double)): AbsStmt =
    Assign(name, v)
  def assignValues(name: String, vs: (List[Int] | List[Double])): AbsStmt =
    AssignVec(name, vs)

  // mCn: all combibation of length n from 0 to (m-1)
  def combinations(n: Int, m: Int): List[List[Int]] = {
    def combinations_helper(
      k: Int,
      start: Int,
      acc: List[Int],
    ): List[List[Int]] = {
      if (k == 0) List(acc.reverse)
      else {
        (start until m).flatMap { i =>
          combinations_helper(k - 1, i + 1, i :: acc)
        }.toList
      }
    }

    combinations_helper(n, 0, List.empty)
  }
}
