package fhetest.Generate

import scala.util.Random

object Utils {
  def assignValues(name: String, vxs: (List[Int] | List[Double])): AbsStmt =
    AssignVec(name, vxs)

  extension (s: AbsStmt)
    def stringify: String = s match
      case Assign(l, r)    => s"$l = ${formatNumber(r)};"
      case AssignVec(l, r) => s"$l = {${r.map(formatNumber).mkString(",")}};"
      case Add(l, r)       => "x += y;"
      case Sub(l, r)       => "x -= y;"
      case Mul(l, r)       => "x *= y;"
      case Rot(l, r)       => "rotate_left(x, c);"

  extension (t: Template)
    def stringify: String = t.map(_.stringify).mkString("")
    def getMulDepth: Int = t.count {
      case Mul(_, _) => true; case _ => false
    }

    def assignRandValues(len: Int, bound: (Int | Double)): Template = {
      val lx = Random.between(1, len + 1)
      val ly = Random.between(1, len + 1)
      val vxs: (List[Int] | List[Double]) = bound match {
        case intBound: Int => List.fill(lx)(Random.between(0, intBound))
        case doubleBound: Double =>
          List.fill(lx)(Random.between(0.0, doubleBound))
      }
      val vys: (List[Int] | List[Double]) = bound match {
        case intBound: Int => List.fill(ly)(Random.between(0, intBound))
        case doubleBound: Double =>
          List.fill(ly)(Random.between(0.0, doubleBound))
      }

      val assigned = assignValues("x", vxs) :: t
      assigned.flatMap {
        case op @ (Add(_, _) | Sub(_, _) | Mul(_, _)) =>
          assignValues("y", vys) :: op :: Nil
        case op @ Rot(_, _) =>
          Assign("c", Random.between(0, 10)) :: op :: Nil
        case s => s :: Nil
      }
    }
}
