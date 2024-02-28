package fhetest.Generate

import fhetest.Utils.*
import scala.util.Random
import fhetest.Utils.*

object Utils {
  def assignValue(name: String, v: (Int | Double)): AbsStmt =
    Assign(name, v)
  def assignValues(name: String, vs: (List[Int] | List[Double])): AbsStmt =
    AssignVec(name, vs)

  extension (s: AbsStmt)
    def stringify(encType: ENC_TYPE): String = encType match {
      case ENC_TYPE.ENC_INT =>
        s match {
          case Assign(l, r) => s"$l = ${formatNumber(r)};"
          case AssignVec(l, r) =>
            s"$l = {${r.map(formatNumber).mkString(",")}};"
          case Add(_, _)  => "x += y;"
          case AddP(_, _) => "x += yP;"
          case Sub(_, _)  => "x -= y;"
          case SubP(_, _) => "x -= yP;"
          case Mul(_, _)  => "x *= y;"
          case MulP(_, _) => "x *= yP;"
          case Rot(_, _)  => "rotate_left(x, c);"
        }
      case ENC_TYPE.ENC_DOUBLE =>
        s match {
          case Assign(l, r) => s"$l = ${formatNumber(r)};"
          case AssignVec(l, r) =>
            s"$l = {${r.map(formatNumber).mkString(",")}};"
          case Add(_, _)  => "match_params(y, x);x += y;"
          case AddP(_, _) => "match_params(x, x);x += yP;"
          case Sub(_, _)  => "match_params(y, x);x -= y;"
          case SubP(_, _) => "match_params(x, x);x -= yP;"
          case Mul(_, _)  => "match_params(y, x);x *= y;reduce_noise(x);"
          case MulP(_, _) => "match_params(x, x);x *= yP;reduce_noise(x);"
          case Rot(_, _)  => "rotate_left(x, c);"
        }
    }

  extension (t: Template)
    def stringify: String = t.map(_.stringify).mkString("")
    def getMulDepth: Int = t.count {
      case Mul(_, _) => true; case _ => false
    }

    def assignRandValues(len: Int, bound: (Int | Double)): Template = {
      def lx() = Random.between(1, len + 1)
      def ly() = Random.between(1, len + 1)

      // Generate Random Values
      def vxs(): (List[Int] | List[Double]) = bound match {
        case intBound: Int => List.fill(lx())(Random.between(0, intBound))
        case doubleBound: Double =>
          List.fill(lx())(Random.between(0.0, doubleBound))
      }
      def vys(): (List[Int] | List[Double]) = bound match {
        case intBound: Int => List.fill(ly())(Random.between(0, intBound))
        case doubleBound: Double =>
          List.fill(ly())(Random.between(0.0, doubleBound))
      }
      def vyP() = bound match {
        case intBound: Int       => Random.between(0, intBound)
        case doubleBound: Double => Random.between(0.0, doubleBound)
      }

      val assigned = assignValues("x", vxs()) :: t
      assigned.flatMap {
        case op @ (Add(_, _) | Sub(_, _) | Mul(_, _)) =>
          assignValues("y", vys()) :: op :: Nil
        case op @ (AddP(_, _) | SubP(_, _) | MulP(_, _)) =>
          assignValue("yP", vyP()) :: op :: Nil
        case op @ Rot(_, _) =>
          Assign("c", Random.between(0, 10)) :: op :: Nil
        case s => s :: Nil
      }
    }
}
