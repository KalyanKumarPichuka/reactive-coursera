package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.map(
      t => (t._1, Signal(resolve(t._2())(namedExpressions.filterNot(_._1 == t._1), t._1)))
    )
  }

  def resolve(exp: Expr)(implicit namedExpressions: Map[String, Signal[Expr]], s: String): Double =
      exp match {
        case Literal(v) => v
        case Plus(a, b) => resolve(a) + resolve(b)
        case Minus(a, b) => resolve(a) - resolve(b)
        case Times(a, b) => resolve(a) * resolve(b)
        case Divide(a, b) =>
          if (resolve(b) == 0) Double.NaN
          else resolve(a) / resolve(b)
        case Ref(n) => resolve(getReferenceExpr(n))(namedExpressions.filterNot(_._1 == n), s)
        case _ => Double.NaN
      }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String)(implicit references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}

