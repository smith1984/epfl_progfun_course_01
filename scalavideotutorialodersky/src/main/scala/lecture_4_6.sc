import exprs.show

trait Expr
case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr

object exprs{
  def show(e: Expr): String = e match
  {
    case Number(x) => x.toString
    case Sum(x1, x2) => show(x1) + "+" + show(x2)
  }
}
show(Number(1))
show(Sum(Number(1),Number(3)))