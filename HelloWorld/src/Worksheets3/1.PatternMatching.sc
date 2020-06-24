abstract class Expr

case class Number(i: Int) extends Expr

case class Sum(left: Expr, right: Expr) extends Expr

case class Prod(left: Expr, right: Expr) extends Expr

def eval(e: Expr): Int = e match {
  case Number(i) => i
  case Sum(left, right) => eval(left) + eval(right)
  case Prod(left, right) => eval(left) * eval(right)
}

def show(e: Expr): String = {
  def inner(e: Expr, i: Int): String =
    if (i == 0)
      e match {
        case Number(i) => i.toString
        case Sum(left, right) => inner(left, i + 1) + " + " + inner(right, i + 1)
        case Prod(left, right) => inner(left, i + 1) + " * " + inner(right, i + 1)
      }
    else
      e match {
        case Number(i) => i.toString
        case Sum(left, right) => "(" + inner(left, i + 1) + " + " + inner(right, i + 1) + ")"
        case Prod(left, right) => inner(left, i + 1) + " * " + inner(right, i + 1)
      }

  inner(e, 0)
}
show(Prod(Number(5), Prod(Sum(Number(2), Number(2)), Number(2))))

//Tuplas
val a = (5, "Hola", true)
a._1
a match {
  case (x, y, z) => y
}
a match {
  case (_, _, z) => z
}