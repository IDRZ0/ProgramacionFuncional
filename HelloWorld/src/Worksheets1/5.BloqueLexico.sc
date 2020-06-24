//Bloque lexico
{
  def f(x: Int) = x + 2

  f(2)
}

val x = 3
val y = 4

val z = {
  def f(x: Int) = x + y

  val x = 5
  f(x + 1)
} + x

//All Newton con bloques lexicos e inner functions
def newton(x: Double) = {
  def inner(a: Double): Double = {
    def mejorar(a: Double) = (a + x / a) / 2

    def buenaAprox(a: Double) = {
      def abs(x: Double) = if (x < 0) -x else x

      abs(x - a * a) < 0.0001 * x
    }

    if (buenaAprox(a)) a else inner(mejorar(a))
  }

  inner(x / 2)
}

newton(4)