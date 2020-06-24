class Fraction(n: Int, d: Int) {
  require(d > 0)
  assert(d > 0)

  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int =
    if (b == 0)
      a
    else
      gcd(b, a % b)

  private val g = gcd(n, d)
  val num = n / g
  val den = d / g

  def +(f: Fraction) = new Fraction(num * f.den + f.num * den, den * f.den)

  def *(f: Fraction) = new Fraction(num * f.num, den * f.den)

  def unary_- = new Fraction(-num, den)

  def unary_~ : Fraction = if (num < 0) new Fraction(-den, -num) else new Fraction(den, num)

  def -(f: Fraction) = new Fraction(num * f.den - f.num * den, den * f.den)

  def /(f: Fraction) = new Fraction(num * f.den, den * f.num)

  def ==(f: Fraction): Boolean = num == f.num && den == f.den

  def <(f: Fraction): Boolean = num * f.den < f.num * den

  def <=(f: Fraction): Boolean = num * f.den <= f.num * den

  def >(f: Fraction): Boolean = num * f.den > f.num * den

  def >=(f: Fraction): Boolean = num * f.den >= f.num * den

  @Override
  override def toString: String = num + (if (den > 1) "/" + den else "")
}

implicit def intToFraction(x: Int) = new Fraction(x)

val a = new Fraction(2)
val b = new Fraction(3)
val c = new Fraction(4)
val d = new Fraction(2, 3)
a.unary_-
-a
~a
a * b + c
a.*(b).+(c)
c + a * b
c.+(a).*(b)
d + a
d + 2