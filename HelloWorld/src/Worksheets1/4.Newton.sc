def abs(x: Double) = if (x < 0) -x else x

def buenaAprox(x: Double, a: Double) = abs(x - a * a) < 0.0001 * x

def mejorar(x: Double, a: Double) = (a + x / a) / 2

def newtonRec(x: Double, a: Double): Double =
  if (buenaAprox(x, a))
    a
  else
    newtonRec(x, mejorar(x, a))

def newton(x: Double) = newtonRec(x, x / 2)

newton(1e-20)