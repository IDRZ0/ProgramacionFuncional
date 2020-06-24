def sumInts(a: Int, b: Int): Int = if (a < b) a + sumInts(a + 1, b) else b
sumInts(4, 9)

def sumInt(a: Int, b: Int): Int = {
  def inner(a: Int, c: Int): Int = if (a > b) c else inner(a + 1, c + a)

  inner(a, 0)
}

def cubes(x: Int) = x * x * x
def fact(x: Int): Int = if (x == 0) 1 else x * fact(x - 1)
def id(x: Int) = x

//factorizacion de codigos
def sumF(f: Int => Int, a: Int, b: Int) = {
  def inner(i: Int, ac: Int): Int = if (i > b) ac else inner(i + 1, ac + f(i))

  inner(a, 0)
}

def sumCubes2(a: Int, b: Int) = sumF(cubes, a, b)
def sumFact2(a: Int, b: Int) = sumF(fact, a, b)
def sum2(a: Int, b: Int) = sumF(id, a, b)


//Productos
def prodInts(a: Int, b: Int): Int = if (a < b) a * prodInts(a + 1, b) else b
prodInts(1, 4)

def prodInt(a: Int, b: Int): Int = {
  def inner(a: Int, c: Int): Int = if (a > b) c else inner(a + 1, c * a)

  inner(a, 1)
}
prodInt(3, 4)

def prodF(f: Int => Int, a: Int, b: Int): Int = {
  def inner(a: Int, c: Int): Int = if (a > b) c else inner(a + 1, c * f(a))

  inner(a, 1)
}

def factorial(n: Int) = prodF(id, 1, n)
factorial(5)

def superF(a: Int, b: Int, c: Int, f: Int => Int, op: (Int, Int) => Int) = {
  def inner(i: Int, ac: Int): Int = if (i > b) ac else inner(i + 1, op(ac, f(i)))

  inner(a, c)
}

//def mult(a: Int, b: Int) = a * b
// funcion con funciones anonimas
def factorial2(x: Int) = superF(1, x, 1, (n: Int) => n, (a: Int, b: Int) => a * b)
def factorial2(x: Int) = superF(1, x, 1, (_), (_ * _)) //placeholder notation, solo podemos utilizar cada parametro una vez