def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
gcd(105, 75)

def factorial(n: Int): Int = if (n == 0) 1 else n * factorial(n - 1)

def factTR(n: Int) = {
  def inner(n: Int, ac: Int): Int =
    if (n == 0)
      ac
    else
      inner(n - 1, ac * n)

  inner(n, 1)
}
factorial(4)
factTR(4)

def fibo(n: Int): Int =
  if (n < 2)
    1
  else
    fibo(n - 1) + fibo(n - 2)

def fiboTRB(n: Int) = {
  def fiboTR(i: Int, actual: Int, anterior: Int): Int =
    if (i == n)
      actual
    else
      fiboTR(i + 1, actual + anterior, actual)

  if (n < 2) 1 else fiboTR(1, 1, 1)
}
fibo(4)
fiboTRB(3)