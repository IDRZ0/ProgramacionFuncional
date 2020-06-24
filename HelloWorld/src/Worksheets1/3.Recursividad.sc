def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
gcd(105, 75)

def factorial(n: Int): Int = if (n == 0) 1 else n * factorial(n - 1)
factorial(4)

def facTR(n: Int, ac: Int): Int = if (n == 0) ac else facTR(n - 1, ac * n)
facTR(4, 1)

def fibo(n: Int): Int = if (n < 2) 1 else fibo(n - 1) + fibo(n - 2)

def fiboTR(n: Int, i: Int, actual: Int, anterior: Int): Int =
  if (i == n)
    actual
  else
    fiboTR(n, i + 1, actual + anterior, actual)

def fiboTRB(n: Int) = if (n < 2) 1 else fiboTR(n, 1, 1, 1)
fiboTRB(5)