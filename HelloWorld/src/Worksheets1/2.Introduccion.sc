def square(x: Int) = x * x
square(3)

def sumOfSquares(x: Int, y: Int) = square(x) + square(y)
sumOfSquares(4, 3)

def loop: Int = loop
def cp(x: Int, y: => Int) = x * x
cp(2 + 2, loop)

val x = 5
if (x < 10) 0 else 1

def abs(x: Int) = if (x < 0) -x else x

if (x > 0 && x < 10) square(x) else x

def and(x: Boolean, y: => Boolean) = if (x) y else false
def or(x: Boolean, y: => Boolean) = if (x) true else y