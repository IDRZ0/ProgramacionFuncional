def join(op: (Int, Int) => Int)(f: Int => Int, g: Int => Int)(x: Int): Int = op(f(x), g(x))

def f(x: Int): Int = 2 * x
def g(x: Int): Int = x + 4
def h: Int => Int = join(_ + _)(f, g)

h(2)

def join2(op: (Int, Int) => Int)(f: Int => Int, g: Int => Int): Int => Int =
  (x: Int) => op(f(x), g(x))

def join3(op: (Int, Int) => Int): (Int => Int, Int => Int) => (Int => Int) =
  (f: Int => Int, g: Int => Int) => (x: Int) => op(f(x), g(x))

def join4: ((Int, Int) => Int) => ((Int => Int, Int => Int) => (Int => Int)) =
  (op: (Int, Int) => Int) => (f: Int => Int, g: Int => Int) => ((x: Int) => op(f(x), g(x)))

def comp(f: Int => Int, g: Int => Int)(x: Int): Int = f(g(x))
def comp1(f: Int => Int)(g: Int => Int)(x: Int): Int = f(g(x))
def comp2: (Int => Int) => (Int => Int) => Int => Int = (f: Int => Int) => (g: Int => Int) => (x: Int) => f(g(x))

def f1(x: Int) = 2 * x
def g1(x: Int) = x + 4

def h1: Int => Int = comp(f1, g1)
h1(1)

def i(x: Int) = x + 10
def j(x: Int) = x * x - x + 2
def k: Int => Int = join(_ + _)(comp(j, i), j)
def l: Int => Int = comp(i, k)