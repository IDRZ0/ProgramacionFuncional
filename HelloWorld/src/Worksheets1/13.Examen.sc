def sum(a: Int, b: Int): Int = a + b
def sumC(a: Int)(b: Int): Int = a + b //Int
def sumC1(a: Int): Int => Int = (b: Int) => a + b //Funcion que toma int y devuelve int
def sumC2: Int => (Int => Int) = (a: Int) => (b: Int) => a + b //Funcion que toma int y devuelve otra funcion, que toma un int y devuelve int

type MyList = Int => Int
type Set = Int => Boolean

def get(l: MyList, i: Int): Int = l(i)

def isDefined(l: MyList, i: Int): Boolean =
  try {
    l(i)
    true
  } catch {
    case e: IndexOutOfBoundsException => false
  }

def A: MyList = (x: Int) => 0

def length(l: MyList): Int = {
  def inner(i: Int): Int =
    if (!isDefined(l, i))
      i
    else
      inner(i + 1)

  inner(0)
}

def contains(l: MyList, x: Int): Boolean = {
  def inner(i: Int): Boolean =
    if (i == length(l))
      false
    else if (x == get(l, i))
      true
    else
      inner(i + 1)

  inner(0)
}

def contains2(l: MyList, x: Int): Boolean = {
  def inner(i: Int): Boolean =
    (i != length(l)) && ((x == get(l, i)) || inner(i + 1))

  inner(0)
}

type MultiSet = Int => Int
type Set = Int => Boolean

def a(m: Int, n: Int): Int =
  if (m == 0)
    n + 1
  else if (m > 0 && n == 0)
    a(m - 1, 1)
  else
    a(m - 1, a(m, n - 1))