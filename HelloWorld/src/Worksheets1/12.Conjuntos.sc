//funcion caracteristica: f:N -> {true, false}
type Set = Int => Boolean
def c1(x: Int): Boolean = x >= 1 && x <= 3
def c11: Set = (x: Int) => x >= 1 && x <= 3
c1(1)
c1(2)
c1(3)
c1(4)

def c2(x: Int): Boolean = x % 2 == 0
def c22: Set = (x: Int) => x % 2 == 0
c2(1)
c2(2)
c2(3)
c2(4)

def singletonSet(elem: Int): Set = (x: Int) => x == elem
def c3 = singletonSet(3)
c3(1)
c3(2)
c3(3)
c3(4)

def contains(s: Set, elem: Int): Boolean = s(elem)
def union(s: Set, t: Set): Set = (x: Int) => s(x) || t(x)
def intersect(s: Set, t: Set): Set = (x: Int) => s(x) && t(x)
def diff(s: Set, t: Set): Set = (x: Int) => s(x) && !t(x)
def filter(s: Set, p: Int => Boolean): Set = (x: Int) => s(x) && p(x)

def forall(s: Set, p: Int => Boolean): Boolean = {
  def inner(i: Int): Boolean =
    if (i > 1000)
      false
    else if (!s(i) || p(i))
      inner(i + 1)
    else
      false

  inner(-1000)
}

def forall2(s: Set, p: Int => Boolean): Boolean = {
  def inner(i: Int): Boolean =
    i > 1000 || (!diff(s, p)(i) && inner(i + 1))

  inner(-1000)
}

def exists(s: Set, p: Int => Boolean): Boolean = {
  def inner(i: Int): Boolean =
    i > 1000 && ((s(i) && p(i)) || inner(i + 1))

  inner(-1000)
}

def map(s: Set, f: Int => Int): Set =
  (x: Int) => exists(s, (e: Int) => f(e) == x)