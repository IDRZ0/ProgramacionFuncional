abstract class IntSet {
  def add(x: Int): IntSet

  def contains(x: Int): Boolean

  def union(o: IntSet): IntSet

  //def apply[T <: IntSet](f: Int => Int): T
}

class NonEmptyIntSet(val h: Int, val left: IntSet, val right: IntSet) extends IntSet {
  def contains(x: Int): Boolean =
    if (x == h)
      true
    else if (x < h)
      left contains x
    else
      right contains x

  // def contains2(x: Int): Boolean = (h == x) || (x < h && left.contains(x) || (x > h && right.contains2(x))

  def add(x: Int): IntSet =
    if (h == x)
      this
    else if (x < h)
      new NonEmptyIntSet(h, left add x, right)
    else
      new NonEmptyIntSet(h, left, right add x)

  def union(o: IntSet): IntSet =
    (left union right) union o add h

  //def apply[NonEmptyIntSet](f: Int => Int): NonEmptyIntSet = new NonEmptyIntSet(f(h), left.apply(f), right.apply(f))
}

object EmptyIntSet extends IntSet {
  def contains(x: Int): Boolean = false

  def add(x: Int): IntSet = new NonEmptyIntSet(x, EmptyIntSet, EmptyIntSet)

  def union(o: IntSet): IntSet = o

  //def apply[EmptyIntSet](f: Int => Int): EmptyIntSet = this
}

val c: String = null

// def f(x: Int) = throw new IllegalArgumentException

def f(x: Int): Any = if (x.==(6)) EmptyIntSet else "hola"

def g(x: Int): Int = ???
//g(3)

// if(x==3) 8 else true => resultado es de tipo AnyVal
// if(x==5) 3 else "true" => resultado de tipo Any

val a = new NonEmptyIntSet(1, EmptyIntSet, EmptyIntSet)
val b = a
a == b
a eq b

val a1 = EmptyIntSet
val b1 = EmptyIntSet
a1 == b1
a1 eq b1

val a2 = new NonEmptyIntSet(1, EmptyIntSet, EmptyIntSet)
val b2 = new NonEmptyIntSet(2, EmptyIntSet, EmptyIntSet)
a2 == b2
a2 eq b2

val a3 = new NonEmptyIntSet(1, EmptyIntSet, EmptyIntSet)
val b3 = new NonEmptyIntSet(1, EmptyIntSet, EmptyIntSet)
a3 == b3
a3 eq b3

/*def apply[T <: IntSet](s: T, f: Int => Int): T =
  if (s == EmptyIntSet)
    EmptyIntSet
  else {
    val s1 = s.asInstanceOf[NonEmptyIntSet]
    new NonEmptyIntSet(f(s1.h), apply(s1.left, f), apply(s1.right, f))
  } */