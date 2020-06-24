val l = List(1, 1, 1, 2, 2, 3, 3, 1, 5, 5, 5)

def comprimir[T](l: List[T]): List[T] = {
  def inner(l1: List[T], acc: List[T]): List[T] = l1 match {
    case Nil => acc
    case h :: t => inner(t.dropWhile(_ == h), h :: acc)
  }

  inner(l, Nil).reverse
}
comprimir(l)

def prodEscalar(l1: List[Int], l2: List[Int]): Int = {
  def inner(l10: List[Int], l20: List[Int], ans: Int): Int = l10 match {
    case Nil => ans
    case h :: t => inner(t, l20.tail, ans + (h * l20.head))
  }

  inner(l1, l2, 0)
}
prodEscalar(List(1, 2, 3), List(4, 5, 6))

def quitar[T](x: T, l: List[T]): List[T] = {
  def inner(l1: List[T], ans: List[T]): List[T] = l1 match {
    case List() => ans
    case h :: t =>
      inner(l1.dropWhile(_ != x).tail, l1.takeWhile(_ != x) ::: ans)
  }

  inner(l, Nil)
}
//quitar(3, List(1, 2, 3, 4, 5, 4, 3, 2, 1))

def duplicar[T](l: List[T]): List[T] = {
  def inner(l1: List[T], ans: List[T]): List[T] = l1 match {
    case Nil => ans
    case h :: t => inner(l1.tail, h :: h :: ans)
  }

  inner(l, Nil).reverse
}
duplicar(List(1, 2, 3))

def ordenada(l: List[Int]): Boolean = {
  def inner(x: Int, l1: List[Int]): Boolean = l1 match {
    case Nil => true
    case h :: t => !(x > h) && inner(h, t)
  }

  inner(l.head, l.tail)
}
ordenada(List(1, 2, 3, 2))

def pen[T](l: List[T]): T = l.init.last
pen(List(1, 2, 3, 4))

def dupl[T](n: Int, l: List[T]): List[T] = {
  def inner(c: Int, l1: List[T], ans: List[T]): List[T] = l1 match {
    case Nil => ans
    case h :: t => if (c < n) inner(c + 1, l1, h :: ans) else inner(0, t, ans)
  }

  inner(0, l, Nil).reverse
}
dupl(4, List(1, 2, 3))

def rotar[T](n: Int, l: List[T]): List[T] = {
  def inner(c: Int, l1: List[T]): List[T] = l1 match {
    case Nil => Nil
    case h :: t => if (c < n) inner(c + 1, t ::: List(h)) else l1
  }

  inner(0, l)
}
rotar(2, List(1, 2, 3, 4, 5, 6, 7, 8))

def f(l: List[Int]): List[Int] = l match {
  case Nil => Nil
  case h :: t => {
    println(h)
    val t1 = t filterNot (_ == h)
    val (a, b) = t1 partition (_ <= h)
    val (as, bs) = (f(a), f(b))
    as ::: h :: bs
  }
}
f(List(4, 2, 6, 4, 3, 7, 2, 1, 5, 6))