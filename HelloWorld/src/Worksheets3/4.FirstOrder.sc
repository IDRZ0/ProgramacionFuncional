val l = List(1, 2, 3, 4, 5, 4, 3, 2, 1)
l.splitAt(3)

def myMap[T, S](l: List[T], f: T => S): List[S] = l match {
  case Nil => Nil
  case h :: t => f(h) :: myMap(t, f)
}

def myMapTR[T, S](l: List[T], f: T => S): List[S] = {
  def inner(l1: List[T], ac: List[S]): List[S] = l1 match {
    case Nil => ac
    case h :: t => inner(t, f(h) :: ac)
  }

  inner(l, Nil).reverse
}

l.map(_ % 2 == 0)
l.map(_ / 2)

def myFilter[T](l: List[T], f: T => Boolean): List[T] = {
  def inner(l1: List[T], ac: List[T]): List[T] = l1 match {
    case Nil => ac
    case h :: t => if (f(h)) inner(t, h :: ac) else inner(t, ac)
  }

  inner(l, Nil).reverse
}

l.filter(_ > 3)
l.filterNot(_ > 3)
l.partition(_ < 3) //tupla, los que si satisfacen y los que no

def myTakeWhile[T](l: List[T], f: T => Boolean): List[T] = {
  def inner(l1: List[T], acc: List[T]): List[T] = l1 match {
    case Nil => acc
    case h :: t => if (f(h)) inner(t, h :: acc) else acc
  }

  inner(l, Nil).reverse
}

l.takeWhile(_ != 3) ::: l
l.dropWhile(_ != 3).tail
l.span(_ < 3) // separa entre los primeros que cumplen f y los demas

def pack[T](l: List[T]): List[List[T]] = l match {
  case Nil => Nil
  case h :: t => {
    val (a, b) = l.span(_ == h)
    a :: pack(b)
  }
}
pack(List(1, 1, 1, 2, 2, 2, 3, 3, 3, 1, 2, 3))

def encode[T](l: List[T]): List[(T, Int)] = l match {
  case Nil => Nil
  case h :: t => (h, pack(l).head.length) :: encode(t.dropWhile(_ == h))
}
encode(List(1, 1, 1, 2, 2, 2, 3, 3, 3, 1, 2, 3))

//def flatMap[T, S](l: List[T], f: T => List[S]): List[S]

l.reduceLeft(_ + _)

def myReduceLeft[T](l: List[T], op: (T, T) => T): T = {
  def inner(l1: List[T], acc: T): T = l1 match {
    case Nil => acc
    case h :: t => inner(t, op(acc, h))
  }

  inner(l.tail, l.head)
}
myReduceLeft(List(1, 2, 3, 4), (x: Int, y: Int) => x + y)

l.foldLeft(1)(_ + _)

// def myFoldLeft[T, S](l: List[T], acc: S, op: (S, T) => S)

def max(l: List[Int]): Int = l.reduceLeft((a, b) => Integer.max(a, b))

def maxF(l: List[Int]): Int = l.tail.foldLeft(l.head)((a, b) => Integer.max(a, b))
maxF(List(-1, -2))

def myLength[T](l: List[T]): Int = l.foldLeft(0)((acc, t) => acc + 1)
myLength(List(1, 2, 3))

def myConcat[T](l1: List[T], l2: List[T]): List[T] = (l2.foldLeft(l1.reverse)((acc, t) => t :: acc)).reverse
myConcat(List(1, 2), List(3, 4))

1 to 10
1 until 10