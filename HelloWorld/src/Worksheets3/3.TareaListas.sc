def myLast[T](l: List[T]): T = l match {
  case Nil => throw new NoSuchElementException
  case h :: Nil => h
  case h :: t => myLast(t)
}
myLast(List(1, 2))

def myReverse[T](l: List[T]): List[T] = {
  def inner(l1: List[T], l2: List[T]): List[T] = l1 match {
    case Nil => l2
    case h :: t => inner(t, h :: l2)
  }

  inner(l, Nil)
}
myReverse(List(1, 2, 3))

def myInit[T](l: List[T]): List[T] = {
  def inner(l1: List[T], l2: List[T]): List[T] = l1 match {
    case Nil => throw new UnsupportedOperationException
    case h :: Nil => l2
    case h :: t => inner(t, myReverse(h :: l2))
  }

  inner(l, Nil)
}
myInit(List(1, 2, 3))

def myConcat[T](l1: List[T], l2: List[T]): List[T] = l1 match {
  case Nil => l2
  case h :: t => l2 match {
    case Nil => l1
    case h1 :: t1 => myConcat(myReverse(h1 :: myReverse(l1)), t1)
  }
}
myConcat(List(1, 2, 3), List(4, 5, 6))

def myConcat1[T](l1: List[T], l2: List[T]): List[T] = {
  def inner(l: List[T], d: List[T]): List[T] = l match {
    case h :: Nil => Nil
    case h :: t => inner(t, h :: d)
  }

  inner(myReverse(l1), l2)
}

def flatten(l: List[Any]): List[Any] = l match {
  case Nil => Nil
  case h :: t => h match {
    case h1: List[Any] => myConcat(flatten(h1), flatten(t))
    case _ => h :: flatten(t)
  }
}