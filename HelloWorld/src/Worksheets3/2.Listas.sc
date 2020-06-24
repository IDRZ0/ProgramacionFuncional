val l = List("Colo " + "basura")
List(List(2), "loco")
List()
Nil

//Cons: lo ejecuta el de la derecha, va de derecha a izquierda
val l1 = 1 :: 2 :: Nil
l1.head
l1.tail
l1.isEmpty
l1.length

l1 match {
  case a :: b => b
}

def insertionSort(l: List[Int]): List[Int] = {
  def insert(n: Int, l: List[Int]): List[Int] = l match {
    case Nil => List(n)
    case h :: t => if (n < h) n :: l else h :: insert(n, t)
  }

  l match {
    case Nil => Nil
    case h :: t => insert(h, insertionSort(t))
  }
}

val l2 = List("a", "b", "c")
val l3 = List()
l2(1)

l2.head
l2.last
l2.reverse
l2.init //lista sin el ultimo
l2.tail //lista sin el primero
l2.take(2) //devuelve los primeros n elementos
l2.drop(2) //devuelve todos menos los n primeros elementos
l2.splitAt(2)
l2.updated(2, "colo")
l2.indexOf("a")
l2.contains("b")

def myContains[T](l: List[T], e: T): Boolean = l match {
  case Nil => false
  case h :: t => (h == t) || myContains(t, e)
}

l ++ l2
l ::: l2 //especifico de listas