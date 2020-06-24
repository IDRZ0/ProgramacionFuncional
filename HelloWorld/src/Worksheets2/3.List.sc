abstract class Lista[+T] {
  def isEmpty: Boolean

  def head: T

  def tail: Lista[T]

  //def get(i: Int): T

  def add[S >: T](e: S): Lista[S]
}

class Cons[+T](val head: T, val tail: Lista[T]) extends Lista[T] {
  def isEmpty: Boolean = false

  def add[S >: T](e: S): Lista[S] = new Cons(e, this)
}

object Nili extends Lista[Nothing] {
  def isEmpty: Boolean = true

  def head = throw new NoSuchElementException

  def tail = throw new NoSuchElementException

  def add[S](e: S): Lista[S] = new Cons(e, Nili)
}

//val l1 = new Nili[Any]
//val l2:Lista[Any] = new Cons(5, l1)
//val l3 = new Cons("hola", l2)

def singleton[T](x: T) = new Cons(x, Nili)
singleton(5)
singleton(true)

class Perro

class Chihuahua extends Perro

val l: Lista[Perro] = Nili

def f1(x: Perro): Perro = ???
def f2(x: Perro): Chihuahua = ???
def f3(x: Chihuahua): Perro = ???
def f4(x: Chihuahua): Chihuahua = ???

def g1: Perro => Perro = f1
def g2: Perro => Perro = f2
//def g3: Perro => Perro = f3
//def g4: Perro => Perro = f4

def h1: Chihuahua => Perro = f1
def h2: Chihuahua => Perro = f2
def h3: Chihuahua => Perro = f3
def h4: Chihuahua => Perro = f4

//def i1: Perro => Chihuahua = f1
def i2: Perro => Chihuahua = f2
//def i3: Perro => Chihuahua = f3
//def i4: Perro => Chihuahua = f4

//def h1: Chihuahua => Chihuahua = f1
def h2: Chihuahua => Chihuahua = f2
//def h3: Chihuahua => Chihuahua = f3
def h4: Chihuahua => Chihuahua = f4