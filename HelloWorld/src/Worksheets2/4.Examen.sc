abstract class Elem(val nro: Int, val est: String)

case class GasNoble(override val nro: Int) extends Elem(nro, "Gas") {
  override def toString: String = "GasNoble"
}

case class Reactivo(override val nro: Int, override val est: String) extends Elem(nro, est) {
  def reaccionar(e: Elem): Int =
    if (e.toString == "GasNoble")
      0
    else if (est == "Gas")
      nro * e.nro
    else if (est == "Liquido" && (e.est == "Liquido" || e.est == "Solido"))
      nro + e.nro
    else if (est == "Solido" && e.est == "Solido")
      Integer.max(nro, e.nro)
    else
      throw new IllegalArgumentException
}

val a = new Reactivo(3, "Gas")
val b = new Reactivo(5, "Liquido")
val c = new Reactivo(8, "Solido")
val d = new GasNoble(9)

a.reaccionar(a) //9
a.reaccionar(b) //15
a.reaccionar(c) //24
a.reaccionar(d) //0

b.reaccionar(a) //ex
b.reaccionar(b) //10
b.reaccionar(c) //13
b.reaccionar(d) //0

c.reaccionar(a) //ex
c.reaccionar(b) //ex
c.reaccionar(c) //8
c.reaccionar(d) //0

class Trans[+T <: Elem, -T1 <: Elem] {
  //def transA(o:T) :T1 = ???
  def transB(o: T1): T = ???
}

def j(t: Trans[Reactivo, GasNoble]) = ???

val t = new Trans[Reactivo, GasNoble]

j(t)

class C[+T <: Empleado, -U]

abstract class Empleado

abstract class Vendedor extends Empleado

class Gerente extends Empleado

class Senior extends Vendedor

class Junior extends Vendedor

def v(c: C[Vendedor, Empleado]) = ???
val u = new C[Vendedor, Empleado]
v(u)