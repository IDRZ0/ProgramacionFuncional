//Currificacion: funciones que toman varios parametros convertidas en varias funciones que toman un parametro
def sumFcurry(f: Int => Int): (Int, Int) => Int = {
  def myf(a: Int, b: Int): Int = {
    def inner(i: Int, ac: Int): Int = if (i > b) ac else inner(i + 1, ac + f(i))

    inner(a, 0)
  }

  myf
}

def sumCubes3 = sumFcurry(x => x * x * x)
sumCubes3(1, 3)
sumFcurry(x => x * x * x)(1, 3)

def sumFcurry2(f: Int => Int)(a: Int, b: Int) = {
  def inner(i: Int, ac: Int): Int = if (i > b) ac else inner(i + 1, ac + f(i))

  inner(a, 0)
}

def sumTriple: ((Int, Int) => Int) = sumFcurry2(3 * _)
sumTriple(2, 4)

def wrap(begin: String, body: String, end: String) = begin + body + end

def wrapWithP = wrap("<p>", _, "</p>")
wrapWithP("juan")

def wrapBuilder(n: String)(s: String): String = "<" + n + ">" + s + "</" + n + ">"

def wrapWithP2:String=>String = wrapBuilder("p")
wrapWithP2("pedro")

def comp(f: Int => Int, g: Int => Int)(x: Int): Int = f(g(x))

def f(x: Int) = 2 * x
def g(x: Int) = x + 4

def h: Int => Int = comp(f, g)
h(1)