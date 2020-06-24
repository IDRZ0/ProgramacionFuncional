def pascal(c: Int, r: Int): Int =
  if (c == 0 && r == 0)
    1
  else if (c == -1 || r == -1)
    0
  else
    pascal(c - 1, r - 1) + pascal(c, r - 1)

pascal(2, 4)

def balance(l: List[Char]): Boolean = {
  def inner(l: List[Char], c: Int): Boolean =
    if (l.isEmpty)
      c == 0
    else if (l.head == '(')
      inner(l.tail, c + 1)
    else if (l.head == ')')
      if (c > 0)
        inner(l.tail, c - 1)
      else
        false
    else
      inner(l.tail, c)

  inner(l, 0)
}
balance("())(".toList)
balance("(()())".toList)

def countChange(money: Int, coins: List[Int]): Int =
  if (money == 0)
    1
  else if (coins.isEmpty)
    0
  else
    countChange(money, coins.tail) +
      (if (money < coins.head)
        0
      else
        countChange(money - coins.head, coins))

countChange(4, List(1, 2))