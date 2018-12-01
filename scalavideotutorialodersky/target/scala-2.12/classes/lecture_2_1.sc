def sum_(f: Int => Int, a: Int, b: Int): Int =
  if (a > b) 0
  else f(a) + sum_(f, a + 1, b)

def id(x: Int): Int = x
def cube(x: Int): Int = x * x * x
def fact(x: Int): Int = if (x == 0) 1 else fact(x - 1)

def sumInts(a: Int, b: Int)
= sum_(id, a, b)
def sumCubes(a: Int, b: Int)
= sum_(cube, a, b)


def sumFactorials(a: Int, b: Int) = sum_(fact, a, b)

def sumInts_(a: Int, b: Int) = sum_(x => x, a, b)
def sumCubes_(a: Int, b: Int) = sum_(x => x * x * x, a, b)

  def sum (f: Int => Int, a: Int, b: Int) ={
    def loop(a:Int, acc: Int): Int =
      if (a > b) acc
        else
        loop(a + 1, acc + f(a))
    loop(a, 0)
}
sum(x => x*x*x, 2, 3)