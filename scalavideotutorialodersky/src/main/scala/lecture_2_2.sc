def fact(x: Int): Int = if (x == 0) 1 else fact(x - 1)
/*
def sum(f: Int => Int): (Int, Int) => Int = {
  def sumF(a: Int, b: Int): Int =
    if (a > b) 0
    else f(a) + sumF(a + 1, b)
  sumF
}
*/

def sum(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 0 else f(a) + sum(f)(a + 1, b)

/*
def sumInts = sum(x => x)
def sumCubes
= sum(x => x * x * x)
def sumFactorials = sum(fact)
sumCubes(1, 10) + sumFactorials(10, 20)
*/
sum(x=> x*x*x)(1, 10) + sum(fact)(10, 20)

def product(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b ) 1 else f(a)*product(f)(a + 1, b)

product(x => x)(1,10)

def factorial (x: Int): Int = product(x=>x)(1, x)
factorial(10)

def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero : Int)(a: Int, b: Int): Int ={
  if (a > b) zero
  else
    combine(f(a), mapReduce(f, combine, zero)(a + 1, b))
}

mapReduce(x => x, (x, y) => x + y, 0)(1, 10)
mapReduce(x => x, (x, y) => x * y, 1)(1, 10)

def product_(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x * y, 1)(a, b)
def sum_(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x + y, 0)(a, b)

product(x => x)( 1, 10)
product_(x => x)( 1, 10)
sum(x => x)( 1, 10)
sum_(x => x)( 1, 10)