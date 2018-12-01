def abs(x: Double) = if (x < 0) -x else x

def sqrt(x: Double) = {


  def improve(guess: Double) =
    (guess + x / guess) / 2

  def isGoodEnough(guess: Double) =
    abs(guess * guess - x) / x < 0.0001

  def sqrtIter(guess: Double): Double =
    if (isGoodEnough(guess)) guess
    else sqrtIter(improve(guess))

  sqrtIter(1.0)
}

sqrt(2)
sqrt(4)
sqrt(1.0e-6)
sqrt(1.0e60)

def gcd(a: Int, b: Int): Int =
  if (b == 0) a else gcd(b, a % b)

gcd(14, 21)

def factorial(n: Int): Int =
  if (n == 0) 1 else n * factorial(n - 1)
factorial(4)

def factorial_(n: Int): Int = {
  def loop(acc: Int, n: Int): Int =
    if (n == 0)
      acc
    else
      loop(acc * n, n - 1)
  loop(1,n)
}

factorial_(24)


