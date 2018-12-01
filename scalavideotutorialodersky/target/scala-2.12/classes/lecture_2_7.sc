class Rational(x: Int, y: Int) {
  require(y > 0, "denominator must be positive")

  def this(x: Int) = this (x, 1)

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  private val g = gcd(x, y)

  def numer = x / g

  def denom = y / g

  def < (that: Rational) =
    numer * that.denom < that.numer * denom


  def > (that: Rational) =
    ! < (that)

  def min(that: Rational) = if (this < that) this else that

  def max(that: Rational) = if (this < that) that else this


  def + (that: Rational) =
    new Rational(numer * that.denom + denom * that.numer, denom * that.denom)

  def - (that: Rational) = this + -that

  def * (that: Rational) =
    new Rational(that.numer * numer, that.denom * denom)

  def / (that: Rational) =
    new Rational(that.denom * numer, that.numer * denom)

  def unary_- : Rational = new Rational(-numer, denom)

  override def toString: String = numer + "/" + denom
}

val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)
y + y
x + y
-y.numer
-y.denom
x * z
z / x
x < y
x > y