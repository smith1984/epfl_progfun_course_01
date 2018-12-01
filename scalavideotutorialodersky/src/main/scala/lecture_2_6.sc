class Rational(x: Int, y: Int) {
  require(y > 0, "denominator must be positive")

  def this(x: Int) = this (x, 1)

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  private val g = gcd(x, y)

  def numer = x / g

  def denom = y / g

  def less(that: Rational) =
    numer * that.denom < that.numer * denom


  def more(that: Rational) =
   !less(that)

  def min(that: Rational) = if (this.less(that)) this else that

  def max(that: Rational) = if (this.less(that)) that else this


  def add(that: Rational) =
    new Rational(numer * that.denom + denom * that.numer, denom * that.denom)

  def sub(that: Rational) = add(that.neg)

  def mul(that: Rational) =
    new Rational(that.numer * numer, that.denom * denom)

  def div(that: Rational) =
    new Rational(that.denom * numer, that.numer * denom)

  def neg = new Rational(-numer, denom)

  override def toString: String = {
    //val g = gcd(numer, denom)
    numer /* /g */+ "/" + denom  /* /g */
  }

}

val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)
y.add(y)
x.add(y)
x.less(y)
x.max(y)

val w = new Rational(5)
x add y
