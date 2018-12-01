class Rational(x: Int, y: Int) {
  def numer = x
  def denom = y

  def add(that: Rational) =
    new Rational(numer * that.denom + denom*that.numer, denom*that.denom)

  def sub(that: Rational) = add(that.neg)

  def mul(that: Rational) =
    new Rational(that.numer*numer, that.denom*denom)

  def div(that: Rational) =
    new Rational(that.denom*numer, that.numer*denom)

  def neg = new Rational(-numer, denom)

  override def toString: String = numer + "/" + denom

}

def addRational(r: Rational, s: Rational): Rational =
  new Rational(
    r.numer * s.denom + s.numer * r.denom,
    r.denom * s.denom)

def makeString(r: Rational): String = r.numer +" / " + r.denom
makeString(addRational(new Rational(1, 2), new Rational(2, 3)))


val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)
x.add(y).mul(z)


x.add(new Rational(2, 3))
x.sub(new Rational(2,3))
x.neg

x.sub(y).sub(z)

