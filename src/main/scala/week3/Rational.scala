package week3

class Rational(x: Int, y: Int) {
  require(y > 0, "denomiator must be positive")

  def this(x: Int) = this(x, 1)

  private def gcd(x: Int, y: Int): Int = if (y == 0) x.abs else gcd(y, x % y)
  def numer = x / gcd(x, y)
  def denom = y / gcd(x, y)

  def less(other: Rational): Boolean = numer * other.denom < other.numer * denom

  def max(other: Rational): Rational = if (this.less(other)) other else this

  def add(other: Rational): Rational = {
    new Rational(numer * other.denom + other.numer * denom, denom * other.denom)
  }

  def neg(): Rational = new Rational(-1 * numer, denom)

  def sub(other: Rational): Rational = add(other.neg())

  override def toString:String = numer + "/" + denom
}
