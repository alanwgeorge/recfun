import math.pow
import week3.Rational

class RationalRaw(x: Int, y: Int) extends Rational(x, y) {
  require(y > 0, "denomiator must be positive")

  def this(x: Int) = this(x, 1)

  private def gcd(x: Int, y: Int): Int = if (y == 0) x.abs else gcd(y, x % y)
  val gcd: Int = gcd(x, y)
  override def numer = x
  override def denom = y

  override def toString:String = (numer / gcd)  + "/" + (denom / gcd)
}

val rational1 = new Rational(1, 2)
val rational2 = new Rational(1, 4)
val rational3 = new Rational(3, 333)
rational3.neg()
rational1.add(rational2)
rational1.neg()
rational1.sub(rational2)
val x = new RationalRaw(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)
x.sub(y).sub(z)
new Rational(2)
