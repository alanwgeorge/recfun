
import math.abs
object exercise {
  val tolerace: Double = 0.0001
  def isCloseEnough(x: Double, y: Double): Boolean =
    abs((x - y) / x) / x < tolerace
  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
    def iterate(guess: Double): Double = {
      val next = f(guess)
      if (isCloseEnough(next, guess)) next
      else iterate(next)
    }
    iterate(firstGuess)
  }
  fixedPoint(x => 1 + x / 2)(1)
  def averageDamp(f: Double => Double)(x: Double) = (x + f(x))/2
  def sqrt(x: Double): Double =
  fixedPoint(averageDamp(y => x/y))(1)
  sqrt(2)

  def fixedPoint2(f: Double => Double, firstGuess: Double) = {
    def iterate(guess: Double): Double = {
      val next = f(guess)
      if (isCloseEnough(next, guess)) next
      else iterate(next)
    }
    iterate(firstGuess)
  }

  fixedPoint2(x => 1 + x / 2, 1)
}