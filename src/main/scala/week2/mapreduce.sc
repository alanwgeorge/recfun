object exercise {
  def factorial(x: Int): Int = {
    product(x => x)(1, x)
  }
  factorial(4) + factorial(5) + factorial(6)
  def sum(f: Int => Int)(a: Int, b: Int): Int =
    mapReduce(f, (x, y) => x + y, 0)(a, b)
  sum(factorial)(4, 6)
  sum(x => x * x)(4, 6)
  def product(f: Int => Int)(a: Int, b: Int): Int =
    mapReduce(f, (x, y) => x * y, 1)(a, b)
  4 * 5 * 6
  product(x => x)(4, 6)
  product(factorial)(4, 6)
  def applyToRange(f: Int => Int)(a: Int, b: Int, unit: Int): Int = {
    if (a > b) unit
    else if (unit == 1) f(a) * applyToRange(f)(a + 1, b, unit)
    else f(a) + applyToRange(f)(a + 1, b, unit)
  }
  applyToRange(factorial)(4, 6, 1)
  applyToRange(factorial)(4, 6, 0)
  applyToRange(x => x * x)(4, 6, 1)
  applyToRange(x => x * x)(4, 6, 0)
  def mapReduce(f: Int => Int, combine: (Int, Int)  => Int, unit: Int)(a: Int, b: Int): Int = {
    if (a > b) unit
    else combine(f(a), mapReduce(f, combine, unit)(a + 1, b))
  }
  mapReduce(factorial, (x, y) => x * y, 1)(4, 6)
  mapReduce(factorial, (x, y) => x + y, 0)(4, 6)
  mapReduce(x => x * x, (x, y) => x * y, 1)(4, 6)
  mapReduce(x => x * x, (x, y) => x + y, 0)(4, 6)
}
