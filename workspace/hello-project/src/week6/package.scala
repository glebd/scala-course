package object week6 {
  def isPrime(n: Int): Boolean = (2 until n) forall (x => n % x != 0)

  def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
    (xs zip ys).map(xy => xy._1 * xy._2).sum

  def scalarProduct2(xs: Vector[Double], ys: Vector[Double]): Double =
    (xs zip ys).map {case (x, y) => x * y}.sum

  def scalarProduct3(xs: Vector[Double], ys: Vector[Double]): Double =
    (for ((x, y) <- xs zip ys) yield x * y).sum
}