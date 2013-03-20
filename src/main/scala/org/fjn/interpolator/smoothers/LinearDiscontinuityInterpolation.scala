package org.fjn.interpolator.smoothers

import collection.immutable.IndexedSeq

case class Discontinuity(strike: Double, discontinuity: Seq[Double])

class LinearDiscontinuityInterpolation(discontinuities: Seq[Discontinuity], axis_t: Seq[Double]) {
  //generate a surface discontinuity parametrization:

  val strikes = discontinuities.map(_.strike)
  val slices_dv: Map[Int, Seq[Double]] = discontinuities.indices.map(i => (i -> discontinuities(i).discontinuity)).toMap

  private def binarySearch(vector: Seq[Double])(k: Double): (Int, Int) = {
    var start = 0
    var end = strikes.length
    var pivot = ((start + end) * 0.5).toInt

    while (math.abs(end - start) > 1) {
      val dPivot = vector(pivot)
      if (k < dPivot) {
        end = pivot
      } else if (k > dPivot) {
        start = pivot
      } else {
        start = end
        pivot = end
      }
      pivot = ((start + end) * 0.5).toInt
    }

    (start, end)
  }

  def discontinuityInterpolation(k: Double, nt: Int): Double = {

    val (k0, k1) = binarySearch(strikes)(k)
    val dv0 = slices_dv(k0)(nt)
    val dv1 = slices_dv(k1)(nt)
    (k - k0) / (k1 - k0) * dv1 + dv0
  }

  def step(k: Double, t: Double): Double = {

    var index = 0
    var sum = 0d
    while (t > axis_t(index) && index < axis_t.length) {
      sum += discontinuityInterpolation(k, index)
      index += 1
    }

    sum
  }

}
