package org.fjn.interpolator.smoothers

import collection.immutable.IndexedSeq

case class Discontinuity(strike: Double, discontinuity: Seq[Double])

class LinearDiscontinuityInterpolation(discontinuities: Seq[Discontinuity], axis_t: Seq[Double]) {
  //generate a surface discontinuity parametrization:

  val strikes = discontinuities.map(_.strike)
  val slices_dv: Map[Int, Seq[Double]] = discontinuities.indices.map(i => (i -> discontinuities(i).discontinuity)).toMap

  private def binarySearch(vector: Seq[Double])(k: Double): (Int, Int) = {
    var start = 0
    var end = strikes.length - 1
    var pivot = ((start + end) * 0.5).toInt

    while (math.abs(end - start) > 1) {
      val dPivot = vector(pivot)
      if (k < dPivot) {
        end = pivot
      } else if (k > dPivot) {
        start = pivot
      } else {
        end = pivot
        start = pivot
      }
      pivot = ((start + end) * 0.5).toInt
    }

    (start, end)
  }

  private def discontinuityInterpolation(k: Double, nt: Int): Double = {

    val (k0, k1) = binarySearch(strikes)(k)
    val strike0 = strikes(k0)
    val strike1 = strikes(k1)
    val dv0 = slices_dv(k0)(nt)
    val dv1 = slices_dv(k1)(nt)
    if (k0 < k1)
      (k - strike0) / (strike1 - strike0) * (dv1 - dv0) + dv0
    else
      dv0
  }

  def step(k: Double, t: Double): Double = {

    var index = 0
    var sum = 0d
    while (index < axis_t.length && t >= axis_t(index)) {
      sum += discontinuityInterpolation(k, index)
      index += 1
    }

    sum
  }

}
