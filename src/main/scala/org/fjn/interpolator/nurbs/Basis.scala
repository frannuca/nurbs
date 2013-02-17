package org.fjn.interpolator.nurbs

/**
 * contains the vector of spline order list per coordinate
 */
trait BasisFunctionOrder {
  val basisOrder: Seq[Int]
}

trait Basis {
  self: KnotsVector with BasisFunctionOrder =>

  //  private def N(knots: Seq[Seq[Double]])(i: Int, p: Int, nCoord: Int)(u: Double): Double = {
  //    if (p == 0) {
  //      if (knots(nCoord)(i) <= u && u < knots(nCoord)(i + 1))
  //        1.0
  //      else if(knots(nCoord)(i + 1) == 1 && u==1)
  //        1.0
  //      else
  //        0.0
  //    }
  //    else {
  //      val denom1 = (knots(nCoord)(i + p) - knots(nCoord)(i));
  //      val denom2 = (knots(nCoord)(i + p + 1) - knots(nCoord)(i + 1))
  //      val num1 = (u - knots(nCoord)(i))
  //      val num2 = (knots(nCoord)(i + p + 1) - u)
  //      val comp1 =
  //        if (math.abs(denom1) > 1e-6) {
  //          num1 / denom1 * N(knots)(i, p - 1, nCoord)(u)
  //        }
  //        else 0
  //      val comp2 = if (math.abs(denom2) > 1e-6) {
  //        num2 / denom2 * N(knots)(i + 1, p - 1, nCoord)(u)
  //      }
  //      else 0
  //
  //      comp1 + comp2
  //    }
  //  }

  private def N(knots: Array[Seq[Double]])(i: Int, p: Int, nCoord: Int)(u: Double): Double = {
    if (p == 0) {
      if (knots(nCoord)(i) <= u && u < knots(nCoord)(i + 1))
        1.0
      else if (knots(nCoord)(i + 1) == 1 && u == 1)
        1.0
      else
        0.0
    } else {
      val denom1 = (knots(nCoord)(i + p) - knots(nCoord)(i))
      val denom2 = (knots(nCoord)(i + p + 1) - knots(nCoord)(i + 1))
      val num1 = (u - knots(nCoord)(i))
      val num2 = (knots(nCoord)(i + p + 1) - u)
      val comp1 =
        if (math.abs(denom1) > 1e-6) {
          num1 / denom1 * N(knots)(i, p - 1, nCoord)(u)
        } else 0
      val comp2 = if (math.abs(denom2) > 1e-6) {
        num2 / denom2 * N(knots)(i + 1, p - 1, nCoord)(u)
      } else 0

      comp1 + comp2
    }
  }

  def NBasis(i: Int, p: Int, nCoord: Int)(u: Double) = N(self.knotsVector.toArray)(i, p, nCoord)(u)

}

