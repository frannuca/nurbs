package org.fjn.interpolator.basis

/**
 * contains the vector of spline order list per coordinate
 */
trait BasisFunctionOrder {
  val basisOrder: Seq[Int]
}

/*
 Implements the basis function as explained at:
  http://en.wikipedia.org/wiki/Non-uniform_rational_B-spline#Construction_of_the_basis_functions
 */
trait Basis {
  self: KnotsVector with BasisFunctionOrder =>

  /**
   * basic function evaluation
   * @param knots sequence of reference points (knot vector) to on which the nurb is defined
   * @param i index of the  basis funciton evaluation
   * @param p order of the nurb
   * @param nCoord axis coordinate
   * @param u  point of the parametrized curve to be evaluated
   * @param tol tolerance level to cut off division by zero.
   * @return evaluation of the specified basis function
   */
  private def N(knots: Array[Seq[Double]])(i: Int, p: Int, nCoord: Int)(u: Double, tol: Double = 1e-6): Double = {
    if (p == 0) {
      if ((knots(nCoord)(i) <= u && u < knots(nCoord)(i + 1)) || (knots(nCoord)(i + 1) == 1 && u == 1))
        1.0
      else
        0.0
    } else {
      val denom1 = (knots(nCoord)(i + p) - knots(nCoord)(i))
      val denom2 = (knots(nCoord)(i + p + 1) - knots(nCoord)(i + 1))
      val num1 = (u - knots(nCoord)(i))
      val num2 = (knots(nCoord)(i + p + 1) - u)
      val comp1 =
        if (math.abs(denom1) > tol) {
          num1 / denom1 * N(knots)(i, p - 1, nCoord)(u)
        } else 0
      val comp2 = if (math.abs(denom2) > tol) {
        num2 / denom2 * N(knots)(i + 1, p - 1, nCoord)(u)
      } else 0

      comp1 + comp2
    }
  }

  def NBasis(i: Int, p: Int, nCoord: Int)(u: Double) = N(self.knotsVector.toArray)(i, p, nCoord)(u)

}

