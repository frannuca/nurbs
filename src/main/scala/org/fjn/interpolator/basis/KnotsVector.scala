package org.fjn.interpolator.basis

import collection.{ immutable, Seq }

/**
 * this trait hosts the vector knot which contains the list of instance per coordinate
 *  Nurbs need to define a series of points in the normalized spaced (from 0 to 1) which define the reference
 * points for constructing the basis functions of order p.
 *
 */
trait KnotsVector {
  self: ParameterVector with BasisFunctionOrder =>

  def computeKnots(params: Seq[Double], p: Int): Seq[Double] = {

    val N = params.length - 1
    val a = Seq.fill(p + 1)(0d)
    val b = for (i <- 1 to N - p) yield {

      val numberOfItems = p + 1

      val s1 = i + numberOfItems
      val s0 = i

      val auxSq = params.slice(s0, s1)

      auxSq.sum / numberOfItems.toDouble
    }
    val c = Seq.fill(p + 1)(1d)
    a ++ b ++ c
  }

  lazy val knotsVector: Seq[Seq[Double]] = for (p <- self.parameterKnots.indices) yield computeKnots(self.parameterKnots(p), basisOrder(p))
}