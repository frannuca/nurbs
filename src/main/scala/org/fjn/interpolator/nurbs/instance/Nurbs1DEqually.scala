package org.fjn.interpolator.nurbs.instance

import org.fjn.matrix.Matrix
import org.fjn.interpolator.basis.ParameterVectorEqually

class Nurbs1DEqually(val qk: Seq[Matrix[Double]], val basisOrder: Seq[Int], val xMax: Double, val xMin: Double)
    extends Nurbs1DBase with ParameterVectorEqually {

  val tolerance: Double = 0
  override def getNormalizedCoord(x: Double): Double = {

    (x - xMin) / (xMax - xMin)

  }
}
