package org.fjn.interpolator.nurbs.instance

import org.fjn.interpolator.basis.ParameterVectorEqually
import breeze.linalg.DenseMatrix

class Nurbs1DEqually(val qk: IndexedSeq[DenseMatrix[Double]], val basisOrderForCoord: IndexedSeq[Int], val xMax: Double, val xMin: Double)
    extends Nurbs1DBase with ParameterVectorEqually {

  val tolerance: Double = 0
  override def getNormalizedCoord(x: Double): Double = {

    (x - xMin) / (xMax - xMin)

  }
}
