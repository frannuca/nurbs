package org.fjn.interpolator.nurbs.instance

import org.fjn.interpolator.basis.{ ParameterVectorCentripetal, ControlPoint }
import breeze.linalg.DenseMatrix

class Nurbs2DCentripetal(val qk: IndexedSeq[DenseMatrix[Double]], val basisOrderForCoord: IndexedSeq[Int], val dim: IndexedSeq[Int], implicit val tolerance: Double = 1.0e-4)
    extends ControlPoint with ParameterVectorCentripetal with Nurbs2DBase {

  println("Nurbs2DCentripetal")

}
