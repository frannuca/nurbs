package org.fjn.interpolator.nurbs.instance

import org.fjn.interpolator.basis.ParameterVectorChord
import breeze.linalg.DenseMatrix

class Nurbs1DChord(val qk: Seq[DenseMatrix[Double]], val basisOrder: Seq[Int], val tolerance: Double)
    extends Nurbs1DBase with ParameterVectorChord {

}
