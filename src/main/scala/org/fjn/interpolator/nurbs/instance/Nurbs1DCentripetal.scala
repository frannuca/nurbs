package org.fjn.interpolator.nurbs.instance

import org.fjn.interpolator.basis.ParameterVectorCentripetal
import breeze.linalg.DenseMatrix

class Nurbs1DCentripetal(val qk: IndexedSeq[DenseMatrix[Double]], val basisOrderForCoord: IndexedSeq[Int], val tolerance: Double)
    extends Nurbs1DBase with ParameterVectorCentripetal {

}
