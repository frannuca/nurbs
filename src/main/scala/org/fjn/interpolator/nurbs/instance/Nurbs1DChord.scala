package org.fjn.interpolator.nurbs.instance

import org.fjn.matrix.Matrix
import org.fjn.interpolator.basis.ParameterVectorChord

class Nurbs1DChord(val qk: Seq[Matrix[Double]], val basisOrder: Seq[Int], val tolerance: Double)
    extends Nurbs1DBase with ParameterVectorChord {

}
