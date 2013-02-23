package org.fjn.interpolator.nurbs.instance

import org.fjn.interpolator.basis.ParameterVectorChord
import breeze.linalg.DenseMatrix

/**
 * Created with IntelliJ IDEA.
 * User: fran
 * Date: 18/02/13
 * Time: 23:28
 * To change this template use File | Settings | File Templates.
 */
class Nurbs2DChord(val qk: Seq[DenseMatrix[Double]], val basisOrderForCoord: Seq[Int], val dim: Seq[Int], implicit val tolerance: Double = 1.0e-2)
    extends ParameterVectorChord with Nurbs2DBase {

  println("Nurbs2DChord")

}
