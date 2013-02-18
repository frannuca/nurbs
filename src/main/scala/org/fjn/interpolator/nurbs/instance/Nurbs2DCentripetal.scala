package org.fjn.interpolator.nurbs.instance

import org.fjn.matrix.Matrix
import org.fjn.interpolator.basis.{ ParameterVectorCentripetal, ControlPoint }

/**
 * Created with IntelliJ IDEA.
 * User: fran
 * Date: 18/02/13
 * Time: 23:28
 * To change this template use File | Settings | File Templates.
 */
class Nurbs2DCentripetal(val qk: Seq[Matrix[Double]], val basisOrder: Seq[Int], val dim: Seq[Int], implicit val tolerance: Double = 1.0e-4)
    extends ControlPoint with ParameterVectorCentripetal with Nurbs2DBase {

  println("Nurbs2DCentripetal")

}
