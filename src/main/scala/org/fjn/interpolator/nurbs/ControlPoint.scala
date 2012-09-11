package org.fjn.interpolator.nurbs

import org.fjn.interpolator.common.{Point, MultiArrayView}
import org.fjn.interpolator.common.matrix.Matrix


/**
 * Trait hosting the list grid
 */
trait ControlPoint {

  /**List of points conforming the n-Dimensional samples provided for interpolation */
  val qk:Seq[Matrix[Double]]

  /**sequence hosting the number of points per dimension. This sequence is used to compute */
  val dim:Seq[Int]


  /**Accessor to n-dimensional grid */
  lazy val viewQk = new MultiArrayView[Matrix[Double]](qk,dim)

}
