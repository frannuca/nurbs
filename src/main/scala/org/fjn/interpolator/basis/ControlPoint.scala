package org.fjn.interpolator.basis

import org.fjn.interpolator.common.{ Point, MultiArrayView }
//import org.fjn.matrix.Matrix
import breeze.linalg.DenseMatrix

/**
 * Trait hosting the list grid
 */
trait ControlPoint {

  /**List of points conforming the n-Dimensional samples provided for interpolation */
  def qk: Seq[DenseMatrix[Double]]

  /**sequence hosting the number of points per dimension. This sequence is used to compute */
  def dim: Seq[Int]

  /**Accessor to n-dimensional grid */
  lazy val viewQk = new MultiArrayView[DenseMatrix[Double]](qk, dim)

}
