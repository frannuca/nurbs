package org.fjn.interpolator.nurbs.solver

import org.fjn.interpolator.common.MultiArrayView
import breeze.linalg.{ LinearAlgebra, DenseMatrix }

//import org.fjn.matrix.Matrix
import org.fjn.interpolator.basis.{ ParameterVector, BasisFunctionOrder, Basis, ControlPoint }

trait Solver2D {
  self: Basis with ParameterVector with ControlPoint with BasisFunctionOrder =>

  val pointDimension = 3 //(x,y,z)

  self.qk
  val weights: Array[Double] = new Array[Double](self.qk.length)
  var pk: Seq[DenseMatrix[Double]] = Seq()

  def SolveOnU(z: Seq[Double], viewer_norm: MultiArrayView[DenseMatrix[Double]],
    viewer_original: MultiArrayView[DenseMatrix[Double]], viewerZ: MultiArrayView[Double]): Seq[DenseMatrix[Double]] = {
    //Preparing the matrix for constant y-slices:
    var qXMatrix = new DenseMatrix[Double](dim(0), dim(0))
    for (i <- 0 until dim(0)) {
      val uk = parameterKnots(0)(i)
      for (j <- 0 until dim(0)) {

        val vv = NBasis(j, basisOrder(0), 0)(uk)
        qXMatrix(i, j) = vv
      }
    }
    qXMatrix = LinearAlgebra.inv(qXMatrix)

    val sampleSize = 3 //(x,y,z)

    //Solving linear systems for y index (x,0),(x,1),....,(x,m-1) with x 0 .. n-1
    //var Rl: Seq[Matrix[Double]] =
    val futuresOnSystemSolver: Seq[() => DenseMatrix[Double]] =
      for (l <- 0 until dim(1)) //we have dim(1) dim(1) points in y-direction, which are slices now
      yield {

        //Building linear system for the kth slice:

        //Basis function matrix:

        //sample vector(right side of the system):

        val f: () => DenseMatrix[Double] = () => {
          val rightM = new DenseMatrix[Double](dim(0), sampleSize)
          for (i <- 0 until dim(0)) {
            val auxPos = Seq(i, l)
            val posq = viewer_original(auxPos)
            for (j <- 0 until sampleSize - 1) {

              rightM(i, j) = posq(j, 0)
            }

            val auxZ = viewerZ(auxPos)
            rightM(i, sampleSize - 1) = auxZ
          }

          val auxVal = qXMatrix * rightM
          auxVal

        }
        f
      }

    val result = futuresOnSystemSolver.par.map(f => f())
    result.seq
  }

  def solve(z: Seq[Double]): Boolean = {

    val viewerZ = new MultiArrayView[Double](z, dim)

    val Rl = SolveOnU(z, viewTQk, viewQk, viewerZ)

    var qXMatrix = new DenseMatrix[Double](dim(1), dim(1))
    for (i <- 0 until dim(1)) {
      val vk = parameterKnots(1)(i)
      for (j <- 0 until dim(1)) {
        val vv = NBasis(j, basisOrder(1), 1)(vk)
        qXMatrix(i, j) = vv
      }
    }

    qXMatrix = LinearAlgebra.inv(qXMatrix)

    val futuresOnSystemSolver: Seq[() => DenseMatrix[Double]] =
      for (k <- 0 until dim(0)) //we have dim(1) dim(1) points in y-direction, which are slices now
      yield {

        //sample vector(right side of the system):
        () =>
          {
            val rightM = new DenseMatrix[Double](dim(1), pointDimension)
            for (i <- 0 until dim(1)) {
              for (j <- 0 until pointDimension)
                rightM(i, j) = Rl(i)(k, j)
            }

            qXMatrix * rightM
          }

      }

    pk = futuresOnSystemSolver.par.map(f => f()).seq

    true
  }

}