package org.fjn.interpolator.nurbs.instance

import org.fjn.interpolator.basis._
import breeze.linalg.DenseMatrix
import org.fjn.interpolator.nurbs.solver.Solver2D
import collection.immutable.IndexedSeq

trait Nurbs2DBase
    extends ControlPoint
    with ParameterVector
    with BasisFunctionOrder
    with KnotsVector
    with Basis
    with Solver2D {

  def tolerance: Double

  def apply(u: Double, v: Double): DenseMatrix[Double] = {

    var sum = new DenseMatrix[Double](3, 1)
    for (i <- getBasisRange(0)(u)) {
      for (j <- getBasisRange(1)(v)) {
        val pAux = new DenseMatrix[Double](3, 1)
        pAux(0, 0) = pk(i)(j, 0)
        pAux(1, 0) = pk(i)(j, 1)
        pAux(2, 0) = pk(i)(j, 2)
        val basis = (NBasis(i, basisOrderForCoord(0), 0)(u) * NBasis(j, basisOrderForCoord(1), 1)(v))
        sum = sum + pAux * basis
      }

    }

    sum
  }

  lazy val numberOfPointsX = ((qk.last(0, 0) - qk.head(0, 0)) / tolerance).toInt
  lazy val numberOfPointsY = ((qk.last(1, 0) - qk.head(1, 0)) / tolerance).toInt

  private def Real2TransformedAxis(nPoints: Int, nCoord: Int): IndexedSeq[Double] = {
    (0 to nPoints).map(x => {
      if (nCoord == 0) this.apply(x.toDouble / nPoints.toDouble, 0.0)(0, 0)
      else this.apply(0, x.toDouble / nPoints.toDouble)(1, 0)
    })
  }
  lazy val xReal: IndexedSeq[Double] = Real2TransformedAxis(numberOfPointsX, 0)
  lazy val yReal: IndexedSeq[Double] = Real2TransformedAxis(numberOfPointsY, 1)

  def getNormalizedCoord(x: Double, nCoord: Int): Double = {

    val (axis, nPoints) =
      if (nCoord == 0) {
        (xReal, numberOfPointsX)
      } else {
        (yReal, numberOfPointsY)
      }

    if (x >= axis.last) return 1.0
    else if (x <= axis.head) return 0.0

    var dLow = 0
    var dHigh = axis.length
    var dMean = (axis.length * 0.5).toInt

    var mean = axis(dMean)
    var found = false
    while (!found) {

      if (math.abs(x - mean) < tolerance) {
        found = true
      } else {
        if (x < mean) {
          dHigh = dMean
        } else if (x > mean) {
          dLow = dMean
        } else
          found = true

        dMean = ((dHigh + dLow) * 0.5).toInt
        mean = axis(dMean)

        if (dHigh <= dLow || dHigh - dLow == 1)
          found = true
      }

    }
    dMean.toDouble / nPoints.toDouble

    //    def nurb: (Double => Double) = x => {
    //      (if (nCoord == 0) this.apply(x, 0) else this.apply(0, x))(nCoord, 0)
    //    }
    //
    //    var dLow = 0.0
    //    var dHigh = 1.0
    //    var dMean = 0.5
    //
    //    var mean = nurb(dMean)
    //    var counter = 0
    //    var found: Boolean = false
    //    while (!found) {
    //      if (math.abs(x - mean) < tolerance) {
    //        found = true
    //      } else {
    //        if (x < mean) {
    //          dHigh = dMean
    //        } else if (x > mean) {
    //          dLow = dMean
    //        } else
    //          found = true
    //
    //        dMean = (dHigh + dLow) * 0.5
    //        mean = nurb(dMean)
    //
    //        if (dHigh <= dLow)
    //          found = true
    //      }
    //      counter = counter + 1
    //      if (counter > 500)
    //        found = true
    //    }
    //
    //    dMean
  }

  /**
   * Finds all the basis functions that contribute to a point {{{t}}}.
   * This is an optimization since not all basis functions influence every point.
   *
   * @param nCoord which coordinate
   * @param t point
   * @return
   */
  def getBasisRange(nCoord: Int)(t: Double): Seq[Int] = {

    //return 0 until  knotsVector(nCoord).length - basisOrder(nCoord)    -1

    val vector = knotsVector(nCoord)
    val sz = vector.length - basisOrderForCoord(nCoord) - 1

    var i = 0
    var found: Boolean = false
    var counter = 0
    while (!found && counter < sz) {
      if (t <= vector(counter)) {
        i = counter
        found = true
      }
      counter = counter + 1
    }

    val resVector =
      if (found) {
        i - basisOrderForCoord(nCoord) - 1 to i + basisOrderForCoord(nCoord) + 1
      } else
        0 until vector.length

    resVector.filter(c => c >= 0 && c < sz)

  }
}
