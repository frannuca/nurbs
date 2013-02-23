package org.fjn.interpolator.nurbs.instance

import org.fjn.interpolator.basis._
import breeze.linalg.DenseMatrix
import org.fjn.interpolator.nurbs.solver.Solver2D

trait Nurbs2DBase
    extends ControlPoint
    with ParameterVector
    with BasisFunctionOrder
    with KnotsVector
    with Basis
    with Solver2D {

  def tolerance: Double

  def apply(u: Double, v: Double): DenseMatrix[Double] = {

    val sum = new DenseMatrix[Double](3, 1)
    for (i <- getBasisRange(0)(u)) {
      for (j <- getBasisRange(1)(v)) {
        val pAux = new DenseMatrix[Double](3, 1)
        pAux(0, 0) = pk(i)(j, 0)
        pAux(1, 0) = pk(i)(j, 1)
        pAux(2, 0) = pk(i)(j, 2)
        val basis = (NBasis(i, basisOrderForCoord(0), 0)(u) * NBasis(j, basisOrderForCoord(1), 1)(v))
        sum += pAux * basis
      }

    }

    sum
  }

  lazy val Real2TransformedAxis = (0 to 1000).map(x => this.apply(x / 1000.0, x / 1000.0))
  lazy val xReal = Real2TransformedAxis.map(x => x(0, 0))
  lazy val yReal = Real2TransformedAxis.map(x => x(1, 0))

  def getNormalizedCoord(x: Double, nCoord: Int): Double = {

    val axis =
      if (nCoord == 0) {
        xReal
      } else {
        yReal
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
    dMean / 1000.0

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

    val vector = knotsVector(nCoord)
    // TODO: Fran please document
    val sz = vector.length - basisOrderForCoord(nCoord) - 1

    val i = vector.indexWhere(t <= _)

    val resVector = if (i != -1 && i < sz) {
      i - basisOrderForCoord(nCoord) - 1 to i + basisOrderForCoord(nCoord) + 1
    } else {
      0 until vector.length
    }

    resVector.filter(c => c >= 0 && c < sz)

  }
}
