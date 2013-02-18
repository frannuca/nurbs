package org.fjn.interpolator.nurbs.instance

import org.fjn.matrix.Matrix
import org.fjn.interpolator.nurbs._
import solver.Solver1D
import org.fjn.interpolator.basis._

trait Nurbs1DBase
    extends ControlPoint
    with ParameterVector
    with BasisFunctionOrder
    with KnotsVector
    with Basis
    with Solver1D {

  val tolerance: Double
  lazy val dim = Seq(qk.length)

  def apply(t: Double): Matrix[Double] = {

    var sum = new Matrix[Double](2, 1)
    (for (i <- getBasisRange(t)) yield {

      val a: Matrix[Double] = (new Matrix[Double](2, 1) <= Seq(pk(i, 0), pk(i, 1))) * NBasis(i, basisOrder(0), 0)(t)
      a
    }).toSeq.foldLeft(new Matrix[Double](2, 1).zeros)((acc, b) => acc + b)

  }

  /**
   *  TODO: do linear access to point in order to have fast coordinate look-up
   *  Correct non equally distribuions --> 1D coordinate look up is buggy(infinite loops)
   */
  def getNormalizedCoord(x: Double): Double = {

    def nurb = (x: Double) => this.apply(x)(0, 0)

    var dLow = 0.0
    var dHigh = 1.0
    var dMean = 0.5

    var mean = nurb(dMean)
    var counter = 0
    var found: Boolean = false
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

        dMean = (dHigh + dLow) * 0.5
        mean = nurb(dMean)

        if (dHigh <= dLow)
          found = true
      }
      counter = counter + 1
      if (counter > 500)
        found = true
    }

    dMean
  }

  def getBasisRange(t: Double): Seq[Int] = {
    val vector = knotsVector(0)
    val sz = vector.length - basisOrder(0) - 1

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
        i - basisOrder(0) - 1 until i + basisOrder(0) + 1
      } else
        0 until vector.length

    resVector.filter(c => c >= 0 && c < sz)

  }
}
