package org.fjn.interpolator.nurbs

import org.fjn.interpolator.common.matrix.Matrix
import solver.{Solver2D, Solver1D}



trait Nurbs1DBase
  extends ControlPoint
  with ParameterVector
  with BasisFunctionOrder
  with KnotsVector
  with Basis
  with Solver1D {


  val tolerance:Double
  lazy val dim = Seq(qk.length)

  def apply(t: Double): Matrix[Double] = {

    var sum = new Matrix[Double](2, 1)
    for (i <- getBasisRange(t)) {
      val pAux = new Matrix[Double](2, 1)
      pAux.set(0, 0, pk(i, 0))
      pAux.set(1, 0, pk(i, 1))
      sum = sum + pAux * NBasis(i, basisOrder(0), 0)(t)

    }

    sum

  }



  /**
   *  TODO: do linear access to point in order to have fast coordinate look-up
   *  Correct non equally distribuions --> 1D coordinate look up is buggy(infinite loops)
   * */
  def getNormalizedCoord(x: Double): Double = {



    def nurb = (x:Double) => this.apply(x)(0,0)

    var dLow = 0.0
    var dHigh = 1.0
    var dMean = 0.5


    var mean = nurb(dMean)
    var counter = 0
    var found: Boolean = false
    while (!found) {
      if (math.abs(x - mean) < tolerance) {
        found = true
      }
      else {
        if (x < mean) {
          dHigh = dMean
        }
        else if (x > mean) {
          dLow = dMean
        }
        else
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
      }
      else
        0 until vector.length

    resVector.filter(c => c >= 0 && c < sz)


  }
}


class Nurbs1DEqually(val qk: Seq[Matrix[Double]],val basisOrder: Seq[Int],val xMax:Double,val xMin:Double)
extends  Nurbs1DBase with ParameterVectorEqually{

  val tolerance:Double = 0
  override def getNormalizedCoord(x: Double): Double = {


    (x - xMin) / (xMax - xMin)


  }
}

class Nurbs1DChord(val qk: Seq[Matrix[Double]],val basisOrder: Seq[Int],val tolerance:Double)
  extends  Nurbs1DBase with ParameterVectorChord{

}

class Nurbs1DCentripetal(val qk: Seq[Matrix[Double]],val basisOrder: Seq[Int],val tolerance:Double)
  extends  Nurbs1DBase with ParameterVectorCentripetal{

}


trait Nurbs2DBase
  extends ControlPoint
  with ParameterVector
  with BasisFunctionOrder
  with KnotsVector
  with Basis
  with Solver2D {


  def tolerance: Double

  def apply(u: Double, v: Double): Matrix[Double] = {

    var sum = new Matrix[Double](3, 1)
    for (i <- getBasisRange(0)(u)) {
      for (j <- getBasisRange(1)(v)) {
        val pAux = new Matrix[Double](3, 1)
        pAux.set(0, 0, pk(i)(j, 0))
        pAux.set(1, 0, pk(i)(j, 1))
        pAux.set(2, 0, pk(i)(j, 2))
        val basis = (NBasis(i, basisOrder(0), 0)(u) * NBasis(j, basisOrder(1), 1)(v))
        sum = sum + pAux * basis
      }

    }

    sum
  }



  def getNormalizedCoord(x: Double, nCoord: Int): Double = {

    def nurb: (Double => Double) = x => {
      if (nCoord == 0) this.apply(x, 0)(nCoord, 0) else this.apply(0, x)(nCoord, 0)
    }

    var dLow = 0.0
    var dHigh = 1.0
    var dMean = 0.5


    var mean = nurb(dMean)
    var counter = 0
    var found: Boolean = false
    while (!found) {
      if (math.abs(x - mean) < tolerance) {
        found = true
      }
      else {
        if (x < mean) {
          dHigh = dMean
        }
        else if (x > mean) {
          dLow = dMean
        }
        else
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

  def getBasisRange(nCoord: Int)(t: Double): Seq[Int] = {

    //return 0 until  knotsVector(nCoord).length - basisOrder(nCoord)    -1

    val vector = knotsVector(nCoord)
    val sz = vector.length - basisOrder(nCoord) - 1

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
        i - basisOrder(nCoord) - 1 to i + basisOrder(nCoord) + 1
      }
      else
        0 until vector.length

    resVector.filter(c => c >= 0 && c < sz)



  }
}


class Nurbs2D(val qk:Seq[Matrix[Double]], val basisOrder: Seq[Int], val dim: Seq[Int], implicit val tolerance: Double = 1.0e-4)
  extends Nurbs2DBase with ParameterVectorEqually {


  val maxValX = qk.map(v => v(0,0)).max
  val maxValY = qk.map(v => v(1,0)).max


  val minValX = qk.map(v => v(0,0)).min
  val minValY = qk.map(v => v(1,0)).min


  override def getNormalizedCoord(x: Double, nCoord: Int): Double = {


    var maxVal = if(nCoord == 0) maxValX else maxValY
    var minVal = if(nCoord == 0) minValX else minValY



    val a = (x - minVal) / (maxVal - minVal)
    a


  }
}


class Nurbs2DChord(val qk: Seq[Matrix[Double]], val basisOrder: Seq[Int], val dim: Seq[Int], implicit val tolerance: Double = 1.0e-4)
  extends ParameterVectorChord with Nurbs2DBase {


  println("Nurbs2DChord")

}


class Nurbs2DCentripetal(val qk: Seq[Matrix[Double]], val basisOrder: Seq[Int], val dim: Seq[Int], implicit val tolerance: Double = 1.0e-4)
  extends ControlPoint with ParameterVectorCentripetal with Nurbs2DBase {


  println("Nurbs2DCentripetal")

}
