package org.fjn.interpolator.nurbs

import instance.{ Nurbs1DBase, Nurbs1DEqually, Nurbs1DChord, Nurbs1DCentripetal }
import breeze.linalg.DenseMatrix

object testNurbs1D {

  def main(args: Array[String]) {

    def func(x: Double): Double = {
      math.sin(x * math.Pi)
    }

    val qk = (0 until 1000).par.map(n => {
      val x = n.toDouble / 1000.0
      val z = func(x)
      val m = new DenseMatrix[Double](2, 1)
      m(0, 0) = x
      m(1, 0) = z
      m
    })

    val order = 1
    val xAxis: Seq[DenseMatrix[Double]] = qk.par.map(v => {
      val o = new DenseMatrix[Double](1, 1);
      o(0, 0) = v(0, 0)
      o
    }
    ).seq.toSeq
    //Equally:

    def testFunc(bspline: Nurbs1DBase) {

      bspline.solve(qk.map(v => v(1, 0)).toArray);

      //check values
      val diff = (0 until 1000).par.map(n => {
        val x = n.toDouble / 1000.0
        val z = func(x)
        val tx = bspline.getNormalizedCoord(x)
        val zNurb = bspline(tx)
        math.abs(z - zNurb(1, 0))

      })

      println("max diff=" + diff.max.toString())

    }

    val bspline = new Nurbs1DEqually(xAxis, Seq(order), 1.0, 0.0)
    testFunc(bspline)

    //Chord:
    val bspline2 = new Nurbs1DChord(xAxis, Seq(order), 1e-2)
    testFunc(bspline2)

    val bspline3 = new Nurbs1DCentripetal(xAxis, Seq(order), 1e-2)
    testFunc(bspline3)

  }

}
