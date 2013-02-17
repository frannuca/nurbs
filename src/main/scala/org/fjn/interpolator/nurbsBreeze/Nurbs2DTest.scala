package org.fjn.interpolator.nurbsBreeze

import math._
import breeze.linalg._
import collection.immutable.IndexedSeq

object Nurbs2DTest {

  def generateSamples(nSamplesX: Int, nSamplesY: Int): IndexedSeq[DenseMatrix[Double]] = {

    val middle = 8

    for {
      k <- 0 until nSamplesY
      h <- 0 until nSamplesX
    } yield {
      val v1 = -0.5 * middle * Pi + middle * Pi * h.toDouble / (nSamplesX - 1)
      val v2 = -0.5 * middle * Pi + middle * Pi * k.toDouble / (nSamplesY - 1)
      DenseVector(v1, v2).t
    }
  }

  private def psinc(u: Double, v: Double): Double = {
    sin(math.sqrt(u * u + v * v + 1e-4)) / sqrt(u * u + v * v + 1e-4)
  }

  def main(args: Array[String]) {
    val nSamplesX = 25
    val nSamplesY = 25
    val qk = generateSamples(nSamplesX, nSamplesY)

    val nSamplesX2 = 75
    val nSamplesY2 = 75
    val qk2 = generateSamples(nSamplesX2, nSamplesY2)

    val order = 3

    val bspline = Nurbs2D(qk, IndexedSeq(order, order), IndexedSeq(nSamplesX, nSamplesY))

    val z: IndexedSeq[Double] = for (q <- qk) yield {
      val x = q(0, 0)
      val y = q(1, 0)

      psinc(x, y)
    }

    //    bspline.solve(z);

    //        var sumError =
    //          qk.par.map(item => {
    //            val u = bspline.getNormalizedCoord(item(0, 0), 0)
    //            val v = bspline.getNormalizedCoord(item(1, 0), 1)
    //            val ax = bspline(u, v)
    //            val x = ax(0, 0)
    //            val y = ax(1, 0)
    //            val z = ax(2, 0)
    //            val r = psinc(item(0, 0), item(1, 0))
    //
    //            math.abs(z - r)
    //          })
    //
    //        println("Total error =" + sumError.max)
    //
    //        SwingUtilities.invokeLater(new Runnable {
    //          def run {
    //            val ff =
    //              (x: Double, y: Double) => bspline(bspline.getNormalizedCoord(x, 0), bspline.getNormalizedCoord(y, 1))(2, 0)
    //            testSomething(ff, psinc, dim, qk)
    //          }
    //        })

    //    testFunc(bspline, z, qk2, nSamplesX2)
  }

}
