package org.fjn.interpolator.nurbs

import instance.{ Nurbs2DEqually, Nurbs2DBase, Nurbs2DChord, Nurbs2DCentripetal }
import scala.Array
import javax.swing.{ SwingUtilities, JFrame }
import java.awt.BorderLayout
import java.util.Random
import org.fjn.interpolator.common.MultiArrayView
import org.fjn.interpolator.util.Timer

import net.ericaro.surfaceplotter.JSurfacePanel
import net.ericaro.surfaceplotter.surface.ArraySurfaceModel
import scala.math._
import breeze.linalg.DenseMatrix
import org.fjn.interpolator.smoothers.{ Discontinuity, LinearDiscontinuityInterpolation }

object plotting {

  val nSamplesX = 100
  val nSamplesY = 500

  val nSamplesX2 = 125
  val nSamplesY2 = 125

  def testSomething(f: (Double, Double) => Double, fRef: (Double, Double) => Double, dimX: Int, qk: Seq[DenseMatrix[Double]]) {

    val dimY = dimX

    val jsp: JSurfacePanel = new JSurfacePanel
    jsp.setTitleText("NURBS 2D")
    val jf: JFrame = new JFrame("test")
    jf.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    jf.getContentPane.add(jsp, BorderLayout.CENTER)
    jf.pack
    jf.setVisible(true)
    val rand: Random = new Random

    val z1 = Array.ofDim[Float](dimX, dimY)
    val z2 = Array.ofDim[Float](dimX, dimY)

    val vw = new MultiArrayView[DenseMatrix[Double]](qk, Seq(dimX, dimY))
    for {
      i <- 0 until dimX
      j <- 0 until dimY
    } {

      val p = vw(Seq(i, j))
      z1(i)(j) = f(p(0, 0), p(1, 0)).toFloat
      z2(i)(j) = fRef(p(0, 0), p(1, 0)).toFloat
    }

    val xx = qk.map(p => p(0, 0))
    val xMax = xx.max
    val xMin = xx.min

    val yy = qk.map(p => p(1, 0))
    val yMax = yy.max
    val yMin = yy.min

    val sm: ArraySurfaceModel = new ArraySurfaceModel
    sm.setValues(xMin.toFloat, xMax.toFloat, yMin.toFloat, yMax.toFloat, dimX, z1, z2)
    jsp.setModel(sm)
  }

  def psinc(u: Double, v: Double): Double = {
    sin(sqrt(u * u + v * v + 1e-4)) / sqrt(u * u + v * v + 1e-4) * 100.0 + steps.step(u, v)
  }

  val middle = 8

  val axis_t = (0 until 6).map(k => -0.5 * middle * Pi + middle * Pi * k.toDouble / (nSamplesY - 1))
  def generateDiscontinuities(n: Int): Seq[Discontinuity] = {

    (0 until n).map(i => {
      val v1 = (-0.5 * middle * Pi + middle * Pi * i.toDouble / (nSamplesX - 1)) * 3.0
      new Discontinuity(strike = v1, discontinuity = axis_t.map(_ => 0.1).toSeq)
    }).toSeq

  }
  def generateSamples(nSamplesX: Int, nSamplesY: Int): (IndexedSeq[DenseMatrix[Double]]) = {

    val a = for {
      k <- 0 until nSamplesY
      h <- 0 until nSamplesX
    } yield {
      val mt = new DenseMatrix[Double](2, 1)

      val v1 = (-0.5 * middle * Pi + middle * Pi * h.toDouble / (nSamplesX - 1)) * 3.0
      val v2 = -0.5 * middle * Pi + middle * Pi * k.toDouble / (nSamplesY - 1)
      mt(0, 0) = v1
      mt(1, 0) = v2
      mt
    }
    a

  }

  val steps = new LinearDiscontinuityInterpolation(generateDiscontinuities(5), axis_t)
  def testFunc(bspline: Nurbs2DBase, z: Array[Double], qk: Seq[DenseMatrix[Double]], dim: Int) {

    bspline.solve(z)

    val (time, sumError) = Timer.timed {
      qk.par.map {
        (item: DenseMatrix[Double]) =>
          val u1 = item(0, 0)
          val v1 = item(1, 0)
          val u = bspline.getNormalizedCoordSlow(u1, 0)
          val v = bspline.getNormalizedCoordSlow(v1, 1)
          val ax = bspline(u, v)
          val x = ax(0, 0)
          val y = ax(1, 0)
          val z = ax(2, 0)
          val r = psinc(item(0, 0), item(1, 0))

          abs(z - r)
      }
    }

    println("Total error = %f in %f s".format(sumError.max, time / 1000d))

    SwingUtilities.invokeLater(new Runnable {
      def run {
        val ff =
          (x: Double, y: Double) => bspline(bspline.getNormalizedCoord(x, 0), bspline.getNormalizedCoord(y, 1))(2, 0)
        testSomething(ff, psinc, dim, qk)
      }
    })

  }
}

object testNurbs2DEqually {

  def main(args: Array[String]) {

    import plotting._

    val qk = generateSamples(nSamplesX, nSamplesY)

    val z = (for (q <- qk) yield {
      val x = q(0, 0)
      val y = q(1, 0)

      psinc(x, y)
    }).toArray

    val order = 3

    val bspline = new Nurbs2DEqually(qk, Array(1, order), Array(nSamplesX, nSamplesY))

    val qk2 = generateSamples(nSamplesX2, nSamplesY2)
    testFunc(bspline, z, qk2, nSamplesX2)
  }

}

object testNurbs2DChord {

  def main(args: Array[String]) {

    import plotting._

    val qk = generateSamples(nSamplesX, nSamplesY)

    val z = (for (q <- qk) yield {
      val x = q(0, 0)
      val y = q(1, 0)

      psinc(x, y)
    }).toArray

    val order = 2

    val bspline = new Nurbs2DChord(qk, Array(order, order), Array(nSamplesX, nSamplesY), 1e-2)

    val qk2 = generateSamples(nSamplesX2, nSamplesY2)
    testFunc(bspline, z, qk2, nSamplesX2)
  }

}

object testNurbs2DCentripetal {

  def main(args: Array[String]) {

    import plotting._

    val qk = generateSamples(nSamplesX, nSamplesY)

    val z = (for (q <- qk) yield {
      val x = q(0, 0)
      val y = q(1, 0)

      psinc(x, y)
    }).toArray

    val order = 3

    val bspline = new Nurbs2DCentripetal(qk, Array(order, order), IndexedSeq(nSamplesX, nSamplesY), 1e-2)

    val qk2 = generateSamples(nSamplesX2, nSamplesY2)
    testFunc(bspline, z, qk2, nSamplesX2)
  }

}