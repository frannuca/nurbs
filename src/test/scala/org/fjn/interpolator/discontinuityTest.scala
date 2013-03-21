package org.fjn.interpolator

import common.MultiArrayView
import smoothers.{ LinearDiscontinuityInterpolation, Discontinuity }
import breeze.linalg.DenseMatrix
import net.ericaro.surfaceplotter.JSurfacePanel
import javax.swing.{ SwingUtilities, JFrame }
import java.awt.BorderLayout
import java.util.Random
import net.ericaro.surfaceplotter.surface.ArraySurfaceModel

object discontinuityTest extends App {

  val minValT = 0
  val maxValT = 1
  val minVal = 5
  val maxVal = 8
  val nDiscont = 50
  val nStrike = 50
  val axis_t = (0 until nDiscont).map(i => i.toDouble / nDiscont.toDouble * (maxValT - minValT) + minValT)
  val jumps = axis_t.indices.map(x => 2.0)
  val strikes = (0 until nStrike).map(i => {
    i.toDouble / (nStrike.toDouble - 1.0) * (maxVal - minVal) + minVal
  })

  def generateDiscontinuities: Seq[Discontinuity] = {
    var dvol = 1d;
    strikes.map(dk => {
      dvol = dvol + 0.1
      new Discontinuity(strike = dk, discontinuity = jumps.map(j => j * math.exp(dk)).toSeq)
    }).toSeq
  }

  val steps = new LinearDiscontinuityInterpolation(generateDiscontinuities, axis_t)

  def testSomething(f: (Double, Double) => Double, fRef: (Double, Double) => Double, dimX: Int, dimY: Int, qk: Seq[DenseMatrix[Double]]) {

    val jsp: JSurfacePanel = new JSurfacePanel
    jsp.setTitleText("discontinuity 2D")
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

  def generateSamples(nSamplesX: Int, nSamplesY: Int): (IndexedSeq[DenseMatrix[Double]]) = {

    val a = for {
      k <- 0 until nStrike
      h <- 0 until nDiscont
    } yield {
      val mt = new DenseMatrix[Double](2, 1)

      val v1 = k.toDouble / (nStrike.toDouble - 1.0) * (maxVal - minVal) + minVal
      val v2 = h.toDouble / nDiscont.toDouble * (maxValT - minValT) + minValT
      mt(0, 0) = v1
      mt(1, 0) = v2
      mt
    }
    a

  }

  SwingUtilities.invokeLater(new Runnable {
    def run {
      testSomething((u, v) => steps.step(u, v), (u, v) => 0.0, nStrike, nDiscont, generateSamples(nDiscont, nDiscont))
    }
  })
}
