package org.fjn.interpolator.nurbsBreeze

import breeze.linalg.DenseMatrix

final case class Nurbs2D(qk: IndexedSeq[DenseMatrix[Double]], basisOrder: IndexedSeq[Int], dim: IndexedSeq[Int], tolerance: Double = 1.0e-4) {

  val maxValX = qk.map(v => v(0, 0)).max
  val maxValY = qk.map(v => v(1, 0)).max

  val minValX = qk.map(v => v(0, 0)).min
  val minValY = qk.map(v => v(1, 0)).min

  private def genSeq(v: Int, nCoord: Int, nDim: Int): IndexedSeq[Int] = {
    IndexedSeq.tabulate[Int](nDim) {
      i => if (i == nCoord) v else 0
    }
  }

  //  lazy val parameterKnots: Seq[Seq[Double]] = {
  //    for (nD <- dim.indices) yield {
  //      val nDim = dim(nD)
  //      // eye???
  //      val a = (for (n <- 0 until nDim) yield {
  //        val sq = genSeq(n, nD, dim.length)
  //        val a: Matrix[Double] = self.viewQk(sq)
  //        a
  //      }).toSeq
  //
  //      computParameters(a.map(v => v(nD, 0)))
  //    }
  //  }
  //
  //  //  override def getNormalizedCoord(x: Double, nCoord: Int): Double = {
  //  //
  //  //    var maxVal = if (nCoord == 0) maxValX else maxValY
  //  //    var minVal = if (nCoord == 0) minValX else minValY
  //  //
  //  //    val a = (x - minVal) / (maxVal - minVal)
  //  //    a
  //  //
  //  //  }
}
