package org.fjn.interpolator.nurbs

import org.fjn.interpolator.common.MultiArrayView

/**
 * Created with IntelliJ IDEA.
 * User: fran
 * Date: 9/18/12
 * Time: 7:56 AM
 * To change this template use File | Settings | File Templates.
 */
object testViewer {
  def main(args: Array[String]) {
    val a = for (
      j <- 0 to 10;
      i <- 0 to 10
    ) yield {

      (i, j)
    }

    val vw = new MultiArrayView[(Int, Int)](a, Seq(11, 11))

    val _11 = vw(Seq(1, 1))
    val _9_4 = vw(Seq(9, 4))
    val _9_0 = vw(Seq(9, 0))
    val _0_9 = vw(Seq(0, 9))

    val ok = true;

  }
}
