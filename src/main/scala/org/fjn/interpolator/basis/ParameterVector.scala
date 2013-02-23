package org.fjn.interpolator.basis

import org.fjn.interpolator.common.{ MultiArrayView, Point }
import breeze.linalg.DenseMatrix
import scala.math._

/**
 * Created by fjn army of one.
 * User: fran
 * Date: 8/29/12
 * Time: 7:19 PM
 */
/**
 * This trait computes the location of the control points into the target NURBS space
 * The control points are computed per coordinate given the list of input points qk
 * We can think of the control points as the mapping from real-space points into the normalized NURBS space points
 * This trait needs to be extended in order to compute the location in the normalized space. Typically three
 * methods are used: Centripetal, Chord and Equally distributed
 */
trait ParameterVector {
  self: ControlPoint =>

  /**
   * Computes the instance vector given the sample points provided in the inherited trait ControlPoint
   * @param axis sequence of vector compounding the axis to be processed for instance computation
   * @return sequence of transformed vector in the normalized space
   */
  protected def computeParameters(axis: Seq[Double]): Seq[Double]

  private def genSeq(v: Int, nCoord: Int, nDim: Int): Seq[Int] = {
    for (n <- 0 until nDim) yield {
      if (n == nCoord) v else 0
    }
  }

  lazy val parameterKnots: Seq[Seq[Double]] = {

    for { nD <- 0 until dim.length } yield {
      val nDim = dim(nD)
      val a = for (n <- 0 until nDim) yield {
        val sq: Seq[Int] = genSeq(n, nD, dim.length)
        val a: DenseMatrix[Double] = viewQk(sq).copy ///it is better to give a copy to prevent modifications of the local parameter axis
        a
      }

      computeParameters(a.map(v => v(nD, 0)))
    }

  }

  /**
   * the linear 'matrix' of transformed points, which consists
   * of the original points qk but placed into the transformed coordinates (u,v)
   */
  lazy val tqk: Seq[DenseMatrix[Double]] = {
    for {
      i <- qk.indices
    } yield {
      val sq = viewQk.fromIndex2Seq(i)
      val m: DenseMatrix[Double] = viewQk(sq).copy
      for {
        n <- sq.indices
      } {
        m(n, 0) = parameterKnots(n)(sq(n))
      }
      m
    }
  }

  lazy val viewTQk = new MultiArrayView(tqk, dim)

}

trait ParameterVectorChord extends ParameterVector {
  self: ControlPoint =>

  // TODO: Fran please document
  protected def computeParameters(axis: Seq[Double]): Seq[Double] = {
    val lengths = for {
      Seq(a, b) <- axis.sliding(2).toSeq
    } yield {
      abs(b - a)
    }

    val normLength = lengths.sum

    val normalizedLengths = lengths.map(_ / normLength)
    val result = normalizedLengths.scanLeft(0d)(_ + _)

    val resultWithEndPointsForced = Seq(0.0) ++ result.slice(1, result.length - 1) ++ Seq(1.0)
    resultWithEndPointsForced
  }

}

trait ParameterVectorCentripetal extends ParameterVector {
  self: ControlPoint =>

  protected def computeParameters(axis: Seq[Double]): Seq[Double] = {
    val sqrt_normLength =
      (for (n <- 1 until axis.length) yield { math.sqrt(axis(n) - axis(n - 1)) }).toSeq.foldLeft(0.0)((acc, v) => acc + v)

    var lastVal = 0.0
    val result =
      Seq(0.0) ++
        (for (n <- 1 until axis.length - 1) yield {
          lastVal = lastVal + math.sqrt(math.abs(axis(n) - axis(n - 1))) / sqrt_normLength
          lastVal
        }).toSeq ++ Seq(1.0)

    result
  }
}

trait ParameterVectorEqually extends ParameterVector {
  self: ControlPoint =>

  protected def computeParameters(axis: Seq[Double]): Seq[Double] = {
    val N = axis.length
    val result: Seq[Double] =

      (for (n <- 0 until axis.length) yield {
        n.toDouble / (N.toDouble - 1)
      }).toSeq

    result
  }

}

