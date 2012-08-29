package org.fjn.interpolator.nurbs

import org.fjn.interpolator.common.Point
import collection.immutable.IndexedSeq
import collection.immutable

/**
 * Created by fjn army of one.
 * User: fran
 * Date: 8/29/12
 * Time: 7:19 PM
 */
/**
 * This trait computes the location of the control points into the target nurbs space
 * The control points are computed per coordinate given the list of input points qk
 * We can think of the control points as the mapping from real-space points into the normalized nurbs space points
 * This trait needs to be extended in order to compute the location in the normalized space. Typically three
 * methods are used: Centripetal, Chord and Equally distributed
 */
trait ParameterVector {
  self: ControlPoint =>



  protected def computParameters(axis:Seq[Double]):Seq[Double]



  lazy val parameterKnots_x: Seq[Double] = computParameters(xAxis)
  lazy val parameterKnots_y: Seq[Double] = computParameters(yAxis)

  /**Calculating the final parameter knots associated to the sequence of points qk
     with the Chord method*/
  val parameterKnots = Seq(parameterKnots_x,parameterKnots_y)


  /**
     * the linear 'matrix'  of transformed points, which consists
     * of the original points qk but placed into the transformed coordinates (u,v)
     */

   lazy val tqk: Seq[Point[Double]] =
     (for (yn <- 0 until parameterKnots_y.length;
          xn <- 0 until parameterKnots_x.length)
       yield{
        new Point[Double](x = parameterKnots_x(yn),y = parameterKnots_y(yn), z = zValues(yn*parameterKnots_x.length+xn) )
     } ) toSeq

}

trait ParameterVectorChord extends ParameterVector {
  self: ControlPoint =>

  protected def computParameters(axis:Seq[Double]):Seq[Double]={
    val normLength =
              (for (n <- 1 until axis.length)
              yield{math.abs(axis(n) - axis(n - 1))}).toSeq.foldLeft(0.0)((acc, v) => acc + v)


            var lastVal = 0.0
            val result =
              Seq(0.0) ++
            (for (n <- 1 until axis.length-1)
             yield {
               lastVal = lastVal + math.abs(math.abs(axis(n) - axis(n-1))) / normLength
               lastVal
            }).toSeq ++ Seq(1.0)

    result
  }



}


trait ParameterVectorCentripetal extends ParameterVector {
  self: ControlPoint =>

  protected def computParameters(axis:Seq[Double]):Seq[Double]={
    val sqrt_normLength =
              (for (n <- 1 until axis.length)
              yield{math.sqrt(axis(n) - axis(n - 1))}).toSeq.foldLeft(0.0)((acc, v) => acc + v)


            var lastVal = 0.0
            val result =
              Seq(0.0) ++
            (for (n <- 1 until axis.length-1)
             yield {
               lastVal = lastVal + math.sqrt(math.abs(axis(n) - axis(n-1))) / sqrt_normLength
               lastVal
            }).toSeq ++ Seq(1.0)

    result
  }
}


  trait ParameterVectorEqually extends ParameterVector {
    self: ControlPoint =>

    protected def computParameters(axis:Seq[Double]):Seq[Double]={
              val N = axis.length
              val result: Seq[Double] =
                Seq(0.0) ++
              (for (n <- 1 until axis.length-1)
               yield {
                 n.toDouble / N.toDouble
              }).toSeq ++ Seq(1.0)

      result
    }


}

