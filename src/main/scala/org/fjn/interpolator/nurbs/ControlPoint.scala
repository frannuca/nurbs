package org.fjn.interpolator.nurbs

import org.fjn.interpolator.common.{Point, MultiArrayView}


/**
 * Trait hosting the list grid
 */
trait ControlPoint {

  /**list of point along x coordinate*/
  val xAxis:Seq[Double]

  /**list of points along x coordinate */
  val yAxis:Seq[Double]

  /**list of z-values distributed as series of xAxis.length slices zval(i,j)=zValue(j*xArray.length+i)*/
  val zValues:Seq[Double]

  /** Computed list of 3D points composing the grid x,y,z*/
  protected lazy val qk =
    for(yc <- 0 until yAxis.length;
        xc <- 0 until xAxis.length
       ) yield{
      new Point[Double](x=xAxis(xc),y=yAxis(yc),z=zValues(yc*xAxis.length+xc))
    }

}
