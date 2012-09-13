package org.fjn.interpolator.nurbs

import java.io.{InputStreamReader, BufferedReader}
import org.fjn.interpolator.common.matrix._
import collection.immutable.IndexedSeq
import collection.immutable

/**
 * Created with IntelliJ IDEA.
 * User: fran
 * Date: 9/11/12
 * Time: 7:25 PM
 * To change this template use File | Settings | File Templates.
 */
object testNurbs1D {

  def main(args:Array[String]){


    def func(x:Double):Double={
      math.sin(x*math.Pi)
    }



    val qk = (0 until 10).map(n => {
      val x = n.toDouble/10.0
      val z=func(x)
      val m = new Matrix[Double](2,1)
      m.set(0,0,x)
      m.set(1,0,z)
      m
    })



    val order = 1
    val xAxis: immutable.Seq[Matrix[Double]] = qk.map(v =>{
      val o = new Matrix[Double](1,1);
      o.set(0,0,v(0,0))
      o}
      ).toSeq
    val bspline = new Nurbs1D(xAxis, Seq(order))
    bspline.solve(qk.map(v => v(1,0)).toArray);


    //check values
    val diff = (0 until 100).map(n=>{
      val x = n.toDouble/100.0
      val z=func(x)
      val tx = bspline.getNormalizedCoord(x)
      val zNurb = bspline(tx)
      math.abs(z-zNurb(1,0))
    })





    val br = new BufferedReader(new InputStreamReader(System.in));
    br.readLine()

  }

}
