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


    val order = 2
    val xAxis: immutable.Seq[Matrix[Double]] = qk.map(v =>{
      val o = new Matrix[Double](1,1);
      o.set(0,0,v(0,0))
      o}
    ).toSeq
    //Equally:

    def testFunc(bspline:Nurbs1DBase){

      bspline.solve(qk.map(v => v(1,0)).toArray);


      //check values
      val diff = (0 until 1500).map(n=>{
        val x = n.toDouble/1500.0
        val z=func(x)
        val tx = bspline.getNormalizedCoord(x)
        val zNurb = bspline(tx)
        math.abs(z-zNurb(1,0))

      })





      println("max diff="+diff.max.toString())

    }

    val bspline = new Nurbs1DEqually(xAxis, Seq(order),1.0,0.0)
    testFunc(bspline)

    //Chord:
    val bspline2 = new Nurbs1DChord(xAxis, Seq(order),1e-2)
    testFunc(bspline2)

    val bspline3 = new Nurbs1DCentripetal(xAxis, Seq(order),1e-2)
    testFunc(bspline3)


  }

}
