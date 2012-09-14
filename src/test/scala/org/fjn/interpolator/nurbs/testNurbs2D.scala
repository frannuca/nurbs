package org.fjn.interpolator.nurbs

import org.fjn.interpolator.common.matrix.Matrix
import scala.Array


object testNurbs2D {

  def psinc:Function2[Double,Double,Double] =
    (u,v) => math.sin(math.sqrt(u*u+v*v+1e-4))/math.sqrt(u*u+v*v+1e-4)

  def generateSamples(nSamplesX:Int,nSamplesY:Int):(Seq[Matrix[Double]],Seq[Double],Seq[Double])={

    var xAxis = Seq[Double]()
    var yAxis = Seq[Double]()

      ((for(
        k<- 0 until nSamplesY;
        h <- 0 until nSamplesX
      ) yield {
        val mt = new Matrix[Double](2,1)
        mt.zeros;
        xAxis = xAxis ++ Seq(3.0*h.toDouble/nSamplesX)
        yAxis =yAxis ++ Seq( 3.0*k.toDouble/nSamplesY)
        mt.set(0,0,3.0*h.toDouble/nSamplesX)
        mt.set(1,0,3.0*k.toDouble/nSamplesY)
        mt
      }
        ).toSeq,xAxis,yAxis)



  }
  def main(args:Array[String]){




    val nSamplesX = 15
    val nSamplesY= 12
    val (qk,xAxis,yAxis) = generateSamples(nSamplesX,nSamplesY)


    val z =(for(q <- qk)
    yield
    {
      val x= q(0,0)
      val y =q(1,0)

      psinc(x,y)
    }).toArray

    val order = 2
    val bspline = new Nurbs2D(xAxis.toList.distinct.sortWith((a,b)=> a < b),yAxis.toList.distinct.sortWith((a,b)=> a < b),Array(order,order),Seq(nSamplesX,nSamplesY))

    val nSamplesX2=20
    val nSamplesY2=25
    val (qk2,x,y) = generateSamples(nSamplesX2,nSamplesY2)
    testFunc(bspline,z,qk2)
  }

  def testFunc(bspline:Nurbs2DBase,z:Array[Double],qk:Seq[Matrix[Double]]){

      bspline.solve(z);

      var sumError = 0.0d
      for(item <- qk)
      {
        val u = bspline.getNormalizedCoord( item(0,0),0)
        val v = bspline.getNormalizedCoord( item(1,0),1)
        val ax = bspline(u,v)
        val x = ax(0,0)
        val y = ax(1,0)
        val z = ax(2,0)
        val r = psinc(item(0,0),item(1,0))
        sumError = sumError + math.abs(z-r)
        if(math.abs(z-r)>1e-2)
        {
          println("Error in the solved system larger than 1e-3"+"Expected "+r+" obtained "+z)

        }
        else if(math.abs(x-item(0,0))>1e-2)
        {
          println("Error in the solved system larger than 1e-3"+"Expected "+x+" obtained "+item(0,0))
        }
        else if(math.abs(y-item(1,0))>1e-2)
        {
          println("Error in the solved system larger than 1e-3"+"Expected "+y+" obtained "+item(1,0))
        }


      }

      println("Total error ="+sumError)

    }


}
