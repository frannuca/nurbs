package org.fjn.interpolator.nurbs

import _root_.net.ericaro.surfaceplotter.JSurfacePanel
import _root_.net.ericaro.surfaceplotter.surface.ArraySurfaceModel
import org.fjn.interpolator.common.matrix.Matrix
import scala.Array
import javax.swing.{SwingUtilities, JFrame}
import java.awt.BorderLayout
import java.util.Random
import scala.collection.JavaConversions._

object testNurbs2D {

  def testSomething(f:Function2[Double,Double,Double],max:Int) {
    val jsp: JSurfacePanel = new JSurfacePanel
    jsp.setTitleText("Hello")
    val jf: JFrame = new JFrame("test")
    jf.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    jf.getContentPane.add(jsp, BorderLayout.CENTER)
    jf.pack
    jf.setVisible(true)
    val rand: Random = new Random

    val z1 = Array.ofDim[Float](max,max)
    val z2 = Array.ofDim[Float](max,max)


      for(i <- 0 until max) {
        {


            for (j <- 0 until max) {

               val x = 0.5*math.Pi*i.toDouble/max
               val y = 0.5*math.Pi*j.toDouble/max

                z1(i)(j) = f(x,y).toFloat
                z2(i)(j) = psinc(x.toFloat,y.toFloat) .toFloat


            }

        }


    }
    val sm: ArraySurfaceModel = new ArraySurfaceModel
    sm.setValues(0f, 0.5f*math.Pi.toFloat, 0f, 0.5f*math.Pi.toFloat, max, z1, z2)
    jsp.setModel(sm)
  }
  def psinc:Function2[Double,Double,Double] ={
    (u,v) => math.sin(3.0*v*math.Pi)// math.sin(math.sqrt(u*u+v*v+1e-4))/math.sqrt(u*u+v*v+1e-4)
    //(u,v)=> 8.0
  }


  def generateSamples(nSamplesX:Int,nSamplesY:Int):(Seq[Matrix[Double]])={


      val a = (for(
        k<- 0 until nSamplesY;
        h <- 0 until nSamplesX
      ) yield {
        val mt = new Matrix[Double](2,1)
        mt.zeros;
        mt.set(0,0,0.5*math.Pi*h.toDouble/nSamplesX)
        mt.set(1,0,0.5*math.Pi*k.toDouble/nSamplesY)
        mt
      }
        ).toSeq

           a

  }
  def main(args:Array[String]){




    val nSamplesX = 5
    val nSamplesY= 5
    val qk = generateSamples(nSamplesX,nSamplesY)


    val z =(for(q <- qk)
    yield
    {
      val x= q(0,0)
      val y =q(1,0)

      psinc(x,y)
    }).toArray

    val order = 2

    val bspline = new Nurbs2D(qk,Array(order,order),Seq(nSamplesX,nSamplesY))


    val nSamplesX2=5
    val nSamplesY2=5
    val qk2 = generateSamples(nSamplesX2,nSamplesY2)
    testFunc(bspline,z,qk2)
  }

  def testFunc(bspline:Nurbs2DBase,z:Array[Double],qk:Seq[Matrix[Double]]){

      bspline.solve(z);

      var sumError =
      qk.par.map( item =>{
        val u = bspline.getNormalizedCoord( item(0,0),0)
        val v = bspline.getNormalizedCoord( item(1,0),1)
        val ax = bspline(u,v)
        val x = ax(0,0)
        val y = ax(1,0)
        val z = ax(2,0)
        val r = psinc(item(0,0),item(1,0))


        math.abs(z-r)
      })


      println("Total error ="+sumError.max)

    SwingUtilities.invokeLater(new Runnable {
      def run {
        testSomething((x:Double,y:Double)=> bspline(bspline.getNormalizedCoord(x,0),bspline.getNormalizedCoord(y,1))(2,0),40)
      }
    })

    }


}
