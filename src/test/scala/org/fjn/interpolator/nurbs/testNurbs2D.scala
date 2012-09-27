package org.fjn.interpolator.nurbs

import scala.Array
import javax.swing.{SwingUtilities, JFrame}
import java.awt.BorderLayout
import java.util.Random
import scala.collection.JavaConversions._
import org.fjn.interpolator.common.MultiArrayView
import net.ericaro.surfaceplotter.JSurfacePanel
import org.fjn.matrix.Matrix
import net.ericaro.surfaceplotter.surface.ArraySurfaceModel

object plotting{

  val nSamplesX = 25
  val nSamplesY= 25

  val nSamplesX2=75
  val nSamplesY2=75

  def testSomething(f:Function2[Double,Double,Double],fRef:Function2[Double,Double,Double],dimX:Int,qk:Seq[Matrix[Double]]) {

    val dimY = dimX

    val jsp: JSurfacePanel = new JSurfacePanel
    jsp.setTitleText("NURBS 2D")
    val jf: JFrame = new JFrame("test")
    jf.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    jf.getContentPane.add(jsp, BorderLayout.CENTER)
    jf.pack
    jf.setVisible(true)
    val rand: Random = new Random

    val z1 = Array.ofDim[Float](dimX,dimY)
    val z2 = Array.ofDim[Float](dimX,dimY)


    val vw = new MultiArrayView[Matrix[Double]]( qk ,Seq(dimX,dimY))
    for(i <- 0 until dimX) {
      {
        for (j <- 0 until dimY) {

          val p = vw(Seq(i,j))
          z1(i)(j) = f(p(0,0),p(1,0)).toFloat
          z2(i)(j) = fRef(p(0,0),p(1,0)).toFloat


        }

      }
    }

    val xx = qk.map(p => p(0,0))
    val xMax = xx.max
    val xMin = xx.min

    val yy = qk.map(p => p(1,0))
    val yMax = yy.max
    val yMin = yy.min



    val sm: ArraySurfaceModel = new ArraySurfaceModel
    sm.setValues(xMin.toFloat, xMax.toFloat, yMin.toFloat, yMax.toFloat ,dimX, z1, z2)
    jsp.setModel(sm)
  }

  def psinc:Function2[Double,Double,Double] ={
    (u,v)=>{
      math.sin(math.sqrt(u*u+v*v+1e-4))/math.sqrt(u*u+v*v+1e-4)
    }


  }

  def generateSamples(nSamplesX:Int,nSamplesY:Int):(Seq[Matrix[Double]])={


    val middle = 8
    val a = (for(
      k<- 0 until nSamplesY;
      h <- 0 until nSamplesX
    ) yield {
      val mt = new Matrix[Double](2,1)
      mt.zeros;
      val v1 = -0.5*middle*math.Pi + middle*math.Pi*h.toDouble/(nSamplesX-1)
      val v2 = -0.5*middle*math.Pi + middle*math.Pi*k.toDouble/(nSamplesY-1)
      mt.set(0,0,v1)
      mt.set(1,0,v2)
      mt
    }
      ).toSeq

    a

  }
  def testFunc(bspline:Nurbs2DBase,z:Array[Double],qk:Seq[Matrix[Double]],dim:Int){

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
        val ff =
          (x:Double,y:Double)=> bspline(bspline.getNormalizedCoord(x,0),bspline.getNormalizedCoord(y,1))(2,0)
        testSomething(ff,psinc,dim,qk)
      }
    })

  }
}

object testNurbs2D {


  def main(args:Array[String]){


    import plotting._



    val qk = generateSamples(nSamplesX,nSamplesY)


    val z =(for(q <- qk)
    yield
    {
      val x= q(0,0)
      val y =q(1,0)

      psinc(x,y)
    }).toArray

    val order = 3

    val bspline = new Nurbs2D(qk,Array(order,order),Seq(nSamplesX,nSamplesY))



    val qk2 = generateSamples(nSamplesX2,nSamplesY2)
    testFunc(bspline,z,qk2,nSamplesX2)
  }




}


object testNurbs2DChord {


  def main(args:Array[String]){


    import plotting._


    val qk = generateSamples(nSamplesX,nSamplesY)


    val z =(for(q <- qk)
    yield
    {
      val x= q(0,0)
      val y =q(1,0)

      psinc(x,y)
    }).toArray

    val order = 3

    val bspline = new Nurbs2DChord(qk,Array(order,order),Seq(nSamplesX,nSamplesY),1e-2)



    val qk2 = generateSamples(nSamplesX2,nSamplesY2)
    testFunc(bspline,z,qk2,nSamplesX2)
  }




}


object testNurbs2DCentripetal {


  def main(args:Array[String]){


    import plotting._



    val qk = generateSamples(nSamplesX,nSamplesY)


    val z =(for(q <- qk)
    yield
    {
      val x= q(0,0)
      val y =q(1,0)

      psinc(x,y)
    }).toArray

    val order = 3

    val bspline = new Nurbs2DCentripetal(qk,Array(order,order),Seq(nSamplesX,nSamplesY),1e-2)




    val qk2 = generateSamples(nSamplesX2,nSamplesY2)
    testFunc(bspline,z,qk2,nSamplesX2)
  }




}