package org.fjn.interpolator.nurbs
import collection.{immutable, Seq}


/**
 * this trait hosts the vector knot which contains the list of parameter per coordinate
 *
 */
trait KnotsVector {
  self: ParameterVector with BasisFunctionOrder=>

  def computeKnots(params: Seq[Double],p:Int): Seq[Double] = {

    val N = params.length-1
    val a: Seq[Double] = (0 to p).map(i => 0.0d).toSeq ++
      (1 to N-p).map(i=>{

        val s1 = i+p+1
        val s0 =  i

        val numberOfItems = p+1
        val auxSq2  = params.slice(s0,s1)
        val auxSq  = auxSq2 ++ (auxSq2.length until p+1).map(x => 1.0d)

        val v = auxSq.foldLeft(0.0d)((acc,v)=> acc+v)*1.0/numberOfItems.toDouble

        v
      }).toSeq ++
      (0 to p).map(i => 1.0d).toSeq


    a
  }


  lazy val knotsVector: Seq[Seq[Double]] =
    (0 until self.parameterKnots.length).map(p => computeKnots(self.parameterKnots(p),basisOrder(p))).toSeq
}