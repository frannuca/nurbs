package org.fjn.interpolator.nurbs
import collection.{immutable, Seq}


/**
 * this trait hosts the vector knot which contains the list of parameter per coordinate
 *
 */
trait KnotsVector {
  self: ParameterVector with BasisFunctionOrder=>

  def computeKnots(params: Seq[Double],p:Int): Seq[Double] = {

    val a: Seq[Double] = (0 until p+1).map(i => 0.0d).toSeq ++
      (0 until params.length).map(i=>{
        val numberOfItems = math.min(params.length-1,i+p+1)-math.max(0,i-p-1)
        val v = params.slice(math.max(0,i-p-1),math.min(params.length-1,i+p+1)).foldLeft(0.0d)((acc,v)=> acc+v)*1.0/numberOfItems.toDouble
        v
      }).toSeq ++
      (0 until p).map(i => 1.0d).toSeq

    a
  }


  lazy val knotsVector: Seq[Seq[Double]] =
    (0 until self.parameterKnots.length).map(p => computeKnots(self.parameterKnots(p),basisOrder(p))).toSeq
}