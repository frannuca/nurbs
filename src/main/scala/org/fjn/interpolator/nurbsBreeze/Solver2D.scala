//package org.fjn.interpolator.nurbsBreeze
//
//object Solver2D {
//  def SolveOnU(nurbs:Nurbs2D, z: Seq[Double]/*, viewer_norm: MultiArrayView[Matrix[Double]],
//    viewer_original: MultiArrayView[Matrix[Double]], viewerZ: MultiArrayView[Double]*/): Seq[Matrix[Double]] = {
//
//    //Preparing the matrix for constant y-slices:
//    val qXMatrix = new Matrix[Double](dim(0), dim(0))
//    for (i <- 0 until dim(0)) {
//      val uk = parameterKnots(0)(i)
//      for (j <- 0 until dim(0)) {
//
//        val vv = NBasis(j, basisOrder(0), 0)(uk)
//        qXMatrix.set(i, j, vv)
//      }
//    }
//
//    qXMatrix.invert
//
//    val sampleSize = 3 //(x,y,z)
//
//    //Solving linear systems for y index (x,0),(x,1),....,(x,m-1) with x 0 .. n-1
//    //var Rl: Seq[Matrix[Double]] =
//    val futuresOnSystemSolver: Seq[() => Matrix[Double]] =
//      for (l <- 0 until dim(1)) //we have dim(1) dim(1) points in y-direction, which are slices now
//      yield {
//
//        //Building linear system for the kth slice:
//
//        //Basis function matrix:
//
//        //sample vector(right side of the system):
//
//        val f: Function0[Matrix[Double]] = () => {
//          var rightM = new Matrix[Double](dim(0), sampleSize)
//          for (i <- 0 until dim(0)) {
//            val auxPos = Seq(i, l)
//            val posq = viewer_original(auxPos)
//            for (j <- 0 until sampleSize - 1) {
//
//              rightM.set(i, j, posq(j, 0))
//            }
//
//            val auxZ = viewerZ(auxPos)
//            rightM.set(i, sampleSize - 1, auxZ)
//          }
//
//          val auxVal = qXMatrix * rightM
//          auxVal
//
//        }
//        f
//      }
//
//    val result = futuresOnSystemSolver.par.map(f => f())
//    result.seq
//  }
//
//}
