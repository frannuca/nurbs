package  org.fjn.interpolator.common.matrix

import scala.Array
import java.util.{HashMap, ArrayList}
import scala.collection.JavaConverters._

import Complex._

class Matrix[T1](nRows: Int, nCols: Int, isRowMajor: Boolean = false)(implicit m2: Manifest[T1], implicit val m: Fractional[T1]) {

  outer =>


  type DataType = T1

  def getArray(): Array[T1] = data

  private var data: Array[T1] = new Array[T1](nCols * nRows)

  def apply(row: Int, col: Int): T1 = {

    val index = isRowMajor match {
      case true => col + row * numberCols
      case false => row + col * numberRows
    }

    data(index)
  }

  def getColArray(i: Int): Array[T1] = {

    val start = if (isRowMajor) i else i * this.numberRows
    val step = if (isRowMajor) this.numberCols else 1
    var counter = 0
    val rArr = new Array[T1](this.numberRows)
    while (counter < this.numberRows) {
      rArr(counter) = data(start + step * counter)
      counter = counter + 1
    }
    rArr
  }


  def getColArrayIndexer(i: Int): (Int, Int) = {
    (if (isRowMajor) i else i * this.numberRows, if (isRowMajor) this.numberCols else 1)
  }


  trait MatrixLineIterator extends Iterator[T1] {
    protected var position: Int = -1
    protected val start: Int
    protected val step: Int
    val maxCount: Int

    def hasNext: Boolean = {
      (position + 1) < maxCount
    }


    def next: T1 = {
      try {
        position = position + 1
        outer.data(start + (position) * step)
      } catch {
        case e: Exception => {
          println(e.toString)
          m.zero
        };


      }


    }

    def reset(): Unit = {
      position = -1
      Unit
    }

  }

  class RowsIterator(row: Int) extends MatrixLineIterator {
    val (a, b) = getRowArrayIndexer(row)
    val start = a;
    val step = b
    val maxCount = outer.numberCols
  }

  class ColsIterator(col: Int) extends MatrixLineIterator {
    val (a, b) = getColArrayIndexer(col)
    val start = a;
    val step = b
    val maxCount = outer.numberCols
  }

  def getColArrayIterator(i: Int): MatrixLineIterator = {
    new ColsIterator(i)
  }


  def getRowArray(j: Int): Array[T1] = {

    val start = if (isRowMajor) j * this.numberCols else j
    val step = if (isRowMajor) 1 else this.numberRows
    var counter = 0
    val rArr = new Array[T1](this.numberCols)
    while (counter < this.numberCols) {
      rArr(counter) = data(start + step * counter)
      counter = counter + 1
    }
    rArr
  }

  def getRowArrayIndexer(j: Int): (Int, Int) = {
    (if (isRowMajor) j * this.numberCols else j, if (isRowMajor) 1 else this.numberRows)
  }

  def getRowArrayIterator(i: Int): MatrixLineIterator = {
    new RowsIterator(i)
  }

  val numberRows = nRows
  val numberCols = nCols

  def zeros = {
    var i: Int = 0;
    while (i < numberRows) {
      var j: Int = 0
      while (j < numberCols) {
        this.set(i, j, m.zero)
        j += 1
      }
      i = i + 1
    }
  }

  def eye = {
    zeros
    val limit = math.min(numberRows, numberCols)
    var i = 0
    while (i < limit) {
      this.set(i, i, m.one)
      i = i + 1

    }
  }


  def set[T2 <% T1](row: Int, col: Int, v: T2): Unit = {

    val index = isRowMajor match {
      case true => col + row * numberCols
      case false => row + col * numberRows
    }

    data(index) = v

    Unit
  }


  def *(a: T1): Matrix[T1] = {

    val rMatrix: Matrix[T1] = new Matrix[T1](this.numberRows, this.numberCols);
    var i = 0
    while (i < rMatrix.data.length) {
      rMatrix.data(i) = m.times(this.data(i), a)
      i = i + 1
    }

    rMatrix
  }

  def +(b: T1): Matrix[T1] = {
    val resM = this.clone()
    var i = 0;
    while (i < data.length) {
      resM.data(i) = m.plus(data(i), b)
      i = i + 1
    }
    resM
  }

  def +(b: Matrix[T1]): Matrix[T1] = {
    require(this.numberCols == b.numberCols && this.numberRows == b.numberRows)
    val rMatrix: Matrix[T1] = new Matrix[T1](this.numberRows, this.numberCols);
    var i = 0
    var j = 0
    while (i < rMatrix.numberRows) {
      j = 0

      val iterator2 = b.getRowArrayIterator(i)
      val iterator1 = this.getRowArrayIterator(i)
      while (j < rMatrix.numberCols) {
        rMatrix.set(i, j, m.plus(iterator1.next, iterator2.next))
        j = j + 1
      }
      i = i + 1
    }

    rMatrix

  }

  def -(b: Matrix[T1]): Matrix[T1] = {
    require(this.numberCols == b.numberCols && this.numberRows == b.numberRows)
    val rMatrix: Matrix[T1] = new Matrix[T1](this.numberRows, this.numberCols);
    var i = 0
    var j = 0
    while (i < rMatrix.numberRows) {
      j = 0

      val iterator2 = b.getRowArrayIterator(i)
      val iterator1 = this.getRowArrayIterator(i)
      while (j < rMatrix.numberCols) {
        rMatrix.set(i, j, m.minus(iterator1.next, iterator2.next))
        j = j + 1
      }
      i = i + 1
    }

    rMatrix

  }


  override def clone(): Matrix[T1] = {
    val out = new Matrix[T1](this.numberRows, this.numberCols)
    outer.data.copyToArray(out.data)
    out
  }

  def unary_+ : Matrix[T1] = {
    val out = new Matrix[T1](this.numberRows, this.numberCols)
    this.data.copyToArray(out.data)
    out
  }

  def unary_- : Matrix[T1] = {
    val out = new Matrix[T1](this.numberRows, this.numberCols)
    out.data = this.data.map(x => m.minus(m.zero, x))
    out
  }

  def *(b: Matrix[T1]): Matrix[T1] = {
    case class RowColMsg(rowIndex: Int, colIndex: Int, a1: Array[T1], a2: Array[T1])

    require(this.numberCols == b.numberRows)
    val rMatrix: Matrix[T1] = new Matrix[T1](this.numberRows, b.numberCols);
    var i = 0
    var j = 0


      var k = 0
      while (i < rMatrix.numberRows) {

        j = 0
        val iterator1 = outer.getRowArrayIterator(i)
        while (j < b.numberCols) {
          val iterator2 = b.getColArrayIterator(j)
          var r: T1 = m.zero

          k = 0
          while (k < b.numberRows) {
            r = m.plus(r, m.times(iterator1.next, iterator2.next))
            k = k + 1
          }

          rMatrix.set(i, j, r)
          j = j + 1
          iterator1.reset
        }
        i = i + 1
      }






    rMatrix
  }

  override def toString = {
    val rStr = new StringBuilder
    var i = 0;
    var j = 0;
    while (i < numberRows) {
      j = 0
      while (j < numberCols) {
        rStr.append(this.apply(i, j))
        rStr.append(",")
        j = j + 1;
      }
      rStr.append('\n')
      i = i + 1
    }

    rStr.toString()

  }

  def invert(): Unit = {
    //MatrixInterface.invert(data.asInstanceOf[Array[Double]],this.numberCols)
    import Jama.{_}

    val A: Jama.Matrix = new Jama.Matrix(data.asInstanceOf[Array[Double]], this.numberCols);
    val I = Jama.Matrix.identity(this.numberRows, this.numberCols);
    val s = A.solve(I);
    this.data = s.getColumnPackedCopy().asInstanceOf[Array[T1]]
    //650321210
  }
}


