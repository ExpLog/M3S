import m3s._
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import Generators._

class RowNormMatrixTest extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  "A row norm matrix" should "be row normalized" in {
    forAll(rowNormMatrix)( mtx => mtx.m.forall(_.sum == 1.0))
  }

  it should "be square" in {
    forAll(rowNormMatrix){ mtx =>
      val row = mtx.length
      (for(col <- mtx.m) yield row == col.length) forall(_ == true)
    }
  }

  it should "throw an exception if it isn't square" in {
    val mtx = Vector(Vector(1.0,2.0), Vector(1.0))
    a [Exception] should be thrownBy {
      matrixToRowNormMatrix(mtx)
    }

    the [Exception] thrownBy matrixToRowNormMatrix(mtx) should have message "requirement failed: RowNormMatrix: matrix isn't square"
  }

  it should "not contain negative entries" in {
    forAll(rowNormMatrix){ mtx =>
      mtx.m forall(row => row forall(v => v >= 0))
    }
  }

  it should "throw an exception if it contains a negative entry" in {
    val mtx: Matrix = Vector(Vector(-1.0))
    a [Exception] should be thrownBy {
      matrixToRowNormMatrix(mtx)
    }

    the [Exception] thrownBy matrixToRowNormMatrix(mtx) should have message "requirement failed: RowNormMatrix: negative value in matrix"
  }
}