import org.scalatest.Matchers
import org.scalatest.FunSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks

/*
Just a friendly reminder for the future:
To use ScalaTest with Scala-2.11, we need to add both
scala-combinators and scala-xml libs to the project

TODO: http://www.scalatest.org/user_guide/property_based_testing
*/

class RowNormMatrixTest extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  describe("We can use test data from Scala check") {
    it("runs the same but with different constructs") {
      forAll {
        (a: Int, b: Int) =>
          whenever(b > 14) {(a + b) should be(b + a)}
      }
    }
  }
}