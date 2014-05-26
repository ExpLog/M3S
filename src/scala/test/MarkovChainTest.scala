/**
 * Created by Leonardo Fontoura on 12/05/2014.
 */

package scala.test

import org.scalatest.Matchers
import org.scalatest.FunSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.Gen

/*
Just a friendly reminder for the future:
To use ScalaTest with Scala-2.11, we need to add both
scala-combinators and scala-xml libs to the project

TODO: http://www.scalatest.org/user_guide/property_based_testing
*/

class MarkovChainTest extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  describe("We can use test data from Scala check") {
    it("runs the same but with different constructs") {
      forAll {
        (a: Int, b: Int) =>
          whenever(b > 14) {(a + b) should be(b + a)}
      }
    }
  }
}
