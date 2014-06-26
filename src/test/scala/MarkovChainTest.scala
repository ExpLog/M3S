import m3s.MarkovChain
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.Gen
import Generators._

class MarkovChainTest extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  "A markov chain" should "transition to a valid state" in {
    forAll(markovChain){
      mtx => forAll(Gen.choose(0,mtx.nStates-1)){
        i =>
          val state = mtx.transition(i)
          state >= 0 && state < mtx.nStates
      }
    }
  }

  it should "be created implicitly from a non-negative square matrix" in {
    forAll(squareMatrix){
      mtx =>
        val mc: MarkovChain = mtx
        mc.isInstanceOf[MarkovChain]
    }
  }

  it should "be created implicitly from a RowNormMatrix" in {
    forAll(rowNormMatrix){
      mtx =>
        val mc: MarkovChain = mtx
        mc.isInstanceOf[MarkovChain]
    }
  }
}
