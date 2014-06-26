/**
 * Created by Leonardo Fontoura on 12/05/2014.
 */

import m3s.MarkovChain
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.{Gen, Prop}
import Generators._

class MarkovChainTest extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  "A markov chain" should "transition to a valid state" in {
    Prop.forAll(markovChain){
      mtx => Prop.forAll(Gen.choose(1,mtx.nStates)){
        i =>
          val state = mtx.transition(i)
          state >= 0 && state < mtx.nStates
      }
    }
  }

  it should "be created implicitly from a non-negative square matrix" in {
    Prop.forAll(squareMatrix){
      mtx =>
        val mc: MarkovChain = mtx
        mc.isInstanceOf[MarkovChain]
    }
  }

  it should "be created implicitly from a RowNormMatrix" in {
    Prop.forAll(rowNormMatrix){
      mtx =>
        val mc: MarkovChain = mtx
        mc.isInstanceOf[MarkovChain]
    }
  }
}
