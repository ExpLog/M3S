/**
 * Created by Leonardo Fontoura on 06/05/2014.
 */
package main.scala.m3s

import MarkovChain._
import scala.util.Random
import scala.language.implicitConversions

/**
 * A Markov Chain represented by a transition matrix.
 * @param matrix A [[RowNormMatrix]] with all non-negative values. Its rows will be normalized.
 */
class MarkovChain(matrix: RowNormMatrix) {
  /**
   * Total number of [[State]]. Note that since the first state is 0,
   * that the state `s = nStates` is not a valid state.
   */
  val nStates: Int = matrix.length

  /**
   * Computes the next state of the chain given the current state.
   * @param i Current state of the chain
   * @return Next state of the chain.
   */
  def transition(i: State): State = {
    def aux(db: Double, acc: Double, j: State): State = {
      val prob = matrix(i)(j)
      if(db < acc + prob) j else aux(db, acc + prob, j+1)
    }
    aux(rand.nextDouble(), 0.0, 0)
  }

  override def toString: String = {
    val lines: Vector[String] = matrix.m.map(x => x.mkString(" "))
    lines.foldLeft("")(_+"\n"+_)
  }
}

/**
 * Helper functions and random value generation for [[MarkovChain]].
 */
object MarkovChain {
   /**
   * Factory method for [[MarkovChain]].
   * @param m A square [[Matrix]]
   * @return A Markov Chain defined by `m`
   */
  def apply(m: Matrix): MarkovChain = new MarkovChain(m)

  /**
   * Random number generator used to calculate transitions.
   */
  val rand: Random = new Random(System.currentTimeMillis)
}