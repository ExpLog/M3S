/**
 * Created by Leonardo Fontoura on 06/05/2014.
 */

import MarkovChain._

/**
 * A Markov Chain represent by a matrix.
 * @param transProb Must be square and its rows will be normalized.
 */
class MarkovChain(transProb: Vector[Vector[Double]]) {
  require(transProb.length*transProb.length == transProb.foldLeft(0){(n,v) => n+v.length})  //matrix is square

  /**
   * Row-normalized transition matrix.
   */
  private val m : Matrix = normalize(transProb)

  /**
   * Computes the next state.
   * @param s Current state of the chain.
   * @return Next state of the chain.
   */
  def transition(s: Int): Int = ???
}


object MarkovChain {
  /**
   * Defines a Matrix as a Vector of Double Vectors.
   */
  type Matrix = Vector[Vector[Double]]

  /**
   * Normalizes the rows of a matrix (i.e. each row will sum to 1).
   */
  def normalize(m: Matrix): Matrix = ???
}