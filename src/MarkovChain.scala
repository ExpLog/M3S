/**
 * Created by Leonardo Fontoura on 06/05/2014.
 */
package m3s

import MarkovChain._

/**
 * A Markov Chain represented by a matrix.
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

/**
 * Helpers for [[MarkovChain]].
 */
object MarkovChain {
  /**
   * Defines a Matrix as a Vector of Double Vectors.
   */
  type Matrix = Vector[Vector[Double]]

  /**
   * Row-normalize the input matrix.
   * @param m A [[Matrix]]
   * @return Row-normalized [[Matrix]]
   */
  def normalize(m: Matrix): Matrix = ???

  /**
   * Converts a String representing a filename into the [[Matrix]] in the file.
   * @param s Filename.
   * @return [[Matrix]] in the file
   */
  implicit def fileToMatrix(s: String): Matrix = ???

  /**
   * Factory method for [[MarkovChain]]
   * @param m A square [[Matrix]]
   * @return A [[MarkovChain]] defined by m
   */
  def apply(m: Matrix): MarkovChain = ???
}