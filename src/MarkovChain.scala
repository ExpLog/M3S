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
 * Helpers for [[m3s.MarkovChain]].
 */
object MarkovChain {
  /**
   * Defines a Matrix as a Vector of Double Vectors.
   */
  type Matrix = Vector[Vector[Double]]

  /**
   * Row-normalize the input matrix.
   * @param m A [[m3s.MarkovChain.Matrix]]
   * @return Row-normalized [[m3s.MarkovChain.Matrix]]
   */
  def normalize(m: Matrix): Matrix = ???

  /**
   * Converts a String representing a filename into the [[m3s.MarkovChain.Matrix]]  in the file.
   * @param s Filename.
   * @return [[m3s.MarkovChain.Matrix]]  in the file
   */
  implicit def fileToMatrix(s: String): Matrix = ???

  /**
   * Factory method for [[m3s.MarkovChain]]
   * @param m A square [[m3s.MarkovChain.Matrix]]
   * @return A [[m3s.MarkovChain]] defined by m
   */
  def apply(m: Matrix): MarkovChain = ???
}