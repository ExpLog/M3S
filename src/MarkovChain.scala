/**
 * Created by Leonardo Fontoura on 06/05/2014.
 */
package m3s

import MarkovChain._

/**
 * A Markov Chain represented by a matrix.
 * @param transProb Must be square with all non-negative values. Its rows will be normalized.
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
  def transition(s: State): State = ???

  override def toString: String = {
    val lines: Vector[String] = transProb.map(x => x.mkString(" "))
    lines.foldLeft("")(_+"\n"+_)
  }
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
   * A [[m3s.MarkovChain.State]] is an Int that represents the current state of a [[m3s.MarkovChain]]
   */
  type State = Int

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
  implicit def stringToMatrix(s: String): Matrix = {
    val lines: Iterator[String] = scala.io.Source.fromFile(s).getLines()
    val m: Matrix = (for( l <- lines ) yield l.split(" ").map(x => x.toDouble).toVector).toVector
    m
  }

  /**
   * Factory method for [[m3s.MarkovChain]]
   * @param m A square [[m3s.MarkovChain.Matrix]]
   * @return A [[m3s.MarkovChain]] defined by m
   */
  def apply(m: Matrix): MarkovChain = new MarkovChain(m)
}