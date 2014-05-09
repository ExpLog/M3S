/**
 * Created by Leonardo Fontoura on 06/05/2014.
 */
package m3s

import MarkovChain._
import scala.language.implicitConversions

/**
 * A Markov Chain represented by a transition matrix.
 * @param transProb Square [[Matrix]] with all non-negative values. Its rows will be normalized.
 */
class MarkovChain(transProb: Vector[Vector[Double]]) {
  require(transProb.length*transProb.length == transProb.foldLeft(0){(n,v) => n+v.length},
    "MarkovChain: matrix isn't square")
  require(transProb.forall(x => x.forall( y => y >= 0 )), "MarkovChain: negative value in matrix.")

  /**
   * Row-normalized transition [[Matrix]].
   */
  private val m: Matrix = rowNorm(transProb)

  /**
   * Total number of [[State]]. Note that since the first state is 0,
   * that the state `s = nStates` is not a valid state.
   */
  val nStates: Int = m.length

  /**
   * Computes the next [[State]].
   * @param s Current [[State]] of the chain.
   * @return Next [[State]] of the chain.
   */
  def transition(s: State): State = ???

  override def toString: String = {
    val lines: Vector[String] = transProb.map(x => x.mkString(" "))
    lines.foldLeft("")(_+"\n"+_)
  }
}

/**
 * Helper functions and types for [[MarkovChain]].
 */
object MarkovChain {


  /**
   * Row-normalizes the input matrix.
   * @param m A [[Matrix]]
   * @return Row-normalized [[Matrix]]
   */
  def rowNorm(m: Matrix): Matrix = ???

  /**
   * Implicitly converts a filename into the [[Matrix]] in the file.
   *
   * The file must contain only the matrix in plain text.
   * Each line in the file is a matrix row with spaces between the numbers.
   *
   * For example, this is a 2x2 valid matrix file:
   * {{{
   *   0.2 0.8
   *   0.3 0.4
   * }}}
   * @param s Filename.
   * @return The matrix in the file
   */
  implicit def fileToMatrix(s: String): Matrix = {
    val lines: Iterator[String] = scala.io.Source.fromFile(s).getLines()
    val m: Matrix = (for( l <- lines ) yield l.split(" ").map(x => x.toDouble).toVector).toVector
    m
  }

  /**
   * Factory method for [[MarkovChain]].
   * @param m A square [[Matrix]]
   * @return A Markov Chain defined by m
   */
  def apply(m: Matrix): MarkovChain = new MarkovChain(m)
}