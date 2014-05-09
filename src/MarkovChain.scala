/**
 * Created by Leonardo Fontoura on 06/05/2014.
 */
package m3s

import MarkovChain._
import scala.language.implicitConversions

/**
 * A Markov Chain represented by a transition matrix.
 * @param transProb Square [[MarkovChain.Matrix]] with all non-negative values. Its rows will be normalized.
 */
class MarkovChain(transProb: Vector[Vector[Double]]) {
  require(transProb.length*transProb.length == transProb.foldLeft(0){(n,v) => n+v.length},
    "MarkovChain: matrix isn't square")
  require(transProb.forall(x => x.forall( y => y >= 0 )), "MarkovChain: negative value in matrix.")

  /**
   * Row-normalized transition [[MarkovChain.Matrix]].
   */
  private val m: Matrix = rowNorm(transProb)

  /**
   * Total number of [[MarkovChain.State]]. Note that since the first state is 0,
   * that the state `s = nStates` is not a valid state.
   */
  val nStates: Int = m.length

  /**
   * Computes the next [[MarkovChain.State]].
   * @param s Current [[MarkovChain.State]] of the chain.
   * @return Next [[MarkovChain.State]] of the chain.
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
   * Defines a matrix as a vector of vector of doubles.
   */
  type Matrix = Vector[Vector[Double]]

  /**
   * A state is an Int that represents the current state of a [[MarkovChain]]
   */
  type State = Int

  /**
   * Row-normalizes the input matrix.
   * @param m A [[MarkovChain.Matrix]]
   * @return Row-normalized [[MarkovChain.Matrix]]
   */
  def rowNorm(m: Matrix): Matrix = ???

  /**
   * Implicitly converts a filename into the [[MarkovChain.Matrix]] in the file.
   * @param s Filename.
   * @return [[MarkovChain.Matrix]]  in the file
   */
  implicit def fileToMatrix(s: String): Matrix = {
    val lines: Iterator[String] = scala.io.Source.fromFile(s).getLines()
    val m: Matrix = (for( l <- lines ) yield l.split(" ").map(x => x.toDouble).toVector).toVector
    m
  }

  /**
   * Factory method for [[MarkovChain]]
   * @param m A square [[MarkovChain.Matrix]]
   * @return A [[MarkovChain]] defined by m
   */
  def apply(m: Matrix): MarkovChain = new MarkovChain(m)
}