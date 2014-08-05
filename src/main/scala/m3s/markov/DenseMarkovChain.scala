package m3s.markov

import scala.language.implicitConversions
import m3s._

/**
 * A Markov Chain represented by a transition matrix.
 * @param matrix A [[RowNormMatrix]] with all non-negative values. Its rows will be normalized.
 */
class DenseMarkovChain(val matrix: RowNormMatrix) {
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
    def aux(u: Double, acc: Double, j: State): State = {
      val prob = matrix(i)(j)
      if (u < acc + prob) j else aux(u, acc + prob, j + 1)
    }
    aux(rand.nextDouble(), 0.0, 0)
  }

  override def toString: String = {
    val lines: Vector[String] = matrix.m.map(x => x.mkString("[", ", ", "]"))
    lines.mkString("[", ", ", "]")
  }
}

/**
 * Helper functions and random value generation for [[DenseMarkovChain]].
 */
object DenseMarkovChain {
  /**
   * Factory method for [[DenseMarkovChain]].
   * @param m A square [[Matrix]]
   * @return A Markov Chain defined by `m`
   */
  def apply(m: Matrix): DenseMarkovChain = new DenseMarkovChain(m)

  /**
   * Factory method for [[DenseMarkovChain]].
   * @param m A [[RowNormMatrix]]
   * @return A Markov Chain defined by `m`
   */
  def apply(m: RowNormMatrix): DenseMarkovChain = new DenseMarkovChain(m)

  /**
   * Implicit conversion from [[Matrix]] to [[DenseMarkovChain]].
   * @param m Square non-negative matrix.
   * @return
   */
  implicit def matrixToMarkovChain(m: Matrix) = new DenseMarkovChain(m)

  /**
   * Implicit conversion from [[RowNormMatrix]] to [[DenseMarkovChain]].
   * @param m
   * @return
   */
  implicit def rowNormMatrixToMarkovChain(m: RowNormMatrix) = new DenseMarkovChain(m)
}