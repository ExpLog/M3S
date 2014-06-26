/**
 * Created by Leonardo Fontoura on 09/05/2014.
 */

import scala.language.implicitConversions

/**
 * Framework for simulating stochastic machines.
 */
package object m3s {
  /**
   * A state is an Int that represents the current state of a [[MarkovChain]]
   */
  type State = Int

  /**
   * Defines a matrix as a vector of vector of doubles.
   */
  type Matrix = Vector[Vector[Double]]

  /**
   * Implicitly converts a file containing a matrix into that [[Matrix]].
   *
   * The file must contain only the matrix in plain text.
   * Each line in the file is a matrix row with spaces between the numbers.
   *
   * For example, this is a 2x2 valid matrix file:
   * {{{
   *   0.2 0.8
   *   0.3 0.4
   * }}}
   * @param s The file path
   * @return The matrix in the file
   */
  implicit def fileToMatrix(s: String): Matrix = {
    val lines: Iterator[String] = scala.io.Source.fromFile(s).getLines()
    val m: Matrix = (for( l <- lines ) yield l.split(" ").map(x => x.toDouble).toVector).toVector
    m
  }

  /**
   * Row-normalizes the input [[Matrix]].
   * @param m A matrix
   * @return Row-normalized matrix
   */
  def rowNorm(m: Matrix): Matrix =
    for( l <- m ) yield {
      val norm = l.sum
      l.map( x => x/norm)
    }

  /**
   * Used to indicate that a matrix is row normalized and only cointains non-negative numbers.
   */
  trait RowNormMatrix {
    val m: Matrix

    def apply(i: Int): Vector[Double] = m(i)

    def apply(i: Int, j: Int): Double = m(i)(j)

    def length : Int = m.length

    override def toString: String = m.toString()
  }

  /**
   * Implicit conversion of a matrix to a [[RowNormMatrix]].
   * @param mtx A [[Matrix]]
   * @return A row normalized matrix
   */
  implicit def matrixToRowNormMatrix(mtx: Matrix) = {
    require(mtx.forall(l => l.length == mtx.length),
      "RowNormMatrix: matrix isn't square")
    require(mtx.forall(x => x.forall( y => y >= 0 )), "RowNormMatrix: negative value in matrix")

    new RowNormMatrix{val m: Matrix = rowNorm(mtx)}
  }
}