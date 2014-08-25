package m3s.parsers

import scala.util.parsing.combinator._
import m3s.markov.DenseMarkovChain
import m3s._

trait M3SParsers extends JavaTokenParsers {
  /**
   * Parser for Vector of Doubles.
   * @return
   */
  def vectorDoubleParser: Parser[Vector[Double]] =
    "Vector(" ~> floatingPointNumber ~ rep("," ~> floatingPointNumber) <~ ")" ^^ {
      case d ~ lst => {d.toDouble :: lst.map{x => x.toDouble}}.toVector
    }

  /**
   * Parser for [[m3s.Matrix]].
   * @return
   */
  def matrixParser: Parser[Matrix] =
    "Vector(" ~> vectorDoubleParser ~ rep("," ~> vectorDoubleParser) <~ ")" ^^ {
      case vec ~ lst => {vec :: lst}.toVector
    }

  /**
   * Auxiliary parser for [[DenseMarkovChain]].
   * @return
   */
  def rowParser: Parser[Vector[Double]] =
    "Row(" ~> floatingPointNumber ~ rep("," ~> floatingPointNumber) <~ ")" ^^ {
      case d ~ lst => {d.toDouble :: lst.map{x => x.toDouble}}.toVector
    }

  /**
   * Parser for [[DenseMarkovChain]].
   * @return
   */
  def denseMarkovChainParser: Parser[DenseMarkovChain] =
    "MarkovChain(" ~> rowParser ~ rep("," ~> rowParser) <~ ")" ^^ {
      case vec ~ lstVec =>
        val matrix = {vec :: lstVec}.toVector
        new DenseMarkovChain(matrix)
    }
}