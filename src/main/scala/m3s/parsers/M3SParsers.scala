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
    "Vector(" ~> repsep(floatingPointNumber, ",") <~ ")" ^^ {
      case lst => lst.map{x => x.toDouble}.toVector
    }

  /**
   * Parser for [[m3s.Matrix]].
   * @return
   */
  def matrixParser: Parser[Matrix] =
    "Vector(" ~> repsep(vectorDoubleParser, ",") <~ ")" ^^ {
      case lst => lst.toVector
    }

  /**
   * Auxiliary parser for [[DenseMarkovChain]].
   * @return
   */
  def rowParser: Parser[Vector[Double]] =
    "Row(" ~> repsep(floatingPointNumber, ",") <~ ")" ^^ {
      case lst => lst.map{x => x.toDouble}.toVector
    }

  def rowParser2: Parser[Vector[Double]] = "Row(" ~> repsep(floatingPointNumber, ",") <~ ")" ^^ {
    case lst => lst.map(x => x.toDouble).toVector
  }

  /**
   * Parser for [[DenseMarkovChain]].
   * @return
   */
  def denseMarkovChainParser: Parser[DenseMarkovChain] =
    "MarkovChain(" ~> repsep(rowParser, ",") <~ ")" ^^ {
      case lstVec =>
        val matrix = lstVec.toVector
        new DenseMarkovChain(matrix)
    }

  /**
   * Placeholder parser for when [[m3s.markov.DenseMarkovChain]] extends [[m3s.markov.MarkovChain]].
   * @return
   */
  def markovChainParser: Parser[DenseMarkovChain] = denseMarkovChainParser
}