package m3s.parsers

import scala.util.parsing.combinator.JavaTokenParsers
import m3s.machines.output._


trait OutputParsers extends JavaTokenParsers {
  def linearOutputParser: Parser[LinearOutput] =
    "LinearOutput(" ~> {floatingPointNumber <~ ","} ~ floatingPointNumber <~ ")" ^^ {
      case a ~ b => LinearOutput(a.toDouble, b.toDouble)
    }

  def quadraticOutputParser: Parser[QuadraticOutput] =
    "QuadraticOutput(" ~> {floatingPointNumber <~ ","} ~ {floatingPointNumber <~ ","} ~
      floatingPointNumber <~ ")" ^^ {
      case a ~ b ~ c => QuadraticOutput(a.toDouble, b.toDouble,c.toDouble)
    }

  def outputParser: Parser[Output] = linearOutputParser | quadraticOutputParser
}
