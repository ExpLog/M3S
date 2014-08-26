package m3s.parsers

import scala.util.parsing.combinator.JavaTokenParsers
import m3s.machines.output._


trait OutputParsers extends JavaTokenParsers {
  def linearOutputParser: Parser[LinearOutput] =
    "LinearOutput(" ~> repsep(floatingPointNumber, ",") <~ ")" ^^ {
      case a::b::Nil => LinearOutput(a.toDouble, b.toDouble)
    }

  def quadraticOutputParser: Parser[QuadraticOutput] =
    "QuadraticOutput(" ~> repsep(floatingPointNumber, ",") <~ ")" ^^ {
      case a::b::c::Nil => QuadraticOutput(a.toDouble, b.toDouble,c.toDouble)
    }

  def outputParser: Parser[Output] = linearOutputParser | quadraticOutputParser
}
