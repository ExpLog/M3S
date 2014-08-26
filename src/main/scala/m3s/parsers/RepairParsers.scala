package m3s.parsers

import scala.util.parsing.combinator.JavaTokenParsers
import m3s.machines._

trait RepairParsers extends JavaTokenParsers {

  def rpParser: Parser[RepairPolicy] = {"DoNothing" | "MinorRepair" | "AsGoodAsNew"} ^^ {
    case "DoNothing"    => DoNothing
    case "MinorRepair"  => MinorRepair
    case "AsGoodAsNew"  => AsGoodAsNew
  }

  def rpListParser: Parser[List[RepairPolicy]] = "List(" ~> repsep(rpParser, ",") <~ ")"

  def mapRpDoubleParser: Parser[Map[RepairPolicy, Double]] =
    "Map(" ~> repsep(rpParser ~ "->" ~ floatingPointNumber, ",") <~ ")" ^^ {
      lst => lst.map{case rp ~ arrow ~ cost => (rp, cost.toDouble)}.toMap
    }
}