package m3s.machines

import m3s.parsers._
import scala.io.Source.fromFile

object Reader extends MachineParsers {
  def apply(filePath: String): Machine = parse(machineParser, fromFile(filePath).reader()).get
}
