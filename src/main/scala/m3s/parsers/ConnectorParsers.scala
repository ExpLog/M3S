package m3s.parsers

import scala.util.parsing.combinator.JavaTokenParsers
import m3s.machines.connectors._

trait ConnectorParsers extends JavaTokenParsers {
  def parallelConnParser: Parser[Connector] = "ParallelConnector" ^^ {x => Parallel}

  def seriesConnParser: Parser[Connector] = "SeriesConnector" ^^ {x => Series}

  def connectorParser: Parser[Connector] = parallelConnParser | seriesConnParser
}
