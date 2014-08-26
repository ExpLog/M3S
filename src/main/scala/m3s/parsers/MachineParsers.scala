package m3s.parsers

import scala.util.parsing.combinator.JavaTokenParsers
import m3s.machines._

trait MachineParsers extends JavaTokenParsers
                    with M3SParsers
                    with ConnectorParsers
                    with OutputParsers
                    with RepairParsers {

  def simpleMachineParser: Parser[SimpleMachine] =
    "SimpleMachine(" ~> {
      floatingPointNumber <~ ","
    } ~ {
      outputParser <~ ","
    } ~ markovChainParser <~ ")" ^^ {
      case state ~ output ~ mc => SimpleMachine(mc, state.toInt, output)
    }

  def complexMachineParser: Parser[ComplexMachine] =
    "ComplexMachine(" ~> {
      connectorParser <~ "," <~ floatingPointNumber <~ ","
    } ~
      repsep(machineParser, ",") <~ ")" ^^ {
      case conn ~ machineList => new ComplexMachine(machineList)(conn)
    }

  def repairableSMParser: Parser[RepairableSM] = "RepairableSM(" ~> {
    markovChainParser <~ ","
  } ~ {
    floatingPointNumber <~ ","
  } ~ {
    outputParser <~ ","
  } ~ {
    rpListParser <~ ","
  } ~ {
    mapRpDoubleParser <~ ","
  } ~ {floatingPointNumber <~ ")"} ^^ {
    case mc ~ state ~ out ~ rpList ~ rpCost ~ totCost =>
      new SimpleMachine(mc, state.toInt, out) with RepairableSM {
        val repairList = rpList
        val repairCost = rpCost
        val cost = totCost.toDouble
      }
  }

  def machineParser: Parser[Machine] =
    simpleMachineParser | complexMachineParser | repairableSMParser
}

//s"RepairableSM($m, $state, $out, $repairList, $repairCost, $cost)"

