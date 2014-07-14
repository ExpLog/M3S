package m3s.machines.connectors

import m3s.machines.Machine
import m3s.Performance

/**
 * Serves as a structure function for [[m3s.machines.ComplexMachine]].
 */
trait Connector {
  def structure(lm: List[Machine]): Performance

  def apply(lm: List[Machine]) = structure(lm)
}
