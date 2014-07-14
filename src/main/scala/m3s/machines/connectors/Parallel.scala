package m3s.machines.connectors

import m3s.machines.Machine

case object Parallel extends Connector {
  def structure(lm: List[Machine]) = {
    lm.foldLeft(0.0){case (carry, m) => carry + m.performance}
  }
}
