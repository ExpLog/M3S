package m3s.machines.connectors

import m3s.machines.Machine

case object Series extends Connector {
  def structure(lm: List[Machine]) = lm.map(m => m.performance).min

  override def toString = "SeriesConnector"
}
