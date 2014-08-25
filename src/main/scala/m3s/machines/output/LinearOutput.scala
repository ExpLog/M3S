package m3s.machines.output

import m3s.State

case class LinearOutput(a: Double, b: Double) extends Output {
  def map(s: State) = a * s + b

  override def toString = s"LinearOutput($a, $b)"
}
