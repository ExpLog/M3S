package m3s.machines.output

import m3s.State

case class QuadraticOutput(a: Double, b: Double, c: Double) extends Output {
  def map(s: State) = a * s * s + b * s + c

  override def toString = s"QuadraticOutput($a, $b, $c)"
}
