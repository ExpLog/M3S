package m3s.machines.output

import m3s.State

class QuadraticOutput(a: Double, b: Double, c: Double) extends Output {
  def map(s: State) = a*s*s + b*s + c
}
