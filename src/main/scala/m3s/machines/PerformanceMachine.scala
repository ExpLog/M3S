package m3s.machines

import m3s._

/**
 * Class meant to represent a [[Machine]] where the relevant information
 * is a nominal performance, determined by the machine's current [[State]].
 */
case class PerformanceMachine(m: Machine)(p: State => Double) extends Machine {
  override def step: PerformanceMachine = new PerformanceMachine(m.step)(p)

  override def structure = m.structure

  /**
   * Determines the nominal performance of each [[State]] in a [[Machine]].
   * Must be implemented by the user.
   * @param s A state.
   * @return Nominal performance of the state.
   */
  def sPerf(s: State): Double = p(s)

  /**
   * Current nominal performance of the [[Machine]]
   * @return Nominal performance at present.
   */
  def curPerf: Double = sPerf(structure)
}

class PerformanceMachineSim(m: PerformanceMachine) extends CanSim[PerformanceMachine] {
  def step = m.step
}

object PerformanceMachine {
  implicit def CanSimPerformanceMachine(m: PerformanceMachine): CanSim[PerformanceMachine] =
    new PerformanceMachineSim(m)
}
