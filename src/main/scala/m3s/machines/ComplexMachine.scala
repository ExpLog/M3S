/**
 * Created by Leonardo Fontoura on 26/05/2014.
 */

package m3s.machines

import m3s._

/**
 * A [[ComplexMachine]] is composed of a mixture of other [[ComplexMachine]] and [[SimpleMachine]] plus
 * a [[Machine.structure]] function that determines how the outputs of the children machinery come together to form the
 * more complex machine.
 *
 * For example, a sequence `M` of SimpleMachine connected in series would be a ComplexMachine
 * with `M` as its children plus a structure function defined as:
 *
 * {{{
 *   def f(ms: Seq[Machine] => State): State = ms.map(x => x.structure).min
 * }}}
 *
 * @param ms A sequence of machines
 * @param f A function that takes a sequence of [[Machine]] and outputs a [[State]].
 */
case class ComplexMachine(ms: Machine*)(val f: Seq[Machine] => State) extends Machine {
  require(ms.length > 0, "ComplexMachine: empty list of children machinery")

  override def step: ComplexMachine = {
    val ms2: Seq[Machine] = for( m <- ms ) yield m.step
    ComplexMachine(ms2:_*)(f)
  }

  override def structure = f(ms)
}

class ComplexMachineSim(m: ComplexMachine) extends CanSim[ComplexMachine] {
  def step = m.step
}

object ComplexMachine {
  implicit def CanSimComplexMachine(m: ComplexMachine): CanSim[ComplexMachine] = new ComplexMachineSim(m)
}
