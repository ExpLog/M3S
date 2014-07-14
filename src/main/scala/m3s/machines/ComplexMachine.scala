/**
 * Created by Leonardo Fontoura on 26/05/2014.
 */

package m3s.machines

import m3s._
import m3s.machines.connectors.Connector

/**
 * A [[ComplexMachine]] is composed of a mixture of other [[ComplexMachine]] and [[SimpleMachine]] plus
 * a [[machines.connectors.Connector]] that determines how the outputs of the children machinery come
 * together to form the more complex machine.
 *
 * @param ms A sequence of machines
 * @param conn A function that takes a sequence of [[Machine]] and outputs a [[State]].
 */
case class ComplexMachine(ms: List[Machine])(val conn: Connector) extends Machine {
  require(ms.length > 0, "ComplexMachine: empty list of children machinery")

  override def step: ComplexMachine = {
    val ms2 = for( m <- ms ) yield m.step
    ComplexMachine(ms2)(conn)
  }

  override def performance = conn(ms)

  def mergeWith(that: ComplexMachine)
               (f: (SimpleMachine, SimpleMachine) => SimpleMachine ): ComplexMachine = {
    require(this.conn == that.conn)
    val zipped: List[(Machine,Machine)] = this.ms zip that.ms
    val ms2: List[Machine] = zipped map {
      case (x: SimpleMachine, y: SimpleMachine) => f(x,y)
      case (x: ComplexMachine, y: ComplexMachine) => x.mergeWith(y)(f)
    }

    new ComplexMachine(ms2)(this.conn)
  }
}

object ComplexMachine {
  class ComplexMachineSim(m: ComplexMachine) extends CanSim[ComplexMachine] {
    def step = m.step
  }

  implicit def CanSimComplexMachine(m: ComplexMachine): CanSim[ComplexMachine] = new ComplexMachineSim(m)

  def countSM(cm: ComplexMachine): Int = cm.ms.map{
    case m: SimpleMachine => 1
    case m: ComplexMachine => countSM(m)
  }.sum
}
