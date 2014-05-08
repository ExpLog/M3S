/**
 * Created by Leonardo Fontoura on 08/05/14.
 */

package m3s
import MarkovChain._

/**
 * Simulates a machine that is represented by a tree where the internal nodes are [[m3s.ComplexMachine]] and
 * the leaf nodes are [[m3s.SimpleMachine]].
 */
abstract class Machine {
  /**
   * Simulates the [[m3s.Machine]] for 1 time step.
   * @return
   */
  def step: Machine

  /**
   * The structure function of the machine. Defines how the current state of the machine is obtained.
   * In the case of a [[m3s.ComplexMachine]], it is a function defined on its children.
   * In the case of a [[m3s.SimpleMachine]], it is usually the identity function on the state of its [[m3s.MarkovChain]]
   * @return
   */
  def structure: State

  /**
   * Gives the nominal performance corresponding to the current [[m3s.MarkovChain.State]] of the [[m3s.Machine]].
   * Needs to be overrided by the user.
   * @return Nominal performance of this [[m3s.Machine]]
   */
  def performance(): Double = structure
}


/**
 * A [[m3s.SimpleMachine]] is composed of a [[m3s.MarkovChain]] and a state.
 * The MarkovChains determines the probabilities of the simple machine changing states.
 * @param m A [[m3s.MarkovChain]]
 * @param state The current state.
 */
case class SimpleMachine(m: MarkovChain, state: State) extends Machine {
  override def step: Machine = SimpleMachine(m, m.transition(state))

  override def structure: State = state
}

/**
 * A [[m3s.ComplexMachine]] is composed of a mixture of other [[m3s.ComplexMachine]] and [[m3s.SimpleMachine]] plus
 * a structure function that determines how the outputs of the children machinery come together to form the
 * more complex machine.
 * @param ms A sequence of [[m3s.Machine]].
 * @param f A function that takes a sequence of [[m3s.Machine]] and outputs a [[m3s.MarkovChain.State]].
 */
case class ComplexMachine(ms: Machine*)(f: Seq[Machine] => State) extends Machine {
  override def step: Machine = {
    val ms2: Seq[Machine] = for( m <- ms ) yield m.step
    ComplexMachine(ms2:_*)(f)
  }

  override def structure = f(ms)
}
