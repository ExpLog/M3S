/**
 * Created by Leonardo Fontoura on 08/05/14.
 */

package m3s
import Machine._

/**
 * Simules a machine that is represented by a tree where the internal nodes are [[m3s.ComplexMachine]] and
 * the leaf nodes are [[m3s.SimpleMachine]].
 */
abstract class Machine {
  def step: Machine

  def structure: State

  def performance(s: Int): Double
}


/**
 * A [[m3s.SimpleMachine]] is composed of a [[m3s.MarkovChain]] and a state.
 * The MarkovChains determines the probabilities of the simple machine changing states.
 * @param m A [[m3s.MarkovChain]]
 * @param state The current state.
 */
abstract case class SimpleMachine(m: MarkovChain, state: State) extends Machine {
  override def step: Machine = SimpleMachine(m, m.transition(state))

  override def structure: State = state

  def performance(s: Int): Double
}

/**
 * A [[m3s.ComplexMachine]] is composed of a mixture of other [[m3s.ComplexMachine]] and [[m3s.SimpleMachine]] plus
 * a structure function that determines how the outputs of the children machinery come together to form the
 * more complex machine.
 * @param ms A sequence of [[m3s.Machine]].
 * @param f A function that takes a sequence of [[m3s.Machine]] and outputs a [[m3s.Machine.State]].
 */
abstract case class ComplexMachine(ms: Machine*)(f: Seq[Machine] => State) extends Machine {
  override def step: Machine = {
    val ms2 = for( m <- ms ) yield m.step
    ComplexMachine(ms2)(f)
  }

  override def structure = f(ms)

  def performance(s: Int): Double
}

object Machine{
  /**
   * A [[m3s.Machine.State]] is an Int that represents the current state of a [[m3s.Machine]]
   */
  type State = Int
}