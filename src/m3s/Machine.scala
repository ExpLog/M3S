/**
 * Created by Leonardo Fontoura on 08/05/14.
 */

package m3s

/**
 * Simulates a machine that is represented by a tree where the internal nodes are [[ComplexMachine]] and
 * the leaf nodes are [[SimpleMachine]].
 */
abstract class Machine {
  /**
   * Simulates the machine for 1 time step.
   * @return A machine in the next state
   */
  def step: Machine

  /**
   * The structure function of the machine. Defines how the current state of the machine is obtained.
   *
   * In the case of a [[SimpleMachine]], it is the identity function on the [[State]] of its [[MarkovChain]].
   *
   * In the case of a [[ComplexMachine]], it is a function defined on the states its children.
   * @return Current state of the machine
   */
  def structure: State
}


/**
 * A [[SimpleMachine]] is composed of a [[MarkovChain]] and a state.
 * The MarkovChains determines the probabilities of the simple machine changing states.
 * @param m A Markov Chain
 * @param state The initial state of the machine
 */
case class SimpleMachine(private val m: MarkovChain, private val state: State) extends Machine {
  require(state >= 0 && state < m.nStates, "SimpleMachine: invalid initial state")

  override def step: Machine = SimpleMachine(m, m.transition(state))

  override def structure: State = state
}

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
case class ComplexMachine(ms: Machine*)(f: Seq[Machine] => State) extends Machine {
  require(ms.length > 0, "ComplexMachine: empty list of children machinery")

  override def step: Machine = {
    val ms2: Seq[Machine] = for( m <- ms ) yield m.step
    ComplexMachine(ms2:_*)(f)
  }

  override def structure = f(ms)
}