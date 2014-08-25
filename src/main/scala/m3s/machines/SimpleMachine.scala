package m3s.machines

import m3s._
import m3s.machines.output.Output
import m3s.markov.DenseMarkovChain
import optimization.CanSim

/**
 * A [[SimpleMachine]] is composed of a [[DenseMarkovChain]] and a state.
 * The MarkovChains determines the probabilities of the simple machine changing states.
 * @param m A Markov Chain
 * @param state The initial state of the machine
 */
case class SimpleMachine(m: DenseMarkovChain, state: State, out: Output) extends Machine {
  require(state >= 0 && state < m.nStates, s"SimpleMachine: invalid initial state $state")

  def this(mc: DenseMarkovChain, out: Output) = this(mc, mc.nStates - 1, out)

  override def step: SimpleMachine = new SimpleMachine(m, m.transition(state), out)

  override def performance = out(state)

  override def toString = s"SimpleMachine($state, $out, $m)"
}

object SimpleMachine {

  implicit class SimpleMachineSim(m: SimpleMachine) extends CanSim[SimpleMachine] {
    def step = m.step
  }

}
