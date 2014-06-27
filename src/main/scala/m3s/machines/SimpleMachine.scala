package m3s.machines

import m3s._
/**
 * A [[SimpleMachine]] is composed of a [[MarkovChain]] and a state.
 * The MarkovChains determines the probabilities of the simple machine changing states.
 * @param m A Markov Chain
 * @param state The initial state of the machine
 */
case class SimpleMachine(private val m: MarkovChain, private val state: State) extends Machine {
  require(state >= 0 && state < m.nStates, "SimpleMachine: invalid initial state")

  override def step: SimpleMachine = SimpleMachine(m, m.transition(state))

  override def structure: State = state
}

object SimpleMachine {
  class SimpleMachineSim(m: SimpleMachine) extends CanSim[SimpleMachine] {
    def step = m.step
  }

  implicit def CanSimSimpleMachine(m: SimpleMachine): CanSim[SimpleMachine] = new SimpleMachineSim(m)
}
