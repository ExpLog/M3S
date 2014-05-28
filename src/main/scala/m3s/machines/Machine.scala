/**
 * Created by Leonardo Fontoura on 08/05/14.
 */

package m3s.machines

import m3s._

/**
 * Simulates a machine that is represented by a tree where the internal nodes are [[ComplexMachine]] and
 * the leaf nodes are [[SimpleMachine]].
 */
trait Machine {

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