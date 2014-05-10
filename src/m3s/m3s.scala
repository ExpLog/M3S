/**
 * Created by Leonardo Fontoura on 09/05/2014.
 */

/**
 * Framework for simulating stochastic machines.
 */
package object m3s {
  /**
   * Defines a matrix as a vector of vector of doubles.
   */
  type Matrix = Vector[Vector[Double]]

  /**
   * A state is an Int that represents the current state of a [[MarkovChain]]
   */
  type State = Int
}