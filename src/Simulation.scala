/**
 * Created by Guest on 08/05/14.
 */

package m3s

/**
 * Provides simulation functions for [[m3s.Machine]] systems.
 */
object Simulation {
  /**
   * Simulates [[m3s.Machine]] `m` for `t` steps.
   * @param t Number of steps to run the simulation.
   * @param m [[m3s.Machine]] to be simulated.
   * @return A [[m3s.Machine]] after `t` steps
   */
  def run(t: Int, m: Machine): Machine = t > 0 match {
    case true => run(t-1, m.step)
    case false => m
  }

  /**
   * Simulates [[m3s.Machine]] `m` while `f(m)` is true.
   * @param m [[m3s.Machine]] to be simulated
   * @param f A function that takes a Sequence of [[m3s.Machine]] and outputs to a Boolean
   * @return A pair consisting of a [[m3s.Machine]] and an Int, representing the first failure state and the number of steps until failure
   */
  def runWhile(m: Machine, t: Int = 0)(f: Machine => Boolean): (Machine,Int) = f(m) match {
    case true => runWhile(m.step, t+1)(f)
    case false => (m,t)
  }
}
