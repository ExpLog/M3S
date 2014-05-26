/**
 * Created by Guest on 08/05/14.
 */

package main.scala.m3s

/**
 * Provides simulation functions for [[Machine]] systems.
 */
object Simulation {
  /**
   * Simulates [[Machine]] `m` for `t` steps.
   * @param t Number of steps to run the simulation.
   * @param m [[Machine]] to be simulated.
   * @return A [[Machine]] after `t` steps
   */
  def run(t: Int, m: Machine): Machine = t > 0 match {
    case true => run(t-1, m.step)
    case false => m
  }

  /**
   * Simulates [[Machine]] `m` while the expression `f(m)` is true.
   * @param m Machine to be simulated
   * @param f Function that takes a sequence of machines and outputs to a Boolean
   * @return Pair consisting of the final machine and an Int, representing the first failure state and the number of steps until failure
   */
  def runWhile(m: Machine, t: Int = 0)(f: Machine => Boolean): (Machine,Int) = f(m) match {
    case true => runWhile(m.step, t+1)(f)
    case false => (m,t)
  }
}
