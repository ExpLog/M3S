/**
 * Created by Guest on 08/05/14.
 */

package main.scala.m3s

import main.scala.m3s.machines.{SimpleMachine, PerformanceMachine, ComplexMachine, Machine}

/**
 * Needs to be extended to provide evidence that a class can be simulated.
 * @tparam A Typeclass parameter
 */
trait CanSim[A]{
  def step: A
}

/**
 * Provides functions to simulate an object, given that this object provides
 * evidence that it can be simulated.
 *
 * TODO: add examples of how to use this
 *
 * @param obj
 * @param evidence$1
 * @tparam A
 */
class Simulation[A <% CanSim[A]](obj: A){
  /**
   * Simulates the object for `t` time steps
   * @param t Number of steps to run the simulation.
   * @return The simulation's object after `t` steps
   */
  def run(t: Int): A ={
    def aux(o: A, t: Int): A = t > 0 match {
      case true => aux(o.step, t - 1)
      case false => o
    }
    aux(obj, t)
  }

  /**
   * Simulates the object while the expression `f(obj)` is true.
   * @param f Function that takes a sequence of machines and outputs to a Boolean
   * @return Pair consisting of the final object and an Int, representing the first failure state and the number of steps until failure
   */
  def runWhile(f: A => Boolean): (A,Int) = {
    def aux(o: A, t: Int): (A,Int) = f(o) match {
      case true => aux(o.step, t+1)
      case false => (o,t)
    }
    aux(obj, 0)
  }
}
