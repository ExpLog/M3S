package m3s

/**
 * Needs to be extended to provide evidence that a class can be simulated.
 * @tparam A Typeclass parameter
 */
trait CanSim[+A] {
  def step: A
}

/**
 * Provides functions to simulate an object, given that this object provides
 * evidence that it can be simulated.
 *
 * TODO: add examples of how to use this
 *
 * @param simObj
 * @param evidence$1
 * @tparam A
 */
class Simulation[A <% CanSim[A]](simObj: A) {
  /**
   * Simulates the object for `t` time steps
   * @param t Number of steps to run the simulation.
   * @return The simulation's object after `t` steps
   */
  def run(t: Int): A = {
    def aux(obj: A, ti: Int): A = ti < t match {
      case true => aux(obj.step, ti + 1)
      case false => obj
    }
    aux(simObj, 0)
  }

  /**
   * Simulates the object while the expression `f(obj)` is true, up to a given time.
   * @param t Time
   * @param f Evaluation0
   * @return
   */
  def runWhile(t: Int)(f: A => Boolean): (A, Int) = {
    def aux(obj: A, ti: Int): (A, Int) = ti < t match {
      case true => f(obj) match {
          case true => aux(obj.step, ti + 1)
          case false => (obj, ti)
        }
      case false => (obj, ti)
    }

    aux(simObj, 0)
  }
}

object Simulation {

  //TODO: delete this
  //  /**
  //   * Estimates the reliability of a machine
  //   */
  //  def estimateReliability[A](sim: Simulation[A], time: Int)
  //                            (f: A => Boolean): Double = {
  //    val initialSims = List.fill(100)(sim.runWhile(time)(f))
  //    val randomVars = for((obj, t) <- initialSims) yield if(t == time) 1 else 0
  //    val mean = randomVars.sum/100.0
  //    val variance = (for(xi <- randomVars) yield (xi - mean)*(xi - mean)).sum/99.0
  //  }
}