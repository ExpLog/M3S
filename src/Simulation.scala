/**
 * Created by Guest on 08/05/14.
 */

package m3s

object Simulation {
  def run(t: Int, mt: Machine): Machine = t > 0 match {
    case true => run(t-1, mt.step)
    case false => mt
  }

  def runWhile(mt: Machine)(f: Machine => Boolean): Machine = f(mt) match {
    case true => runWhile(mt.step)(f)
    case false => mt
  }
}
