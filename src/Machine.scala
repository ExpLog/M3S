/**
 * Created by Leonardo Fontoura on 08/05/14.
 */

package m3s

abstract class Machine {
  def step(): Machine

  def structure(): Int

  def performance(s: Int): Double
}

abstract case class SimpleMachine(m: MarkovChain, state: Int) extends Machine {
  override def step(): Machine = SimpleMachine(m, m.transition(state))

  override def structure(): Int = state

  def performance(s: Int): Double
}

abstract case class ComplexMachine(ms: Machine*)(f: Seq[Machine] => Int) extends Machine {
  override def step(): Machine = {
    val ms2 = for( m <- ms ) yield m.step
    ComplexMachine(ms2)(f)
  }

  override def structure() = f(ms)

  def performance(s: Int): Double
}