/**
 * Created by Leonardo Fontoura on 09/05/2014.
 */

package m3s

/**
 * Trait meant to be mixed in with a [[Machine]] where the relevant information
 * is a nominal performance, determined by the machine's current [[State]]
 */
trait Performance extends Machine {
  /**
   * Determines the nominal performance of each [[State]] in a [[Machine]].
   * Must be implemented by the user.
   * @param s A state.
   * @return Nominal performance of the state.
   */
  def sPerf(s: State): Double

  /**
   * Current nominal performance of the [[Machine]]
   * @return Nominal performance at present.
   */
  def curPerf: Double = sPerf(structure)
}
