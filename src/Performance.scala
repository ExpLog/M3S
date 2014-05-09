/**
 * Created by Leonardo Fontoura on 09/05/2014.
 */

package m3s

import m3s.MarkovChain.State


trait Performance extends Machine {
  def sPerf(s: State): Double

  def curPerf: Double = sPerf(structure)
}
