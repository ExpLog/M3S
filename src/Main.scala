/**
 * Created by Leonardo Fontoura on 06/05/2014.
 */

import m3s._
import m3s.MarkovChain._

object Main {
  /**
   * Just testing the scaladoc generation.
   * @param args
   */
  def main(args: Array[String]){
    println("just creating the git")

    val matrix = fileToMatrix("C:\\Users\\DomHellsing\\Desktop\\chain.txt")
    println(matrix)

    val m = new SimpleMachine(MarkovChain(matrix), 1) with Performance{ def sPerf(s: State) = 1.1*s }

    val s = Seq.fill(5)(m)
    s.map(x => x.structure).min
  }
}