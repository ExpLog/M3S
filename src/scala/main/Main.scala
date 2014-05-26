package scala.main

/**
 * Created by Leonardo Fontoura on 06/05/2014.
 */

import scala.main.m3s._
import MarkovChain._

object Main {
  /**
   * Just testing the scaladoc generation.
   * @param args
   */
  def main(args: Array[String]){
    println("just creating the git")

    val mk = MarkovChain("C:\\Users\\Leo\\IdeaProjects\\M3S\\src\\matrix.txt")
    println(mk)

    val m = new SimpleMachine(mk, 1) with Performance{ def sPerf(s: State) = 1.1*s }

    val s = Seq.fill(5)(m)
    s.map(x => x.structure).min
  }
}