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

    val matrix = stringToMatrix("C:\\Users\\DomHellsing\\Desktop\\chain.txt")
    println(matrix)
  }
}