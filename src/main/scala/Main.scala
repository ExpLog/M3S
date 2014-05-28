package main.scala

/**
 * Created by Leonardo Fontoura on 06/05/2014.
 */

import m3s._
import m3s.Simulation
import m3s.machines._

object Main {
  /**
   * Just testing the scaladoc generation.
   * @param args
   */
  def main(args: Array[String]){
    println("just creating the git")

    val mk = MarkovChain("C:\\Users\\Leo\\IdeaProjects\\M3S\\src\\matrix.txt")
    println(mk)

    val m = new SimpleMachine(mk, 1)

    val s = Seq.fill(5)(m)
    s.map(x => x.structure).min
    val parallel = new ComplexMachine(s:_*)(_.map{x => x.structure}.sum)

    val sim = new Simulation(parallel)
    val parAfterSim: ComplexMachine = sim.run(10)
    println(parAfterSim.structure)

    val perfPar = PerformanceMachine(parallel)(x => math.sqrt(2)*x)
    val parSim = new Simulation(perfPar)
    println(parSim.runWhile(x => x.curPerf > 3.0))


//    import scala.pickling._
//    import scala.pickling.binary._
//    val mkp = mk.pickle
//    println(mkp)
//    println(mkp.unpickle[MarkovChain])
  }
}

