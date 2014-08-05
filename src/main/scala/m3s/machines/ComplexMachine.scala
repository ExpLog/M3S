/**
 * Created by Leonardo Fontoura on 26/05/2014.
 */

package m3s.machines

import m3s._
import m3s.machines.connectors.Connector

/**
 * A [[ComplexMachine]] is composed of a mixture of other [[ComplexMachine]] and [[SimpleMachine]] plus
 * a [[machines.connectors.Connector]] that determines how the outputs of the children machinery come
 * together to form the more complex machine.
 *
 * @param ms A sequence of machines
 * @param conn A function that takes a sequence of [[Machine]] and outputs a [[State]].
 */
case class ComplexMachine(ms: List[Machine])(val conn: Connector) extends Machine {
  require(ms.length > 0, "ComplexMachine: empty list of children machinery")

  override def step: ComplexMachine = {
    val ms2 = for (m <- ms) yield m.step
    ComplexMachine(ms2)(conn)
  }

  override def performance = conn(ms)

  /**
   * Merges two [[ComplexMachine]] together, using a function `f` to handle [[SimpleMachine]].
   * @param that The second ComplexMachine
   * @param f Function that takes 2 SimpleMachine and returns another one
   * @return
   */
  def mergeWith(that: ComplexMachine)
               (f: (SimpleMachine, SimpleMachine) => SimpleMachine): ComplexMachine = {
    require(this.conn == that.conn)
    val zipped: List[(Machine, Machine)] = this.ms zip that.ms
    val ms2: List[Machine] = zipped map {
      case (x: SimpleMachine, y: SimpleMachine) => f(x, y)
      case (x: ComplexMachine, y: ComplexMachine) => x.mergeWith(y)(f)
    }

    new ComplexMachine(ms2)(this.conn)
  }

  override def toString = {
    val strings = for (machine <- ms) yield machine.toString
    strings.mkString(s"ComplexMachine(\n\t$conn, $performance\n\t", "\n\t", ")")
  }
}

object ComplexMachine {

  implicit class ComplexMachineSim(cm: ComplexMachine) extends CanSim[ComplexMachine] {
    def step = cm.step
  }

  def countSM(cm: ComplexMachine): Int = cm.ms.map {
    case m: SimpleMachine => 1
    case m: ComplexMachine => countSM(m)
  }.sum

  class ComplexMachineSpecies(original: ComplexMachine,
                              mr: Double,
                              time: Int,
                              minPerformance: Double,
                              minReliability: Double,
                              rpCost: Map[RepairPolicy, Double]) extends Species[ComplexMachine] {

    import RepairableSM._
    import NaturalSelection._
    import RepairPolicy._
    import Estimators._

    val avgCost: Double = rpCost.foldLeft(0.0)(_+_._2)/rpCost.size

    /**
     * Used to mutate a single entrance in a List[RepairPolicy]
     * @param rp
     * @return
     */
    def mutation(rp: RepairPolicy) = {
      val u = rand.nextDouble()
      if(u < mr) randomRepairPolicy else rp
    }

    /**
     * Used to merge two RepairableSM into one, mutating as necessary.
     * @param sm1
     * @param sm2
     * @return
     */
    def mergeSM(sm1: SimpleMachine, sm2: SimpleMachine): RepairableSM = (sm1, sm2) match {
      case (sm1: RepairableSM, sm2: RepairableSM) =>
        val mc    = sm1.m
        val s     = sm1.state
        val out   = sm1.out

        val rpList  = randomMixListWith(sm1.repairList, sm2.repairList)(mutation)
        val rpMap   = sm1.repairCost

        new SimpleMachine(mc, s, out) with RepairableSM {
          val repairList = rpList
          val repairCost = rpMap
          val cost = 0.0
        }
      case _ => throw new Exception("ComplexMachineSpecies: mergeSM.")
    }

    /**
     * Sums the total repair cost of a repair schedule in a ComplexMachine
     * @param cm
     * @return
     */
    def totalCost(cm: ComplexMachine): Double = cm.ms.map{
      case m: RepairableSM => m.totalRepairCost
      case m: ComplexMachine => totalCost(m)
      case _ => throw new Exception("ComplexMachineSpecies: sumCost.")
    }.sum

    //TODO: decide if mutation should be a method by itself or done inside breeding
    def spawn = addRepairCM(original, time, rpCost)

    def breed(i1: ComplexMachine, i2: ComplexMachine, mutationRate: Double) =
      i1.mergeWith(i2)(mergeSM)

    def fitness(i: ComplexMachine) = {
      val sim = new Simulation(i)
      val cost = totalCost(i)
//      println(cost)

      val reliability = reliabilityEstimator(sim, time, 0.05){
        case cm : ComplexMachine => cm.performance > minPerformance
      }

      (reliability.mean - minReliability)*time*avgCost/cost
    }
  }
}
