package optimization

import m3s._
import m3s.machines._
import RepairPolicy._
import m3s.machines.RepairableSM._
import Estimators._
import NaturalSelection._

class ComplexMachineSpecies(original: ComplexMachine,
                            mr: Double,
                            time: Int,
                            minPerformance: Double,
                            minReliability: Double,
                            rpCost: Map[RepairPolicy, Double]) extends Species[ComplexMachine] {

  val avgCost: Double = rpCost.foldLeft(0.0)(_ + _._2) / rpCost.size

  /**
   * Used to mutate a single entrance in a List[RepairPolicy]
   * @param rp
   * @return
   */
  def mutation(rp: RepairPolicy) = {
    val u = rand.nextDouble()
    if (u < mr) randomRepairPolicy else rp
  }

  /**
   * Used to merge two RepairableSM into one, mutating as necessary.
   * @param sm1
   * @param sm2
   * @return
   */
  def mergeSM(sm1: SimpleMachine, sm2: SimpleMachine): RepairableSM = (sm1, sm2) match {
    case (sm1: RepairableSM, sm2: RepairableSM) =>
      val mc = sm1.m
      val s = sm1.state
      val out = sm1.out

      val rpList = randomMixListWith(sm1.repairList, sm2.repairList)(mutation)
      val rpMap = sm1.repairCost

      new SimpleMachine(mc, s, out) with RepairableSM {
        val repairList = rpList
        val repairCost = rpMap
        val cost = 0.0
      }
    case _ => throw new Exception("ComplexMachineSpecies: mergeSM.")
  }



  //TODO: decide if mutation should be a method by itself or done inside breeding
  def spawn = addRepairCM(original, time, rpCost)

  def breed(i1: ComplexMachine, i2: ComplexMachine, mutationRate: Double) =
    i1.mergeWith(i2)(mergeSM)

  def fitness(i: ComplexMachine) = {
    //TODO: there is an infinite loop here.

    val sim = new Simulation(i)
    val cost = totalCost(i)

    val reliability = reliabilityEstimator(sim, time, 0.05) {
      case cm: ComplexMachine => cm.performance > minPerformance
    }

    (1 + reliability.mean - minReliability) * time * avgCost / cost
  }
}
