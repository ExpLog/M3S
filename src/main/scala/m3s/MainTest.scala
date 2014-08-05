package m3s

import m3s.machines.{RepairableSM, ComplexMachine, SimpleMachine}
import m3s.machines.connectors.{Series, Parallel}
import m3s.machines.ComplexMachine._
import m3s.machines.output.LinearOutput
import m3s.machines.RepairableSM._


object MainTest extends App {
  val matrix: Matrix = Vector(Vector(1, 0, 0), Vector(0.2, 0.8, 0), Vector(0, 0.1, 0.9))
  val output = LinearOutput(2.0, 1.5)
  val sm1 = new SimpleMachine(matrix, output)
  val listSM = List.fill(5)(sm1)
  val cm1 = new ComplexMachine(listSM)(Parallel)
  val sim1 = new Simulation(cm1)
  val results1 = List.fill(10)(sim1.run(100).performance)
  println("results " + results1)
  println("cm1 has " + countSM(cm1) + " sms.")

  //testing runWhile
  val g: ComplexMachine => Boolean = {
    case c: ComplexMachine => c.performance > 10.0
  }
  println(sim1.runWhile(100)(g))

  //testing bigger simulations
  val listCM = List.fill(5)(cm1)
  val cm2 = new ComplexMachine(listCM)(Series)
  val sim2 = new Simulation(cm2)
  val results2 = List.fill(10)(sim2.run(10).performance)
  println("results " + results2)
  println("cm2 has " + countSM(cm2) + " sms.")

  //testing if implicit is needed as evidence
  //it doesn't

  //testing cm merge
  val f: (SimpleMachine, SimpleMachine) => SimpleMachine = {
    case (a: SimpleMachine, b) =>
      require(a.out == b.out)
      new SimpleMachine(a.m, List(a.state, b.state).max - 1, a.out)
  }
  val lm1 = List.fill(2)(new SimpleMachine(matrix, output))
  val lm2 = List.fill(2)(new SimpleMachine(matrix, 0, output))
  val cm3 = new ComplexMachine(lm1)(Parallel)
  val cm4 = new ComplexMachine(lm2)(Parallel)
  val cm5 = cm3.mergeWith(cm4)(f)
  println(cm3)
  println(cm4)
  println(cm5)

  val repairList1 = RepairableSM.spawnRandomRepairList(10)
  val repairList2 = RepairableSM.spawnRandomRepairList(10)
  val repairList3 = NaturalSelection.mixLists(repairList1, repairList2)
  val repairList4 = NaturalSelection.randomMixList(repairList1, repairList2)
  println(repairList1)
  println(repairList2)
  //println(repairList3)
  println(repairList4)

  //testing add repairs
  val rpMap: Map[RepairPolicy, Double] = Map(DoNothing -> 0.1, MinorRepair -> 100.0, AsGoodAsNew -> 220.0)
  println(rpMap(DoNothing), rpMap(MinorRepair), rpMap(AsGoodAsNew))
  val rpsm = randomRepairSM(sm1, 8, rpMap)
  println(rpsm)
  println(rpsm.step)
  //shows if rpsm is a RepairableSM
  val rpsmEQUAL = randomRepairSM(rpsm, 8, rpMap)
  println(rpsmEQUAL) //shows if the rpsm stayed the same
  val rpcm = addRepairCM(cm1, 2, rpMap)
  println(rpcm)

  //  testing NaturalSelection
  val cmSpecies = new ComplexMachineSpecies(cm1, 0.05, 100, 10.0, 0.85, rpMap)
  val ga = new NaturalSelection(cmSpecies)
  val best: ComplexMachine = ga.run(50, 100, 0.4, 0.1)
  println(best)

  def totalCost(cm: ComplexMachine): Double = cm.ms.map{
    case m: RepairableSM => m.totalRepairCost
    case m: ComplexMachine => totalCost(m)
    case _ => throw new Exception("ComplexMachineSpecies: sumCost.")
  }.sum

  val bestSim = new Simulation(best)
  println(bestSim.runWhile(100)(_.performance > 10.0))
  println(totalCost(best))
}
