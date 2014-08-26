package m3s

import m3s.machines._
import m3s.machines.connectors._
import m3s.machines.output._
import m3s.machines.ComplexMachine._
import m3s.machines.RepairableSM._

import m3s.markov.DenseMarkovChain
import optimization._

import m3s.parsers._


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
      SimpleMachine(a.m, List(a.state, b.state).max - 1, a.out)
  }
  val lm1 = List.fill(2)(new SimpleMachine(matrix, output))
  val lm2 = List.fill(2)(SimpleMachine(matrix, 0, output))
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
  println(rpsmEQUAL)
  //shows if the rpsm stayed the same
  val rpcm = addRepairCM(cm1, 2, rpMap)
  println(rpcm)

  //  testing NaturalSelection
  val cmSpecies = new ComplexMachineSpecies(cm1, 0.05, 100, 15.0, 0.50, rpMap)
  val ga = new NaturalSelection(cmSpecies)
  val best = ga.run(1, 1, 0.6, 0.1)
  println(best)

  val bestSim = new Simulation(best._1)
  println(bestSim.runWhile(100)(_.performance > 15.0))
  println(totalCost(best._1))

  import Estimators._

  val stats = reliabilityEstimator(bestSim, 100, 0.05)(_.performance > 0.05)
  println(stats)
}

object ParserMain extends App with M3SParsers with OutputParsers with RepairParsers
  with ConnectorParsers with MachineParsers {

  //vector parser test
  val vector = Vector(1.0, 2.0, 3.0, -1.0, 0)
  println(parse(vectorDoubleParser, vector toString()))
  val vector2 = parse(vectorDoubleParser, vector toString()).get
  println(vector.sum == vector2.sum)

  //matrix parser test
  val matrix: Matrix = Vector(Vector(1, 0, 0), Vector(0.2, 0.8, 0), Vector(0, 0.1, 0.9))
  println(parse(matrixParser, matrix toString()))

  //markov chain parser test
  val mc = new DenseMarkovChain(matrix)
  println(parse(denseMarkovChainParser, mc toString()))

  //output parsers test
  val lo = LinearOutput(-1.0, 3.93)
  val qo = QuadraticOutput(-3, 851283.0321, +100.1)

  println(parse(linearOutputParser, lo.toString))
  println(parse(quadraticOutputParser, qo.toString))
  println(parse(outputParser, lo.toString))
  println(parse(outputParser, qo.toString))

  //connector parsers
  println(parse(connectorParser, Series.toString))
  println(parse(connectorParser, Parallel.toString))

  //repair parsers
  val rplist1 =  List(DoNothing, AsGoodAsNew)
  val rplist2 =  List()
  println(parse(rpListParser, rplist1.toString()))
  println(parse(rpListParser, rplist2.toString()))

  val map: Map[RepairPolicy, Double] = Map(DoNothing -> -1.01, MinorRepair -> 3.102, AsGoodAsNew -> 0)
  println(parse(mapRpDoubleParser, map.toString()))

  //machine parsers
  val sm1 = new SimpleMachine(mc, LinearOutput(1.0, 0.0))
  println(parse(simpleMachineParser, sm1.toString))

  val smList1 = List.fill(5)(sm1)
  val cm1 = new ComplexMachine(smList1)(Series)
  println(parse(complexMachineParser, cm1.toString))

  val cm2 = new ComplexMachine(cm1 :: smList1)(Parallel)
  println(parse(complexMachineParser, cm2.toString))

  val rpsm1 = randomRepairSM(sm1, 3, map)
  println(parse(repairableSMParser, rpsm1.toString))

  val cm3 = addRepairCM(cm1, 3, map)

  println(parse(machineParser, sm1.toString))
  println(parse(machineParser, cm1.toString))
  println(parse(machineParser, rpsm1.toString))
  println(parse(machineParser, cm3.toString))
}
