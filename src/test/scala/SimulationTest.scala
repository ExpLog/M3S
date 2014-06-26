import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, FlatSpec}
import m3s._
import m3s.CanSim
import m3s.machines._
import Generators._

class SimulationTest extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  "A Simulation" should "be creatable for any Machine that has evidence of CanSim" in {
    forAll(simpleMachine) {
      m =>
        try {
          val sim = new Simulation(m)
          true
        } catch {
          case _: Throwable => false
        }
    }

    forAll(complexMachine) {
      m =>
        try {
          val sim = new Simulation(m)
          true
        } catch {
          case _: Throwable => false
        }
    }

    forAll(performanceMachine) {
      m =>
        try {
          val sim = new Simulation(m)
          true
        } catch {
          case _: Throwable => false
        }
    }
  }

  it should "return an object of the same type as the initial one" in {
    forAll(simpleMachine) {
      m =>
        val sim = new Simulation(m)
        sim.run(10).isInstanceOf[SimpleMachine]
    }

    forAll(complexMachine) {
      m =>
        val sim = new Simulation(m)
        sim.run(10).isInstanceOf[ComplexMachine]
    }

    forAll(performanceMachine) {
      m =>
        val sim = new Simulation(m)
        sim.run(10).isInstanceOf[PerformanceMachine]
    }
  }
}
