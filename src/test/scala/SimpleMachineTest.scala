import m3s.CanSim
import m3s.machines.output.LinearOutput
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import m3s.machines.SimpleMachine
import m3s.machines.SimpleMachine._
import Generators._

class SimpleMachineTest extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks{
  "A SimpleMachine" should "implicitly convert to a SimpleMachineCanSim" in {
    forAll(simpleMachine) {
      sm =>
        val smcs: CanSim[SimpleMachine] = sm
        smcs.isInstanceOf[CanSim[SimpleMachine]]
    }
  }

  it should "still be a SimpleMachine after one time step" in {
    forAll(simpleMachine) {
      sm =>
        val sm2 = sm.step
        sm2.isInstanceOf[SimpleMachine]
    }
  }

  it should "throw an exception when created with an invalid state" in {
    val mtx = Vector(Vector(1.0))

    a [Exception] should be thrownBy {
      val sm1 = SimpleMachine(mtx,-1)(LinearOutput(1.0,0.0))
    }

    the [Exception] thrownBy SimpleMachine(mtx,-1)(LinearOutput(1.0,0.0)) should have message "requirement failed: SimpleMachine: invalid initial state"

    a [Exception] should be thrownBy {
      val sm2 = SimpleMachine(mtx, 1)(LinearOutput(1.0,0.0))
    }

    the [Exception] thrownBy SimpleMachine(mtx,1)(LinearOutput(1.0,0.0)) should have message "requirement failed: SimpleMachine: invalid initial state"
  }
}
