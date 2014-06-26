import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, FlatSpec}
import m3s._
import m3s.CanSim
import m3s.machines._
import m3s.machines.PerformanceMachine._
import org.scalacheck.Prop
import Generators._

class PerformanceMachineTest extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  "A PerformanceMachine" should "implicitly convert to a PerformanceMachineCanSim" in {
    Prop.forAll(performanceMachine) {
      pm =>
        val pmcs: CanSim[PerformanceMachine] = pm
        pmcs.isInstanceOf[CanSim[PerformanceMachine]]
    }
  }

  it should "still be a PerformanceMachine after one time step" in {
    Prop.forAll(performanceMachine) {
      pm =>
        val pm2 = pm.step
        pm2.isInstanceOf[PerformanceMachine]
    }
  }

  it should "have a performance greater or equal than the machine it wraps when p(x)=d*x and d >= 1" in {
    Prop.forAll(performanceMachine) {
      pm =>
        pm.curPerf >= pm.structure
    }
  }
}
