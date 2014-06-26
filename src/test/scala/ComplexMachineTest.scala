import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, FlatSpec}
import m3s.CanSim
import m3s.machines._
import m3s.machines.ComplexMachine._
import Generators._

class ComplexMachineTest extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  "A ComplexMachine" should "implicitly convert to a ComplexMachineCanSim" in {
    forAll(complexMachine) {
      cm =>
        val cmcs: CanSim[ComplexMachine] = cm
        cmcs.isInstanceOf[CanSim[ComplexMachine]]
    }
  }

  it should "still be a ComplexMachine after one time step" in {
    forAll(complexMachine) {
      cm =>
        val cm2 = cm.step
        cm2.isInstanceOf[ComplexMachine]
    }
  }

  it should "throw an exception if it doesn't have any children machine" in {
    val emptyList: List[Machine] = List()
    a [Exception] should be thrownBy {
      ComplexMachine(emptyList:_*)(sum)
    }

    the [Exception] thrownBy ComplexMachine(emptyList:_*)(sum) should have message "requirement failed: ComplexMachine: empty list of children machinery"
  }
}
