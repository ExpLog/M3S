import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, FlatSpec}
import m3s._
import m3s.CanSim
import m3s.machines._
import m3s.machines.ComplexMachine._
import org.scalacheck.Prop
import Generators._

/**
 * Created by Leonardo Fontoura on 26/06/2014.
 */
class ComplexMachineTest extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  def sum(sm: Seq[Machine]): State = sm.map(m => m.structure).sum

  "A ComplexMachine" should "implicitly convert to a ComplexMachineCanSim" in {
    Prop.forAll(complexMachine(sum)) {
      cm =>
        val cmcs: CanSim[ComplexMachine] = cm
        cmcs.isInstanceOf[CanSim[ComplexMachine]]
    }
  }

  it should "still be a complex machine after one time step" in {
    Prop.forAll(complexMachine(sum)) {
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
  }
}
