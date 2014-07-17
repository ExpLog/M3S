package m3s.machines

import m3s._
import RepairPolicy._

/**
 * It is a repair schedule for a [[m3s.machines.SimpleMachine]].
 * Has to be mixed in with a SimpleMachine.
 * The user also needs to provide a repairList.
 */
trait RepairableSM extends SimpleMachine {
  val repairList: List[RepairPolicy]

  val r = repairList.head match {
    case DoNothing => 0
    case MinorRepair => 1
    case AsGoodAsNew => m.nStates - 1
  }

  override def step: RepairableSM = {
    val newState = m.transition(state)
    val afterPolicy = if(newState + r < m.nStates) newState + r else m.nStates - 1
    val rpl = repairList
    //maybe change rpl.tail to super.repairList.tail
    new SimpleMachine(m, afterPolicy)(out) with RepairableSM{val repairList = rpl.tail}
  }
}

object RepairableSM{
  /**
   * Generates a random list of [[RepairPolicy]] of length `n`.
   * @param n List length
   * @return
   */
  def spawnRandomRepairList(n: Int): List[RepairPolicy] = List.fill(n)(randomRepairPolicy)

  /**
   * Turns a [[machines.SimpleMachine]] into a [[RepairableSM]].
   * @param sm SimpleMachine to be converted
   * @param n Length of repair list
   * @return
   */
  def randomRepairSM(sm: SimpleMachine, n: Int): RepairableSM =
    new SimpleMachine(sm.m, sm.state)(sm.out) with RepairableSM{
      val repairList = spawnRandomRepairList(n)
    }

  /**
   * Turns every [[machines.SimpleMachine]] inside a [[machines.ComplexMachine]]
   * into a [[RepairableSM]].
   * @param cm ComplexMachine to be converted
   * @param n Length of repair lists.
   * @return
   */
  def addRepairCM(cm: ComplexMachine, n: Int): ComplexMachine = {
    val ms2 = for(m <- cm.ms) yield m match {
      case m: SimpleMachine => randomRepairSM(m,n)
      case m: ComplexMachine => addRepairCM(m,n)
    }
    ComplexMachine(ms2)(cm.conn)
  }
}