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
  val repairCost: Map[RepairPolicy, Double]
  val cost: Double

  /*
  apparently, `var r` cannot be inside the body of
  the trait, as it will be instantied before repairList is.
   */

  override def step: RepairableSM = {
    val newState = m.transition(state)

    val r = repairList.head match {
      case DoNothing => 0
      case MinorRepair => 1
      case AsGoodAsNew => m.nStates - 1
    }

    val afterPolicy = if (newState + r < m.nStates) newState + r else m.nStates - 1
    //maybe change rpl.tail to super.repairList.tail
    val newRpList = repairList.tail
    val newRpCost = repairCost
    val newCost = cost + repairCost(repairList.head)

    new SimpleMachine(m, afterPolicy, out) with RepairableSM {
      val repairList = newRpList
      val repairCost = newRpCost
      val cost = newCost
    }
  }

  override def toString = s"RepairableSM($m, $state, $out, $repairList, $repairCost, $cost)"
}

object RepairableSM {
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
   * @param rpCost Repair costs for each RepairPolicy
   * @return
   */
  def randomRepairSM(sm: SimpleMachine, n: Int, rpCost: Map[RepairPolicy, Double]): RepairableSM =
    sm match {
      case s: RepairableSM => s
      case SimpleMachine(m, s, out) => new SimpleMachine(m, s, sm.out) with RepairableSM {
        val repairList = spawnRandomRepairList(n)
        val repairCost = rpCost
        val cost = 0.0
      }
    }


  /**
   * Turns every [[machines.SimpleMachine]] inside a [[machines.ComplexMachine]]
   * into a [[RepairableSM]].
   * @param cm ComplexMachine to be converted
   * @param n Length of repair lists.
   * @param rpCost Repair costs for each RepairPolicy
   * @return
   */
  def addRepairCM(cm: ComplexMachine, n: Int, rpCost: Map[RepairPolicy, Double]): ComplexMachine = {
    val ms2 = for (m <- cm.ms) yield m match {
      case m: SimpleMachine => randomRepairSM(m, n, rpCost)
      case m: ComplexMachine => addRepairCM(m, n, rpCost)
    }
    ComplexMachine(ms2)(cm.conn)
  }
}