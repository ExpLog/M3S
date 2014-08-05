package m3s.machines

import m3s._

sealed trait RepairPolicy

case object DoNothing extends RepairPolicy

case object MinorRepair extends RepairPolicy

case object AsGoodAsNew extends RepairPolicy

object RepairPolicy {
  /**
   * Returns a random RepairPolicy.
   * @return
   */
  def randomRepairPolicy: RepairPolicy = {
    val u = rand.nextInt(3)
    if (u == 0) DoNothing else if (u == 1) MinorRepair else AsGoodAsNew
  }
}


