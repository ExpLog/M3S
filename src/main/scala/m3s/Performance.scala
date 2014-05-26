/**
 * Created by Leonardo Fontoura on 09/05/2014.
 */

package main.scala.m3s

import Performance._

/**
 * Trait meant to be mixed in with a [[Machine]] where the relevant information
 * is a nominal performance, determined by the machine's current [[State]].
 */
trait Performance extends Machine {
  /**
   * Determines the nominal performance of each [[State]] in a [[Machine]].
   * Must be implemented by the user.
   * @param s A state.
   * @return Nominal performance of the state.
   */
  def sPerf(s: State): Double

  /**
   * Current nominal performance of the [[Machine]]
   * @return Nominal performance at present.
   */
  def curPerf: Double = sPerf(structure)

  override def step: Performance = machineToPerf(this)(sPerf)
}

object Performance{
  def machineToPerf(m: Machine)(sp: State => Double): Performance = m match {
    case SimpleMachine(mk, s) =>
      new SimpleMachine(mk, s) with Performance{def sPerf(s: State) = sp(s)}
    case x: ComplexMachine =>
      new ComplexMachine(x.ms:_*)(x.f) with Performance{def sPerf(s: State) = sp(s)}
//    TODO: make this unapply work
//    case ComplexMachine(ms, f) =>
//      new ComplexMachine(ms)(f) with Performance{def sPerf(s: State) = sp(s)}
  }
}