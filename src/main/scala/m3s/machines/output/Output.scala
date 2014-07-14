package m3s.machines.output

import m3s.{Performance, State}

/**
 * Maps states into performances in [[m3s.machines.SimpleMachine]].
 */
trait Output {
  def map(s: State): Performance

  def apply(s: State) = map(s)
}
