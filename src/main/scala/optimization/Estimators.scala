package optimization

import scala.math._

//TODO: take a better look at this, something might still be wrong

/**
 * Accumulates simple statistics.
 * @param mean Initial mean
 * @param variance Initial variance
 * @param sampleSize Sample size
 */
class StatisticsAccumulator(val mean: Double, val variance: Double, val sampleSize: Int) {
  require(sampleSize >= 2)

  /**
   * Updates the accumulator with a new sample.
   * @param newSample New sample
   * @return
   */
  def update(newSample: Double) = {
    val nextSampleSize = sampleSize + 1
    val nextMean = mean + (newSample - mean) / nextSampleSize
    val nextVar = (1.0 - 1.0 / sampleSize) * variance + nextSampleSize * (nextMean - mean) * (nextMean - mean)
    new StatisticsAccumulator(nextMean, nextVar, nextSampleSize)
  }

  def confidence(sd: Double) = 1.96 * sqrt(variance / sampleSize) < sd

  override def toString = s"StatisticsAccumulator($mean, $variance, $sampleSize)"
}

object Estimators {
  /**
   * Meant to estimate the reliability of a multi state system.
   * @param simulation A simulation of a multistate system
   * @param time Objective time
   * @param sdev Allowed standard deviation
   * @param p Simulation predicate
   * @tparam A Simulation type
   * @return StatisticsAccumulator
   */
  def reliabilityEstimator[A](simulation: Simulation[A],
                              time: Int,
                              sdev: Double)
                             (p: A => Boolean) = {
    val initialSims = List.fill(10)(simulation.runWhile(time)(p))
    val randomVars = for ((obj, t) <- initialSims) yield if (t == time) 1 else 0
    val mean = randomVars.sum / 10.0
    val variance = (for (xi <- randomVars) yield (xi - mean) * (xi - mean)).sum / 9.0

    val stats = new StatisticsAccumulator(mean, variance, 10)

    def aux(s: StatisticsAccumulator): StatisticsAccumulator = {
      s.confidence(sdev) match {
        case true => stats
        case false =>
          println(s.confidence(sdev), s.sampleSize)
          val sim = simulation.runWhile(time)(p)
          val sample = if (sim._2 == time) 1 else 0
          aux(s.update(sample))
      }
    }

    aux(stats)
  }
}