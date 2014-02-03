package pl.szymonmatejczyk.competetiveShapley.utils

import scala.util.Random

object RandomDivisionSampler {
  val r = new Random

  // Returns random numberOfParts double which sum is less than 1 
  def sampleDivisionOfUnit(randomGenerator: () => Double)(numberOfParts: Int): Seq[Double] = {
    require(numberOfParts > 0)
    val randoms = (0 to numberOfParts).map { _ => randomGenerator() }
    val sum = randoms.sum
    randoms.map { x => x / sum }.tail
  }

  def sampleDivisionOfUnitUniformly = sampleDivisionOfUnit(r.nextDouble _) _

  def sampleDivisionOfUnitPowerLaw = sampleDivisionOfUnit(powerLawGenerator _) _

  def powerLawGenerator(): Double = {
    val n = 20
    val y = r.nextDouble
    val x0 = 1.0
    val x1 = 5.0
    math.pow(((math.pow(x1, (n + 1)) - 1) * y + 1), (1.0 / (n + 1)))
  }
}