package pl.szymonmatejczyk.competetiveShapley.utils

import scala.util.Random

object RandomCategoricalDistributionExtension {
  implicit class RandomPowerCategoricalDistributionExtension(val r: Random) {
    def nextFromCategoricalDistribution(probabilities: Seq[Double]): Int = {
      val rdm = r.nextDouble
      var sum = 0.0
      probabilities.iterator.zipWithIndex.dropWhile { x => { sum += x._1; sum <= rdm } } match {
        case x if x.hasNext => x.next()._2
        case _ => throw new IllegalArgumentException("wrong categorical distribution probabilities")
      }
    }
  }
}