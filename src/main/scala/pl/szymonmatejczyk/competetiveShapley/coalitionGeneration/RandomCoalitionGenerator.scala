package pl.szymonmatejczyk.competetiveShapley.coalitionGeneration

import scala.util.Random
import scala.collection._

class RandomCoalitionGenerator(val from: Set[Int], coalitions: Int) extends CoalitionGenerator {
  val r = new Random
  var counter = 0

  override def generate(): Option[Set[Int]] = {
    if (from.isEmpty) {
      return None
    }

    if (counter < coalitions) {
      counter += 1
      Some(randomSubset())
    } else {
      None
    }
  }

  private def randomSubset(): Set[Int] = {
    val res = from.filter(_ => r.nextBoolean())
    res
  }
}