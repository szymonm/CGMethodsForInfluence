package pl.szymonmatejczyk.competetiveShapley.coalitionGeneration

import scala.util.Random
import scala.collection._
import java.util.concurrent.atomic.AtomicInteger

class RandomCoalitionGenerator(val from: Set[Int], coalitions: Int) extends CoalitionGenerator {
  val r = new Random
  val counter = new AtomicInteger(0)

  override def generate(): Option[Set[Int]] = {
    if (from.isEmpty) {
      return None
    }

    if (counter.getAndIncrement() < coalitions) {
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