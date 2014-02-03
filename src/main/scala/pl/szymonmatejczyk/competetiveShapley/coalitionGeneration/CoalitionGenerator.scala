package pl.szymonmatejczyk.competetiveShapley.coalitionGeneration

import scala.collection._

trait CoalitionGenerator {
  def generate() : Option[Set[Int]]
}

object CoalitionGenerator {
  def apply(from : Set[Int], coalitions : Int) : CoalitionGenerator = {
    if (math.pow(2, from.size) > coalitions) {
      new RandomCoalitionGenerator(from, coalitions)
    } else {
      new AllCoalitionGenerator(from)
    }
  }
}