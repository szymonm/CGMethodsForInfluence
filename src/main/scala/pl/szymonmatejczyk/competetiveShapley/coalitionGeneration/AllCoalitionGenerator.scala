package pl.szymonmatejczyk.competetiveShapley.coalitionGeneration

import scala.collection._

class AllCoalitionGenerator(from: Set[Int]) extends CoalitionGenerator {
  val subsetsIterator = from.subsets
  override def generate(): Option[Set[Int]] = {
    if (subsetsIterator.hasNext) {
      Some(subsetsIterator.next)
    } else {
      None
    }
  }
}