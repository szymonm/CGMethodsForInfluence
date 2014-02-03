package pl.szymonmatejczyk.competetiveShapley

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import pl.szymonmatejczyk.competetiveShapley.coalitionGeneration.RandomCoalitionGenerator
import pl.szymonmatejczyk.competetiveShapley.utils.GeneratorOverIterator

class CoalitionGeneratorTest extends FlatSpec with ShouldMatchers {
  "Random coalition generator" should "generate given number of coalitions" in {
    val r = new RandomCoalitionGenerator(Set[Int](1,2,3,4,5,6), 10)
    val it = new GeneratorOverIterator(r.generate _)
    it.size shouldEqual 10
  }
}