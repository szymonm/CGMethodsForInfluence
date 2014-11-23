package pl.szymonmatejczyk.competetiveShapley

import pl.szymonmatejczyk.competetiveShapley.utils.GeneratorOverIterator
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import pl.szymonmatejczyk.competetiveShapley.algorithms.cg.BanzhaffIndexApproximator

class BanzhaffIndexApproximatorTest extends FlatSpec with ShouldMatchers {
  class SimpleGenerator {
    var count = 0
    def generate() : Option[Double] = {
      if (count < 5) {
        count += 1
        return Some(count)
      } else {
        return None
      }
    }
  }
  
  val GOI = new GeneratorOverIterator((new SimpleGenerator).generate _)
  val BIA = new BanzhaffIndexApproximator
  "Banzhaf index approximator" should "compute mean" in {
    BIA.mean(GOI.toSeq) should equal (3.0)
  }
}