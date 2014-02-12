package pl.szymonmatejczyk.competetiveShapley.utils

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.util.Random
import pl.szymonmatejczyk.competetiveShapley.utils.random.RandomCategoricalDistributionExtension

@RunWith(classOf[JUnitRunner])
class RandomCategoricalDistributionExtensionTest extends FlatSpec with ShouldMatchers {
  import RandomCategoricalDistributionExtension._
  
  val r = new Random
  
  "random" should "throw illegal argument exception" in {
    
  }
}