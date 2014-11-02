package pl.szymonmatejczyk.competetiveShapley

import org.junit.runner.RunWith
import org.scalatest.Matchers
import org.scalatest.WordSpec
import pl.szymonmatejczyk.competetiveShapley.graphs.WeightedDirectedNetwork
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class LinearThresholdTest extends WordSpec with Matchers {
  "LinearThreshold" should {
    "activate cycle for a seed set" in {
      val cycle8 = WeightedDirectedNetwork.cycle(8, 10, 10)
      val in = new InfluenceNetwork(cycle8.graph, 10)
      in.mcLinearThresholdSeedQuality(Seq(1), 100) should be(8.0 +- 0.2)

      in.mcLinearThresholdSeedQuality(Seq(1, 2), 100) should be(8.0 +- 0.2)
    }

    "activate bi-cycle for a seed set" in {
      val biCycle8 = WeightedDirectedNetwork.biCycle(8, 10, 10)
      val in2 = new InfluenceNetwork(biCycle8)

      in2.mcLinearThresholdSeedQuality(Seq(1), 100) should be(8.0 +- 0.2)
      in2.mcLinearThresholdSeedQuality(Seq(1, 2), 100) should be(8.0 +- 0.2)

      in2.randomLinearThresholdSeedQuality(Seq(1), () => 1.2) shouldEqual 1

      in2.mcLinearThresholdSeedQuality(Seq(1), 100, () => 1.2) should be(1.0 +- 0.2)
      in2.mcLinearThresholdSeedQuality(Seq(1, 2), 100, () => 1.2) should be(2.0 +- 0.2)

      val biCycle10 = WeightedDirectedNetwork.biCycle(10, 5, 10)
      val in3 = new InfluenceNetwork(biCycle10)
      in3.mcLinearThresholdSeedQuality(Seq(1), 100) should be(3.0 +- (0.5))
    }
    
    "activate central node in an inStar" in {
      val inStar = new InfluenceNetwork(WeightedDirectedNetwork.starIn(9, 1, 8))
      inStar.mcLinearThresholdSeedQuality(Seq(2, 3, 4, 5, 6, 7, 8, 9), 10, () => 1.0) should be(
        9.0 +- 0.01)
    }
    
    "incrementally activate proper nodes" in {
      val cycle8 = WeightedDirectedNetwork.cycle(8, 10, 10)
      val in = new InfluenceNetwork(cycle8)
      
      in.incrementalRandomLinearThresholdSeedQuality(Seq(Set(1))) should be (Stream(8))
      
      in.incrementalRandomLinearThresholdSeedQuality(Seq(Set(1), Set(2))) should be (Stream(8, 0))
    }
    
    "incrementally activate bi-cycle for a seed set" which {

      "activates whole cycle in the first step if edges have weight 1" in {
        val in = new InfluenceNetwork(WeightedDirectedNetwork.biCycle(8, 10, 10))
        in.incrementalRandomLinearThresholdSeedQuality(Seq(Set(1))) should be (Stream(8))
        in.incrementalRandomLinearThresholdSeedQuality(Seq(Set(1), Set(2))) should be (Stream(8, 0))
      }

      "activates only seed if edge weights are > 1.0" in {
        val in = new InfluenceNetwork(WeightedDirectedNetwork.biCycle(8, 10, 10))
        in.incrementalRandomLinearThresholdSeedQuality(Seq(Set(1, 2)), () => 1.2) should be (Stream(2))
        
        
        in.incrementalRandomLinearThresholdSeedQuality(Seq(Set(1), Set(2)), () => 1.2) should be (Stream(1, 1))
      }

      "activates correct nodes in consecutive steps" in {
        val biCycle10 = WeightedDirectedNetwork.biCycle(8, 5, 10)
        val in3 = new InfluenceNetwork(biCycle10)
        in3.incrementalRandomLinearThresholdSeedQuality(Seq(Set(1), Set(3), Set(5), Set(7)), () => 1.0) should be
        (Stream(1, 2, 2, 3))
      }
    }
    
    "incrementally activate central node in an inStar" in {
      val inStar = new InfluenceNetwork(WeightedDirectedNetwork.starIn(9, 1, 8))
      inStar.incrementalRandomLinearThresholdSeedQuality(
          Seq(Set(2,3,4), Set(5), Set(6,7,8), Set(9)), 
          () => 0.99) should be (Stream(3, 1, 3, 2))
    }
  }
}