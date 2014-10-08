package pl.szymonmatejczyk.competetiveShapley

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge.Implicits._
import scala.collection.immutable.HashMap
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import pl.szymonmatejczyk.competetiveShapley.graphs.WeightedDirectedNetwork

@RunWith(classOf[JUnitRunner])
class LinearThresholdTest extends FlatSpec with ShouldMatchers {
  "LinearThreshold" should "activate cycle" in {
    val cycle8 = WeightedDirectedNetwork.cycle(8, 10, 10)
    val in = new InfluenceNetwork(cycle8.graph, 10)
    in.mcLinearThresholdSeedQuality(Seq(1), 100) should be (8.0 plusOrMinus 0.2)
    
    in.mcLinearThresholdSeedQuality(Seq(1, 2), 100) should be (8.0 plusOrMinus 0.2)
    
    val biCycle8 = WeightedDirectedNetwork.biCycle(8, 10, 10)
    val in2 = new InfluenceNetwork(biCycle8)
    
    in2.mcLinearThresholdSeedQuality(Seq(1), 100) should be (8.0 plusOrMinus 0.2)
    in2.mcLinearThresholdSeedQuality(Seq(1,2), 100) should be (8.0 plusOrMinus 0.2)

    in2.randomLinearThresholdSeedQuality(Seq(1), () => 1.2) shouldEqual 1
    
    in2.mcLinearThresholdSeedQuality(Seq(1), 100, () => 1.2) should be (1.0 plusOrMinus 0.2)
    in2.mcLinearThresholdSeedQuality(Seq(1, 2), 100, () => 1.2) should be (2.0 plusOrMinus 0.2)

    val biCycle10 = WeightedDirectedNetwork.biCycle(10, 5, 10)
    val in3 = new InfluenceNetwork(biCycle10)
    in3.mcLinearThresholdSeedQuality(Seq(1), 100) should be (3.0 plusOrMinus(0.2))
    
    val inStar = new InfluenceNetwork(WeightedDirectedNetwork.starIn(9, 1, 8))
    inStar.mcLinearThresholdSeedQuality(Seq(2,3,4,5,6,7,8,9), 10, () => 1.0) should be (
        9.0 plusOrMinus 0.01)
  }
}