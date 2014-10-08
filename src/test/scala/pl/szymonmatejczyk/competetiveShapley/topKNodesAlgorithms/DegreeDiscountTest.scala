package pl.szymonmatejczyk.competetiveShapley.topKNodesAlgorithms
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.util.Random
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge.Implicits._
import scalax.collection.edge.WDiEdge

import pl.szymonmatejczyk.competetiveShapley.graphs.WeightedDirectedNetwork
import pl.szymonmatejczyk.competetiveShapley.InfluenceNetwork

@RunWith(classOf[JUnitRunner])
class DegreeDiscountTest extends FlatSpec with ShouldMatchers {

  "DegreeDiscount" should "return center node of a star" in {
    val starOut = WeightedDirectedNetwork.starOut(5, 1, 1)
    val network = InfluenceNetwork(starOut)
    network.computeTopNodesDD().head shouldEqual 1
  }
  
  it should "return only one node of a clique" in {
    val clique = WeightedDirectedNetwork.clique(3, 1, 1)
    val edges = List(1 ~> 4 % 1, 4 ~> 5 % 1 ,2 ~> 5 % 1,
                     1 ~> 5 % 1)
    val network = new InfluenceNetwork(clique.graph ++ edges, 1)
    network.computeTopNodesDD.take(3) shouldEqual Seq(1, 2, 4)
  }
}