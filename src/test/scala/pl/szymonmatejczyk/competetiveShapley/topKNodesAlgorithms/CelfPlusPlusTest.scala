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
class CelfPlusPlusTest extends FlatSpec with ShouldMatchers {
  "Celf++" should "work the same as greedy algorithm" in {
    val g1 = Graph(1,2,3,4,5,6, 1~>3 % 40, 3~>1 %20, 3~>5 % 90, 5~>3 % 60, 
        2~>4 % 30, 4~>2 % 50, 4~>5 % 10, 5~>4 % 20, 5~>6 % 70, 6~>4 % 30)
    val gNet = new InfluenceNetwork(g1, 100)
    val greedyLdagResult = gNet.greedyMostInfluentSearch(3).toSet
    gNet.computeTopKCpp(3, 10000).toSet shouldEqual greedyLdagResult
  }
}