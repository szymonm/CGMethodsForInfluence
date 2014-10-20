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
import org.scalatest.Matchers

@RunWith(classOf[JUnitRunner])
class CelfPlusPlusTest extends FlatSpec with Matchers {
  "Celf++" should "work the same as greedy algorithm" in {
    val g1 = Graph(1,2,3,4,5,6, 1~>3 % 40, 3~>1 %20, 3~>5 % 99, 5~>3 % 60, 
        2~>4 % 30, 4~>2 % 50, 4~>5 % 10, 5~>4 % 20, 5~>6 % 70, 6~>4 % 30)
    val gNet = new InfluenceNetwork(g1, 100)
    val greedyLdagResult = gNet.ldagGreedyTopNodes(Some(0.1)).take(3).toSet
    println("\n\n\n")
    gNet.computeTopCpp(1000).take(3).toSet shouldEqual greedyLdagResult
  }
}