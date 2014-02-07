package pl.szymonmatejczyk.competetiveShapley.graphs
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge.Implicits._
import scalax.collection.edge.WDiEdge
import scala.collection._

class WeightedDirectedNetwork(val g: Graph[Int, WDiEdge], val weightDenominator: Double = 100000.0)
        extends Network[Int, WDiEdge](g, Some(weightDenominator))  {
}

object WeightedDirectedNetwork {
  private def from(nodes : Iterable[Int], edges : Iterable[WDiEdge[Int]], edgeDenominator : Int) : 
      WeightedDirectedNetwork = {
    new WeightedDirectedNetwork(Graph.from[Int, WDiEdge](nodes, edges), edgeDenominator)
  }
  
  def twoNodes(edgeWeight : Int, edgeDenominator : Int) = 
    new WeightedDirectedNetwork(Graph(1 ~> 2 % edgeWeight), edgeDenominator)
  
  def starOut(nodesNum : Int, edgeWeight : Int, edgeDenominator : Int) : WeightedDirectedNetwork = {
    val nodes = 1.until(nodesNum)
    val edges = 2.until(nodesNum).map(x => 1 ~> x % edgeWeight).toIterable
    from(nodes, edges, edgeDenominator)
  }
  
  def starIn(nodesNum : Int, edgeWeight : Int, edgeDenominator : Int) : WeightedDirectedNetwork = {
    val nodes = 1.until(nodesNum)
    val edges = 2.until(nodesNum).map(x => x ~> 1 % edgeWeight).toIterable
    from(nodes, edges, edgeDenominator)
  }
  
  def cycle(n : Int, edgeWeight : Int, edgeDenominator : Int) : WeightedDirectedNetwork = {
    val nodes = 1.until(n)
    val edges = nodes.map(x => x ~> ((x % n) + 1) % edgeWeight).toIterable
    from(nodes, edges, edgeDenominator)
  }
}