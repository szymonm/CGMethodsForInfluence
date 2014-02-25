package pl.szymonmatejczyk.competetiveShapley.graphs
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge.Implicits._
import pl.szymonmatejczyk.competetiveShapley.InfluenceNetwork

trait SizeRestriction {
  self : InfluenceNetwork =>
    
  def restrictSize(maxSize: Int, fromNode: Option[Int] = None): InfluenceNetwork = {
    if (size < maxSize)
      self
    else {
      val newGraph = SubgraphFromExtension.randomSubgraph(g, maxSize, fromNode)
      new InfluenceNetwork(newGraph, weightDenominator)
    }
  }
}