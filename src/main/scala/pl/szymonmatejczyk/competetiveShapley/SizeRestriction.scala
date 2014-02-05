package pl.szymonmatejczyk.competetiveShapley

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge.Implicits._
import scalax.collection.edge.WDiEdge
import pl.szymonmatejczyk.competetiveShapley.graphs.SubgraphFromExtension

trait SizeRestriction {
  self : WeightedDirectedNetwork =>
    
  def restrictSize(maxSize: Int, fromNode: Option[Int] = None): WeightedDirectedNetwork = {
    if (g.size < maxSize)
      self
    else {
      val newGraph = SubgraphFromExtension.randomSubgraph(g, maxSize, fromNode)
      new WeightedDirectedNetwork(newGraph, weightDenominator)
    }
  }
}