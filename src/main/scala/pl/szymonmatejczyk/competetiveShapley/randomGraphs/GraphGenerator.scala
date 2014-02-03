package pl.szymonmatejczyk.competetiveShapley.randomGraphs

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.edge.WDiEdge

trait GraphGenerator {
  def generateGraph[N: Manifest](nodes: Seq[N]): Graph[N, WDiEdge]
}

object GraphGenerator {
  val DEFAULT_WEIGHT_DENOMINATOR = 10000L
}