package pl.szymonmatejczyk.competetiveShapley

import scala.collection._
import scalax.collection.edge.WDiEdge

object SubgraphVisitor {
  def edgeFilter(subgraph: immutable.Map[Int, Int], increasingKeys: Boolean,
    predecessors: Boolean = false)(edge: WDiEdge[Int]): Boolean = {
    if (subgraph.contains(edge._2) && subgraph.contains(edge._1)) {
      (increasingKeys ^ predecessors) == (subgraph(edge._2) > subgraph(edge._1))
    } else
      false
  }
}