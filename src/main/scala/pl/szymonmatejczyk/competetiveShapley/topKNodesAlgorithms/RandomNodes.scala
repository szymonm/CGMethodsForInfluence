package pl.szymonmatejczyk.competetiveShapley.topKNodesAlgorithms

import collection._

import pl.szymonmatejczyk.competetiveShapley._
import pl.szymonmatejczyk.competetiveShapley.InfluenceNetwork

trait RandomNodes {
  self : InfluenceNetwork =>
    
  def kRandomNodes(k : Int) : Seq[Int] = {
    if (k > g.nodes.size)
      throw new IllegalArgumentException("Not enough nodes in the graph.")
    val randomOrdering = Ordering.by[Int, Double](_ => r.nextDouble)
    val randomOrderQueue = mutable.PriorityQueue[Int]()(randomOrdering) ++= g.nodes.map(_.value)
    val nodesSet = mutable.ListBuffer[Int]()
    while (nodesSet.size < k) {
      nodesSet += randomOrderQueue.dequeue
    }
    nodesSet
  }
}

object RandomNodes {
  def influenceHeuristic(): InfluenceHeuristic = new InfluenceHeuristic("random",
    (in: IN) => (k: Int) => in.kRandomNodes(k))
}