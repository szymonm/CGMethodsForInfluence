package pl.szymonmatejczyk.competetiveShapley.topKNodesAlgorithms.cg

import collection._

import pl.szymonmatejczyk.competetiveShapley._
import pl.szymonmatejczyk.competetiveShapley.InfluenceNetwork

trait ShapleyValueWithDiscount {
  self : InfluenceNetwork =>
    
  def topKFringeSVWithDiscount(k : Int) : Seq[Int] = {
    if (k > size) {
      throw new IllegalArgumentException("K greater than number of nodes in graph.")
    }
    
    val discountedShapley = mutable.Map[g.NodeT, Double]()
    
    val byDiscountedShapleyOrdering = Ordering.by[g.NodeT, Double](n => discountedShapley(n))
    val DSVPriorityQueue = mutable.PriorityQueue[g.NodeT]()(byDiscountedShapleyOrdering)
    
    g.nodes.foreach{
      node =>
        discountedShapley += ((node, computeSingleSV(node.value)))
        DSVPriorityQueue += node
    }
    
    val outdatedPosition = mutable.Set[g.NodeT]()
    
    val res = mutable.ListBuffer[Int]()
    
    while (res.size < k) {
      val current = DSVPriorityQueue.dequeue
      if (outdatedPosition.contains(current)) {
        DSVPriorityQueue += current
        outdatedPosition -= current
      } else {
        res += current.value
        for (neighbour <- current.inNeighbors) {
          discountedShapley += ((neighbour, discountedShapley(neighbour) - 
              (1.0 / (1 + current.inDegree))))
          outdatedPosition += neighbour
        } 
      }
    }
    res
  }
}

object ShapleyValueWithDiscount {
    def influenceHeuristic : InfluenceHeuristic = new InfluenceHeuristic("SVWithDiscount", 
        (in : IN) => (k : Int) => in.topKFringeSVWithDiscount(k))
}