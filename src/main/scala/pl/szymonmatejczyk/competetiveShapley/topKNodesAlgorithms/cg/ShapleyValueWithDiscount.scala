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

    val fringeSV = mutable.Map[g.NodeT, Double]()
    val discountedShapley = mutable.Map[g.NodeT, Double]()
    
    val byDiscountedShapleyOrdering = Ordering.by[g.NodeT, Double](n => discountedShapley(n))
    val DSVPriorityQueue = mutable.PriorityQueue[g.NodeT]()(byDiscountedShapleyOrdering)
    
    var maxSV = 0.0
    g.nodes.foreach{
      node =>
        val sv = computeSingleSV(node.value)
        maxSV = math.max(maxSV, sv)
        fringeSV += ((node, sv))
        discountedShapley += ((node, sv))
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
        for (neighbour <- current.outNeighbors) {
          discountedShapley += ((neighbour, fringeSV(neighbour) - (maxSV + 1))) 
          outdatedPosition += neighbour
          val neighboursOfNeighbour = neighbour.inNeighbors.filter(discountedShapley(_) > 0.0)
          for (neighbourOfNeighbour <- neighboursOfNeighbour) {
            discountedShapley += ((neighbourOfNeighbour, discountedShapley(neighbourOfNeighbour) -
              (1.0 / (1 + neighbour.inDegree))))
          }
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