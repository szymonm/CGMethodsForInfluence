package pl.szymonmatejczyk.competetiveShapley.topKNodesAlgorithms

import collection._

import pl.szymonmatejczyk.competetiveShapley.InfluenceNetwork

trait DegreeDiscount {
  self : InfluenceNetwork => 
  
  def computeTokKNodesDD(k : Int) : Seq[Int] = {
    if (g.nodes.size < k) {
      return g.nodes.map(_.value).toSeq
    }
    
    val discountedDegree = mutable.Map[g.NodeT, Int]()
    
    implicit val discountedDegreeOrdering : Ordering[g.NodeT] = 
      Ordering.by[g.NodeT, Int](n => discountedDegree(n))
      
    val DDPriorityQueue = mutable.PriorityQueue[g.NodeT]()
    
    val outdatedPosition = mutable.Set[g.NodeT]()
    
    g.nodes.foreach{
      node => 
        discountedDegree += ((node, node.outDegree))
        DDPriorityQueue += node
    }
    
    val res = mutable.ListBuffer[Int]()
    
    while (res.size < k) {
      val current = DDPriorityQueue.dequeue()
      if (outdatedPosition.contains(current)) {
        DDPriorityQueue += current
        outdatedPosition -= current
      } else {
        res += current
        current.inNeighbors.foreach {
          neighbour =>
              discountedDegree += ((neighbour, discountedDegree(neighbour) - 1))
              outdatedPosition += neighbour
        }
      }
    }
    
    res.toList
  }

}