package pl.szymonmatejczyk.competetiveShapley.topKNodesAlgorithms

import collection._
import pl.szymonmatejczyk.competetiveShapley._
import pl.szymonmatejczyk.competetiveShapley.InfluenceNetwork
import scala.annotation.tailrec

trait DegreeDiscount {
  self : InfluenceNetwork => 
  
  def computeTopNodesDD() : Stream[Int] = {
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
    
    @tailrec
    def getNext() : Int = {
      val current = DDPriorityQueue.dequeue()
      if (outdatedPosition.contains(current)) {
        DDPriorityQueue += current
        outdatedPosition -= current
        getNext()
      } else {
        current.inNeighbors.foreach {
                neighbour =>
                  discountedDegree += ((neighbour, discountedDegree(neighbour) - 1))
                  outdatedPosition += neighbour
              }
        current
      }
    }

    def getStream(): Stream[Int] = {
      if (DDPriorityQueue.nonEmpty) {
        getNext() #:: getStream()
      } else {
        Stream.empty[Int]
      }
    }   
    
    getStream()
  }
}

object DegreeDiscount {
  val NAME = "degreeDiscount"
  def influenceHeuristic(): InfluenceHeuristic = new InfluenceHeuristic(NAME,
    (in: IN) => (k: Int) => in.computeTopNodesDD().take(k))

  def influenceHeuristicForSequenceOfK = streamToInfluenceHeuristic(NAME, 
      (in : InfluenceNetwork) => in.computeTopNodesDD())
}