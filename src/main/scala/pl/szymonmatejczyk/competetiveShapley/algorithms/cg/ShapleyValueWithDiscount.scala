package pl.szymonmatejczyk.competetiveShapley.algorithms.cg

import collection._
import pl.szymonmatejczyk.competetiveShapley._
import pl.szymonmatejczyk.competetiveShapley.InfluenceNetwork
import scala.concurrent.duration.Duration
import pl.szymonmatejczyk.competetiveShapley.utils.TestingUtils
import scala.annotation.tailrec
import pl.szymonmatejczyk.competetiveShapley.algorithms._

trait ShapleyValueWithDiscount {
  self : InfluenceNetwork =>
    
  def topKFringeSVWithDiscount() : Stream[Int] = {
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
    
    val meanEdgeWeight = g.edges.map(_.weight.toDouble / weightDenominator).sum / g.edges.size
    
    val outdatedPosition = mutable.Set[g.NodeT]()
    
    @tailrec
    def getNext() : Int = {
      val current = DSVPriorityQueue.dequeue
      if (outdatedPosition.contains(current)) {
        DSVPriorityQueue += current
        outdatedPosition -= current
        getNext()
      } else {
        val strongOutNeighbours = current.outgoing
          .filter(e => e.weight / weightDenominator > meanEdgeWeight).map(_._2) 
        for (neighbour <- strongOutNeighbours) {
          discountedShapley += ((neighbour, fringeSV(neighbour) - (maxSV + 1))) 
          outdatedPosition += neighbour
          val neighboursOfNeighbour = neighbour.inNeighbors.filter(discountedShapley(_) > 0.0)
          for (neighbourOfNeighbour <- neighboursOfNeighbour) {
            discountedShapley += ((neighbourOfNeighbour, discountedShapley(neighbourOfNeighbour) -
              (1.0 / (1 + neighbour.inDegree))))
            outdatedPosition += neighbourOfNeighbour
          }
        }
        current
      }
    }
    

    def getStream() : Stream[Int] = {
      if (DSVPriorityQueue.nonEmpty) {
        getNext() #:: getStream()
      } else {
        logger.warn("Nodes finished.")
        Stream.empty
      }
    }
    
    getStream()
  }
}

object ShapleyValueWithDiscount {
  val NAME = "SVWithDiscount"
  def influenceHeuristic: InfluenceHeuristic = new InfluenceHeuristic(NAME,
    (in: IN) => (k: Int) => in.topKFringeSVWithDiscount().take(k))
  
  def influenceHeuristicForSequenceOfK = streamToInfluenceHeuristic(NAME,
    (in: InfluenceNetwork) => in.topKFringeSVWithDiscount())
}