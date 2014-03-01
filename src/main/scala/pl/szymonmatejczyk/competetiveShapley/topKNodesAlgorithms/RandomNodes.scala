package pl.szymonmatejczyk.competetiveShapley.topKNodesAlgorithms

import collection._

import pl.szymonmatejczyk.competetiveShapley._
import pl.szymonmatejczyk.competetiveShapley.InfluenceNetwork

trait RandomNodes {
  self : InfluenceNetwork =>
    
  def randomNodes() : Stream[Int] = {
    val randomOrdering = Ordering.by[Int, Double](_ => r.nextDouble)
    val randomOrderQueue = mutable.PriorityQueue[Int]()(randomOrdering) ++= g.nodes.map(_.value)
    
    def getNext() : Int = {
      randomOrderQueue.dequeue
    }
    
    def getStream() : Stream[Int] = {
      if (randomOrderQueue.isEmpty) {
        logger.warn("Nodes finished")
        Stream.empty
      } else {
        getNext() #:: getStream()
      }
    }
    
    getStream()
  }
}

object RandomNodes {
  val NAME = "random"
  def influenceHeuristic: InfluenceHeuristic = new InfluenceHeuristic(NAME,
    (in: IN) => (k: Int) => in.randomNodes().take(k))
  
  def influenceHeuristicForSequenceOfK = streamToInfluenceHeuristic(NAME,
    (in: InfluenceNetwork) => in.randomNodes)
}