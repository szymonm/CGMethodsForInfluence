package pl.szymonmatejczyk.competetiveShapley.topKNodesAlgorithms

import collection._
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration._
import com.typesafe.scalalogging.slf4j.Logging

import pl.szymonmatejczyk.competetiveShapley._
import pl.szymonmatejczyk.competetiveShapley.common._
import pl.szymonmatejczyk.competetiveShapley.InfluenceNetwork

trait CelfPlusPlus extends Logging {
  self : InfluenceNetwork =>
    
  val AWAIT_TIME = 1.second

  case class NodeRecord(var mg1 : Double, prevBest : Option[g.NodeT], mg2 : Double, 
                        var flag : Int = 0)

  /*
   * If curBest is None returns influence spread of node given seed is already influenced.
   * 
   * If curBest is set, return spread of node given seed and spread of node given seed
   * and curBest. 
   */
  def sigma(node : g.NodeT, curBest : Option[g.NodeT], seed : Iterable[g.NodeT], MCRuns : Int) : 
      (Double, Double) = {
    curBest match {
      case Some(bestNode) => 
        val r = mcTotalInfluence(seed.toSet, Seq(Seq(node), Seq(bestNode, node)), MCRuns)
        (r(0), r(1))
      case None => (mcTotalInfluence(seed.toSet, Seq(Seq(node)), MCRuns)(0), 0.0)
    }
  }
  
  def delta(node : g.NodeT, curBest : Option[g.NodeT], seed : Iterable[g.NodeT], MCRuns : Int) 
    : (Double, Double) = {
    curBest match {
      case Some(bestNode) =>
        val r = mcIncrementalInfluence(seed.toSet, Seq(Seq(node), Seq(bestNode, node)), MCRuns)
        (r(0), r(1))
      case None =>
        (mcIncrementalInfluence(seed.toSet, Seq(Seq(node)), MCRuns)(0), 0.0)
    }
  } 
  
  def computeTopKCpp(k : Int, MCRuns : Int = 10000) : Seq[Int] = {
    val seedSet = mutable.ListBuffer[g.NodeT]()
    val records = mutable.Map[g.NodeT, NodeRecord]()
    
    val nodesOrdering : Ordering[g.NodeT] = 
      Ordering.by[g.NodeT, Double](n => records(n).mg1)
    val priorityQueue = new mutable.PriorityQueue[g.NodeT]()(nodesOrdering)
    
    var lastSeed : Option[g.NodeT] = None
    var curBest : Option[g.NodeT] = None
    
    def updateCurBest(newBest : Double, newNode : g.NodeT) {
      curBest = curBest match {
        case Some(oldBest) if (records(oldBest).mg1 < newBest) => Some(newNode)
        case _ => curBest
      }
    }
    
    g.nodes.foreach{
      node => 
        val (mg1, mg2) = sigma(node, curBest, seedSet, MCRuns)
        logger.debug(s"${node.value} mgs: $mg1 $mg2")
        records += ((node, new NodeRecord(mg1, curBest, mg2, 0)))
        priorityQueue += node
        updateCurBest(mg1, node)
    }
    
    while (seedSet.size < k) {
      val current = priorityQueue.dequeue()
      logger.debug(s"Taking ${current.value}")
      if (records(current).flag == seedSet.size) {
        logger.debug(s"Adding to seed set")
        seedSet += current
        lastSeed = Some(current)
      } else {
        if (records(current).prevBest == lastSeed) {
          records(current).mg1 = records(current).mg2
          records(current).flag = seedSet.size
        } else {
          val (mg1, mg2) = delta(current, curBest, seedSet, MCRuns)
          records += ((current, new NodeRecord(mg1, curBest, mg2, seedSet.size)))
        }
        logger.debug(s"${current.value} mgs: ${records(current).mg1} ${records(current).mg2}")
        updateCurBest(records(current).mg1, current)
        priorityQueue += current
      }
      
    }
    
    seedSet.map(_.value).toSeq
   }
}

object CelfPlusPlus {
  def influenceHeuristic(MCRuns : Int) : InfluenceHeuristic = new InfluenceHeuristic("celf++", 
      (in : IN) => (k : Int) => in.computeTopKCpp(k, MCRuns))
}