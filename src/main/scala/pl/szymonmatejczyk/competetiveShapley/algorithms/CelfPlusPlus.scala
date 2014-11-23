package pl.szymonmatejczyk.competetiveShapley.algorithms

import collection._
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration._
import pl.szymonmatejczyk.competetiveShapley._
import pl.szymonmatejczyk.competetiveShapley.common._
import pl.szymonmatejczyk.competetiveShapley.InfluenceNetwork
import scala.annotation.tailrec
import com.typesafe.scalalogging.LazyLogging

trait CelfPlusPlus extends LazyLogging {
  self : InfluenceNetwork =>
    
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
    logger debug("delta(" + node.value + ") given seed: " + seed.mkString(",") + 
        " cur best: " + curBest.map(_.value))
    curBest match {
      case Some(bestNode) =>
        val r = mcIncrementalInfluence(seed.toSet, Seq(Seq(node), Seq(bestNode, node)), MCRuns)
        (r(0), r(1))
      case None =>
        (mcIncrementalInfluence(seed.toSet, Seq(Seq(node)), MCRuns)(0), 0.0)
    }
  } 
  
  /** Greedy algorithm */
  def computeTopCpp(MCRuns : Int = 10000) : Stream[Int] = {
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
    
    @tailrec
    def getNext() : Int = {
      val current = priorityQueue.dequeue()
      logger.debug(s"Taking ${current.value}")
      if (records(current).flag == seedSet.size) {
        logger.debug(s"Adding to seed set")
        seedSet += current
        lastSeed = Some(current)
        current
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
        getNext()
      }
    }
    
    def getStream() : Stream[Int] = {
      if (priorityQueue.nonEmpty)
        getNext() #:: getStream()
      else
        Stream.empty
    }
    
    getStream()
   }
}

object CelfPlusPlus {
  val NAME = "celf++"
  def influenceHeuristic(MCRuns : Int) : InfluenceHeuristic = new InfluenceHeuristic(NAME, 
      (in : IN) => (k : Int) => in.computeTopCpp(MCRuns).take(k))
  
  def influenceHeuristicForSequenceOfK = streamToInfluenceHeuristic(NAME,
    (in: InfluenceNetwork) => in.computeTopCpp())
}