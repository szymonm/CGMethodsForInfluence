package pl.szymonmatejczyk.competetiveShapley.topKNodesAlgorithms

import collection._
import pl.szymonmatejczyk.competetiveShapley.InfluenceNetwork
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration._

trait CelfPlusPlus {
  self : InfluenceNetwork =>
    
  val AWAIT_TIME = 1.second

  case class NodeRecord(var mg1 : Double, prevBest : Option[g.NodeT], mg2 : Double, var flag : Int = 0)
  
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
        val f = mcIncrementallyReachable(seed.view.map(_.value).toSet, Seq(bestNode), MCRuns)
        ???
        Await.result(f, AWAIT_TIME) match {
          case withOutBest +: withBest +: Seq() => (withOutBest, withBest) 
        }
      case None => (Await.result(mcSpreadingFrom(seed.view.map(_.value).toSet, MCRuns), 
          AWAIT_TIME), 0.0)
    }
  }
  
  def delta(node : g.NodeT, curBest : Option[g.NodeT], seed : Iterable[g.NodeT], MCRuns : Int) 
    : (Double, Double) = {
    curBest match {
      case Some(bestNode) =>
        val f1 = mcIncrementallyReachable(seed.view.map(_.value).toSet, Seq(node.value), MCRuns)
        val f2 = mcIncrementallyReachable(seed.view.map(_.value).toSet, Seq(bestNode.value, 
            node.value), MCRuns)
        ???
    }
    ???
  } 
  
  def computeTopKCpp(k : Int, MCRuns : Int = 10000) : Seq[Int] = {
    val seedSet = mutable.ListBuffer[g.NodeT]()
    val records = mutable.Map[g.NodeT, NodeRecord]()
    
    implicit val nodesOrdering : Ordering[g.NodeT] = 
      Ordering.by[g.NodeT, Double](n => records(n).mg1)
    val priorityQueue = mutable.Queue[g.NodeT]()
    
    var lastSeed : Option[g.NodeT] = None
    var curBest : Option[g.NodeT] = None
    
    def updateCurBest(newBest : Double, newNode : g.NodeT) {
      curBest = curBest match {
        case Some(value) if (value < newBest) => Some(newNode)
        case _ => curBest
      }
    }
    
    g.nodes.foreach{
      node => 
        val (mg1, mg2) = sigma(node, curBest, seedSet, MCRuns)
        records += ((node, new NodeRecord(mg1, curBest, mg2, 0)))
        priorityQueue += node
        updateCurBest(mg1, node)
    }
    
    while (seedSet.size < k) {
      val current = priorityQueue.dequeue()
      if (records(current).flag == seedSet.size) {
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
        priorityQueue += current
      }
      
    }
    
    seedSet.map(_.value).toSeq
   }
}
