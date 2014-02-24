package pl.szymonmatejczyk.competetiveShapley.liveGraphs

import collection._
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge.Implicits._
import scalax.collection.config.CoreConfig
import scala.concurrent._
import ExecutionContext.Implicits.global
import pl.szymonmatejczyk.competetiveShapley.InfluenceNetwork
import pl.szymonmatejczyk.competetiveShapley.utils.FutureExtensions.FutureCompanionOps
import scala.Option.option2Iterable
import pl.szymonmatejczyk.competetiveShapley.utils.FutureExtensions

trait LiveGraph {
  self : InfluenceNetwork => 

  def randomLiveGraph(): Graph[Int, DiEdge] = {
    implicit val config: CoreConfig = new CoreConfig()
    val builder = Graph.newBuilder[Int, DiEdge]
    g.edges.toIterable.foreach {
      e =>
        if ((e.weight / weightDenominator) > r.nextDouble)
          builder += (e._1.value ~> e._2.value)
    }
    g.nodes.toIterable.foreach {
      n => builder += n.value
    }
    builder.result
  }

  def randomLiveGraphFrom(influenceSet: Set[Int]): Set[Int] = {
    incrementallyReachableNodes(influenceSet, Seq())._1
  }
  
  /**
   * Returns nodes that become reachable when adding q to initial seed when visited
   * are already visited.
   */
  def randomlyReachableFromQueue(q : immutable.Queue[g.NodeT], visited : Set[Int]) : 
      Set[Int] = {
    val added = mutable.Set[Int]()
    val queue = mutable.Queue() ++ q
    while (!queue.isEmpty) {
      val cur = queue.dequeue
      cur.outgoing.foreach {
        e =>
          if (!visited.contains(e._2.value) && !added.contains(e._2.value) 
              && e.weight.toDouble / weightDenominator > r.nextDouble) {
            queue += e._2
            added += e._2.value
          }
      }
    }
    added
  }
  
  def incrementallyReachableNodes(startSet : Set[Int], additionalNodes : Seq[Int]) : 
      (Set[Int], Seq[Set[Int]]) = {
    val q = immutable.Queue[g.NodeT]() ++ startSet.map { x => g.find(x) }.flatten
    val visited = immutable.Set[Int]() ++ startSet.toIterable
    val added = randomlyReachableFromQueue(q, visited)
    val initiallyReachable = Set[Int]() ++ visited ++ added
    val additionalSets = additionalNodes.map{
      node => 
        randomlyReachableFromQueue(immutable.Queue(g.get(node)), visited)
    }
    (initiallyReachable, additionalSets)
  } 
  
  def mcSpreadFrom(influenceSet : Set[Int]) : Future[Int] = {
    future {
      randomLiveGraphFrom(influenceSet).size
    }
  }
  
  def mcSpreadingFrom(influenceSet : Set[Int], n : Int) : Future[Double] = {
    import FutureExtensions._
    Future.all(Iterator.range(0,n).map{_ => mcSpreadFrom(influenceSet)}.toList).
      map(l => l.sum.toDouble / n)
  } 
}