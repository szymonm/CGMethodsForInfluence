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

  /**
   * Returns a random subgraph of this InfluenceNetwork.
   * 
   * Edge (u ~> v % w) is taken to the subgraph with probability w.
   */
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
   * Returns nodes that are randomly influenced when adding q to initial seed assuming
   * visited are already visited.
   * 
   * Assumes that q and visited are disjoint.
   */
  def randomlyInfluencedFromQueue(q : immutable.Queue[g.NodeT], visited : Set[Int]): Set[Int] = {
    val seed = immutable.Set[Int]() ++ q.map(_.value)
    val added = mutable.Set[Int]()
    val queue = mutable.Queue[g.NodeT]() ++ q
    while (!queue.isEmpty) {
      val cur = queue.dequeue
      cur.outgoing.iterator
        .filter(e => !visited.contains(e._2.value))
        .filter(e => !seed.contains(e._2.value))
        .filter(e => !added.contains(e._2.value))
        .foreach {
          e =>
            if ((e.weight.toDouble / weightDenominator) > r.nextDouble) {
              queue += e._2
              added += e._2.value
            }
        }
    }
    seed ++ added
  }
  
  /**
   * Returns a pair:
   *   1) Nodes randomly influenced by startSet
   *   2) A sequences of sets - nodes influenced by additionalNodes assuming
   *      that an influence campaign from startSet already happened.
   */
  def incrementallyReachableNodes(startSet : Set[Int], additionalNodes : Seq[Int]) : 
      (Set[Int], Seq[Set[Int]]) = {
    val q = immutable.Queue[g.NodeT]() ++ startSet.map (x => g.get(x))
    val visited = immutable.Set[Int]() ++ startSet.toIterable
    val added = randomlyInfluencedFromQueue(q, visited)
    val initiallyInfluenced = Set[Int]() ++ visited ++ added
    val additionalSets: Seq[Set[Int]] = additionalNodes.map{
      node =>
        if (initiallyInfluenced.contains(node)) {
          Set[Int]()
        } else {
          randomlyInfluencedFromQueue(immutable.Queue(g.get(node)), 
                                      initiallyInfluenced) + node
        }
    }
    (initiallyInfluenced, additionalSets)
  } 
  
  def mcSpreadFrom(influenceSet : Set[Int]) : Future[Int] = {
    Future {
      randomLiveGraphFrom(influenceSet).size
    }
  }
  
  def mcSpreadingFrom(influenceSet : Set[Int], n : Int) : Future[Double] = {
    import FutureExtensions._
    Future.sequence(Iterator.range(0,n).map{_ => mcSpreadFrom(influenceSet)}.toList).
      map(l => l.sum.toDouble / n)
  } 
}