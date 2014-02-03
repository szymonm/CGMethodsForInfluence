package pl.szymonmatejczyk.competetiveShapley

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge.Implicits._
import scalax.collection.edge.WDiEdge
import scala.util.Random
import scalax.collection.config.CoreConfig

trait LiveGraph {
  val g : Graph[Int, WDiEdge]
  val weightDenominator : Double
  val r : Random
 
  def randomLiveGraph(): Graph[Int, DiEdge] = {
    implicit val config : CoreConfig = new CoreConfig()
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
    val q = scala.collection.mutable.Queue[g.NodeT]()
    q ++= influenceSet.map { x => g.find(x) }.flatten
    val visited = collection.mutable.Set[Int]()
    visited ++= influenceSet.toIterable
    while (!q.isEmpty) {
      val cur = q.dequeue
      cur.outgoing.foreach {
        e =>
          if (!visited.contains(e._2.value) && e.weight / weightDenominator > r.nextDouble) {
            q += e._2
            visited += e._2.value
          }
      }
    }
    Set[Int]() ++ visited.toList
  }
}