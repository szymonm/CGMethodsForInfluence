package pl.szymonmatejczyk.competetiveShapley.randomGraphs

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge.Implicits._
import scala.util.Random
import scalax.collection.edge.WDiEdge

import pl.szymonmatejczyk.competetiveShapley.common._

class ErdosRandomGraphGenerator(p: Double) extends GraphGenerator {
  val r = new Random
  override def generateGraph[N: Manifest](nodes: Seq[N]): Graph[N, WDiEdge] = {
    val edges = for {
      x <- nodes
      y <- nodes
      if r.nextDouble < p
    } yield x ~> y % 1L
    Graph.from(nodes, edges)
  }
}