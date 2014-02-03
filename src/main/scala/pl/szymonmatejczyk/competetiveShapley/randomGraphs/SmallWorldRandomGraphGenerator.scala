package pl.szymonmatejczyk.competetiveShapley.randomGraphs

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge.Implicits._
import scala.util.Random
import scalax.collection.edge.WDiEdge
import scala.math.abs

import pl.szymonmatejczyk.competetiveShapley.common._

/**
 * See: J. Kleinberg. The small-world phenomenon: An algorithmic perspective. 
 * Proc. 32nd ACM Symposium on Theory of Computing, 2000.
 */
class SmallWorldRandomGraphGenerator(p : Int, q : Int, r : Double) {
  val random = new Random
  
  def generateGraph[N : Manifest](nodes : Seq[N]) : Graph[N, WDiEdge] = {
    val possiblePositions = for (x <- (1 to nodes.size); y <- (1 to nodes.size)) yield (x, y)
    val positions = nodes.zip(random.shuffle(possiblePositions).take(nodes.size)).toMap
    
    def distance(x : N, y : N) : Int = {
      abs(positions(x)._1 - positions(y)._1) + abs(positions(x)._2 - positions(y)._2)
    }
    
    val shortEdges = for {
      x <- nodes
      y <- nodes
      if (distance(x, y) <= p)
    } yield x ~> y % 1L
 //TODO   
//    def longRandomEdge(from : N) : WDiEdge[N] = {
//      
//    }
    val longEdges = List() 
//      for {
//      x <- nodes
//      count <- (1 to q)
//    } yield longRandomEdge(x)
    val edges = longEdges ++ shortEdges
    Graph.from(nodes, edges)
  }
}