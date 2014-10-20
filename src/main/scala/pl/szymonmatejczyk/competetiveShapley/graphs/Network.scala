package pl.szymonmatejczyk.competetiveShapley.graphs

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scala.util.Random
import scala.collection.immutable.TreeMap
import pl.szymonmatejczyk.competetiveShapley.utils.Cache
import com.typesafe.scalalogging.LazyLogging

class Network[N, E[X] <: EdgeLikeIn[X]](val graph: Graph[N, E], 
                                        val weightDenominatorOpt : Option[Double]) 
    extends Cache with LazyLogging {

  def clearCache() {
    logger.debug("Network CC")
  }

  def computeBanzhafForSimpleInfectionPropagation(node: N): Double = {
    val singleOnSingleInfluence = Array.fill(graph.order, graph.order)(0.0)
    val currentValues = Array.fill(graph.order)(0.0)

    val nodesToIntMap = graph.nodes.toSeq.zip(0 until graph.order).toMap

    def addN(node: graph.NodeT) = {
      //      singleOnSingleInfluece
    }
    def addE(edge: graph.EdgeT) = {
      val node1num = nodesToIntMap(edge._1)
      val node2num = nodesToIntMap(edge._2)
      currentValues(node2num) += node1num * edge.weight
    }
//    val traversal = graph.newTraversal(nodeVisitor = addN, edgeVisitor = addE)
//    traversal(graph get node, breadthFirst = true)
    (graph get node).innerEdgeTraverser.foreach(addE)
    0.0
  }
}

