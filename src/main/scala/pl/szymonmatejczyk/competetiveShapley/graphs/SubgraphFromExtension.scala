package pl.szymonmatejczyk.competetiveShapley.graphs

import scala.language.higherKinds
import scalax.collection._
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge.Implicits._
import scalax.collection.config.CoreConfig
import scala.util.Random
import scalax.collection.GraphTraversal
import scalax.collection.edge.WDiEdge
import scala.reflect.ClassTag
import com.typesafe.scalalogging.slf4j.Logging

object SubgraphFromExtension extends Logging {
  val r = new Random
  
  /**
   * Returns pseudo random subgraph of @param graph using BFS from 
   * @param fromNode or a random node not bigger than @param size nodes.
   */
  def randomSubgraph[N: Manifest, E[N] <: EdgeLikeIn[N]](graph: Graph[N, E], size: Int,
    fromNode: Option[N]): Graph[N, E] = {
    def getRandomNode(): graph.NodeT = {
      //      graph.filterNot(graph.having(node = _.outDegree > 4)).nodes.draw(r)
      val arr = graph.nodes.filter(_.outDegree > 4).toIndexedSeq
      val pos = r.nextInt(arr.size)
      arr(pos)
    }
    val startingNode: graph.NodeT = fromNode match {
      case Some(x) => graph.getOrElse(x, getRandomNode)
      case None => getRandomNode
    }
    var nodes = Set[N]()
    startingNode.traverse(maxDepth = math.sqrt(size).toInt)(nodeVisitor =
      u => {
        nodes += (u.value)
        if (nodes.size < size)
          GraphTraversal.VisitorReturn.Continue
        else
          GraphTraversal.VisitorReturn.Cancel
      })
    if (nodes.size < size / 2) {
      logger.warn("Subgraph degenerated...restarting")
      randomSubgraph(graph, size, fromNode)
    } else
      graph filter (graph.having(node = nodes.contains(_)))
  }
}
