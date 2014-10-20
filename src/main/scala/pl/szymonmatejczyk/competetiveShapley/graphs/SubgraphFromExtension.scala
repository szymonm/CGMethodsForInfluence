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
import com.typesafe.scalalogging.LazyLogging
import scalax.collection.GraphTraversal._
import scala.util.control.Breaks._

object SubgraphFromExtension extends LazyLogging {
  val r = new Random
  
  val MAX_RUNS = 25

  /**
   * Returns pseudo random subgraph of @param graph using BFS from 
   * @param fromNode or a random node not bigger than @param size nodes.
   */
  def randomSubgraph[N: Manifest, E[N] <: EdgeLikeIn[N]](graph: Graph[N, E], size: Int,
    fromNode: Option[N], randomNodeMinOutDegre: Int = 4, runNo : Int = 0): Graph[N, E] = {
    def getRandomNode(): graph.NodeT = {
      //      graph.filterNot(graph.having(node = _.outDegree > 4)).nodes.draw(r)
      val arr = graph.nodes.filter(_.outDegree > randomNodeMinOutDegre).toIndexedSeq
      val pos = r.nextInt(arr.size)
      arr(pos)
    }
    val startingNode: graph.NodeT = fromNode match {
      case Some(x) => graph.getOrElse(x, getRandomNode)
      case None => getRandomNode
    }
    var nodes = Set[N]()
    breakable {
    startingNode.outerNodeTraverser(Parameters(direction=AnyConnected,
        maxDepth=2 * math.sqrt(size).toInt))
        .foreach(
      u => {
        nodes += (u.value)
        if (!(nodes.size < size))
          break
      })
    }
    if (nodes.size < size / 2) {
      logger.warn("Subgraph degenerated...restarting")
      if (runNo == MAX_RUNS) {
        throw new IllegalArgumentException("Unable to random nondegenerated graph.")
      }
      randomSubgraph(graph, size, fromNode, runNo + 1)
    } else
      graph filter (graph.having(node = nodes.contains(_)))
  }
}
