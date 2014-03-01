package pl.szymonmatejczyk.competetiveShapley

import scala.collection._
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge.Implicits._
import scalax.collection.edge.WDiEdge
import scala.util.Random
import scalax.collection.config.CoreConfig
import com.typesafe.scalalogging.slf4j.Logger
import org.slf4j.LoggerFactory
import pl.szymonmatejczyk.competetiveShapley.utils.PIMap
import com.typesafe.scalalogging.slf4j.Logging
import scalax.collection.GraphTraversal
import pl.szymonmatejczyk.competetiveShapley.utils.Cache
import pl.szymonmatejczyk.competetiveShapley.graphs.SubgraphVisitor

trait InfluenceComputation extends Cache with Logging {
  val r: Random

  val g: Graph[Int, WDiEdge]
  val weightDenominator: Double

  def getInfluences(): PIMap[Int, Double]
  def computeTotatInfluences(): immutable.Map[Int, Double]

  var ldagNodes: mutable.Map[Int, immutable.Map[Int, Int]]

  var currentInfluences: PIMap[Int, Double] = null
  var incrementalInfluence: collection.mutable.Map[Int, Double] = null
  // Map: (LDAG, node) -> probability
  var activationProbability: collection.mutable.Map[(Int, Int), Double] = null
  var currentInitialSet: Set[Int] = null

  def resetCache() {
    currentInitialSet = null
    currentInfluences = null
    incrementalInfluence = null
    // Map: (LDAG, node) -> probability
    activationProbability = null
  }

  def prepareData() {
    currentInitialSet = collection.mutable.HashSet[Int]()
    currentInfluences = new PIMap[Int, Double](0.0) ++= getInfluences()
    incrementalInfluence = collection.mutable.HashMap[Int, Double]().withDefaultValue(0.0) ++
      computeTotatInfluences()
    activationProbability = collection.mutable.HashMap[(Int, Int), Double]().withDefault(_ => 0.0)
  }

  abstract override def clearCache() {
    logger.debug("Clearing IC data")
    resetCache()
    super.clearCache()
  }

  def computeTotalInfluenceFromActivationProbabilities(): Double = {
    g.nodes.foldLeft[Double](0.0) {
      case (cur, node) =>
        cur + activationProbability((node, node))
    }
  }

  def computeTotalInfluence(initialSet: Iterable[Int]): Double = {
    prepareData()

    initialSet.foreach {
      node =>
        addInfluenceNode(node)
    }
    logger.debug("AP")
    logger.debug(activationProbability.toString)
    computeTotalInfluenceFromActivationProbabilities()
  }

  /* Todo: make this function executable in parallel */
  def addInfluenceNode(addedNode: Int) {
    logger.debug("Added node: " + addedNode)
    currentInfluences.byFirstIterator(addedNode).foreach {
      case ((`addedNode`, to), value) => // to = v
        if (!currentInitialSet.contains(to)) {
          logger.debug("LDAG: " + to + ":" + ldagNodes(to).mkString(";"))
          val deltaInfluence = mutable.HashMap[Int, Double]().withDefaultValue(0.0)
          deltaInfluence += addedNode -> -value
          logger.debug("Influences on: " + to)
          logger.debug(currentInfluences.bySecondIterator(to).toList.mkString(";"))
          // 20
          g.get(addedNode).traverse(direction = GraphTraversal.Predecessors,
            breadthFirst = true,
            edgeFilter = { e: g.EdgeT =>
              SubgraphVisitor.edgeFilter(ldagNodes(to) -- currentInitialSet,
                true, true)(e.toEdgeIn)
            })(
              nodeVisitor = _ => GraphTraversal.VisitorReturn.Continue,
              edgeVisitor = (e => {
                logger.debug("first phase edge: " + e)
                deltaInfluence += (e._1.value) -> (deltaInfluence(e._1.value) +
                  e.weight / weightDenominator * deltaInfluence(e._2.value))
              }))
          // 21
          g.get(addedNode).traverse(direction = GraphTraversal.Predecessors, breadthFirst = true,
            edgeFilter = { e: g.EdgeT =>
              SubgraphVisitor.edgeFilter(ldagNodes(to) -- currentInitialSet,
                true, true)(
                  e.toEdgeIn)
            })(
              nodeVisitor =
                u => {
                  currentInfluences((u, to)) += deltaInfluence(u) // 21
                  incrementalInfluence(u) += deltaInfluence(u) * (1 - activationProbability((to, u))) //22 
                  GraphTraversal.VisitorReturn.Continue
                })

          val deltaAp = mutable.HashMap[Int, Double]().withDefaultValue(0.0)
          deltaAp += addedNode -> (1 - activationProbability((to, addedNode))) //24

          g.get(addedNode).traverse(direction = GraphTraversal.Successors, breadthFirst = true,
            edgeFilter = { e: g.EdgeT =>
              SubgraphVisitor.edgeFilter(ldagNodes(to) -- currentInitialSet,
                false)(e.toEdgeIn)
            })(
              edgeVisitor =
                e => {
                  logger.debug("2nd phase edge: " + e.toString)
                  deltaAp(e._2) += deltaAp(e._1) * e.weight / weightDenominator
                })
          logger.debug("DeltaAp")
          logger.debug(deltaAp.mkString(";"))
          g.get(addedNode).traverse(direction = GraphTraversal.Successors, breadthFirst = true,
            edgeFilter = { e: g.EdgeT =>
              SubgraphVisitor.edgeFilter(ldagNodes(to) -- currentInitialSet,
                false)(e.toEdgeIn)
            })(
              nodeVisitor =
                u => {
                  logger.debug("2nd phase node: " + u.toString)
                  activationProbability((to, u)) += deltaAp(u)
                  incrementalInfluence(u) -= currentInfluences((u, to)) * deltaAp(u)
                  GraphTraversal.VisitorReturn.Continue
                })
        }
    }
    currentInitialSet += addedNode
    incrementalInfluence -= addedNode
  }

}