package pl.szymonmatejczyk.competetiveShapley

import scala.collection._
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge.Implicits._
import scalax.collection.edge.WDiEdge
import scala.util.Random
import scalax.collection.config.CoreConfig
import scala.collection.mutable.HashMap
import scalax.collection.GraphTraversal
import scala.collection.immutable.Queue
import com.typesafe.scalalogging.slf4j.Logging
import pl.szymonmatejczyk.competetiveShapley.utils.Cache

trait LDAGApproximation extends Cache with Logging {
  val g : Graph[Int, WDiEdge]
  val weightDenominator : Double
  val r : Random
  var threshold : Double
  
  final val MAX_LDAG_SIZE = 15
  
  // ldag target node -> node -> priority in ldag
  var ldagNodes : mutable.Map[Int, immutable.Map[Int, Int]] = 
    mutable.Map[Int, immutable.Map[Int, Int]]()

  abstract override def clearCache() {
    logger.debug("Clearing ldags")
    ldagNodes = mutable.Map[Int, immutable.Map[Int, Int]]()
    super.clearCache()
  }
  
  /** Returns map of nodes in dag with priorities and influences */
  def computeLDAG(forNode: Int, withInfluences : Boolean = false) :
	  (immutable.Map[Int, Int], Option[Map[Int, Double]]) = {
    if (!withInfluences && ldagNodes.contains(forNode))
      return ((ldagNodes(forNode), None))

      // todo: fix using fibonacci heap
    val ldag = scalax.collection.mutable.Graph[Int, WDiEdge]()
    val ldagNodesForCurrent = mutable.Map[Int, Int]() // (nodeNumber -> ordinalNumber where forNode -> 0
    val node = g.find(forNode) getOrElse (throw new IllegalArgumentException("Node not found."))

    val influenceOf = HashMap[Int, Double]().withDefault(_ => 0.0);
    influenceOf += forNode -> 1.0
    val priorityQueue = collection.mutable.TreeSet[g.NodeT]()(Ordering[(Double, Int)].on(
        x => (-influenceOf(x.value), x.value)))
    priorityQueue += node
    
    var nodes_no = 0

    while (priorityQueue.nonEmpty && influenceOf(priorityQueue.head.value) > threshold &&
           nodes_no < MAX_LDAG_SIZE) {
      val cur = priorityQueue.head
      priorityQueue -= cur
      val curInfluence = influenceOf(cur.value)

      ldag += cur.value
      ldagNodesForCurrent += cur.value -> (nodes_no + 1)
      nodes_no += 1
      ldag ++= (cur.outgoing.filter { x => (ldag.find(x._2.value).nonEmpty) }).map {
        e => (e._1.value ~> e._2.value % e.weight)
      }

      cur.incoming.foreach {
        e =>
          priorityQueue -= e._1
          influenceOf += e._1.value -> (influenceOf(e._1.value) + curInfluence *
            e.weight / weightDenominator)
          if (ldag.nodes.find(e._1.value).isEmpty) {
            priorityQueue += e._1
          }
      }
    }
    ldagNodes += forNode -> (immutable.Map() ++ ldagNodesForCurrent)
    ((ldagNodes(forNode), if (withInfluences) Some(immutable.Map() ++ influenceOf) else None))
  }
  
  def computeAllLDAGs() {
    g.nodes.foreach {
      node => computeLDAG(node)
    }
  }
  
    /**
   * Computes predecessors in LDAG(ldagFor) using ldagNodes field.
   */
  def computeLDAGPredecessors(ldagFor : Int, node : Int) : Set[Int] = {
    computeLDAG(ldagFor)
    logger.debug("computing ldag pred")
    val ldag = ldagNodes.getOrElse(ldagFor, throw new IllegalStateException("ldags not computed"))
    val predecessors = mutable.Set[Int]()
    g.get(node).traverse(direction = GraphTraversal.Predecessors, breadthFirst = true, 
              edgeFilter = {e : g.EdgeT => SubgraphVisitor.edgeFilter(ldag, true, true)(e.toEdgeIn)})(
              nodeVisitor =
                u => {
                  predecessors += u.value
                  GraphTraversal.VisitorReturn.Continue
                })
                
    logger.debug("computing ldag pred - finished")
    predecessors - node
  }
  
  /**
   * Computes successors in LDAG(ldagFor) using ldagNodes field.
   */
  def computeLDAGSuccessors(ldagFor : Int, node: Int) : Set[Int] = {
    computeLDAG(ldagFor)
    logger.debug("computing ldag succ")
    val ldag = ldagNodes.getOrElse(ldagFor, throw new IllegalStateException("ldags not computed"))
    val successors = mutable.Set[Int]()
    g.get(node).traverse(direction = GraphTraversal.Successors, breadthFirst = true, 
              edgeFilter = {e : g.EdgeT => SubgraphVisitor.edgeFilter(ldag, true, true)(e.toEdgeIn)})(
              nodeVisitor =
                u => {
                  successors += u.value
                  GraphTraversal.VisitorReturn.Continue
                })
                
    logger.debug("computing ldag succ - finished")
    successors - node
  }
  
  def ldagPredecessorsTraverse(ldagHead : Int, traverseStart : Int)(nodeVisitor : 
      (g.NodeT, Double) => Boolean) {
    computeLDAG(ldagHead)
    // todo: mozna to zrobic lepiej - wchodzac do wierzcholka tylko raz
    var toVisit = immutable.TreeMap[g.NodeT, Double]()(Ordering[Int].on(x => ldagNodes(ldagHead)(x.value)))
    if (ldagNodes(ldagHead).contains(traverseStart))
      toVisit = toVisit + ((g.get(traverseStart), 1.0))
    while (!toVisit.isEmpty) {
      val (cur, weight) = toVisit.head
      toVisit = toVisit - cur
      val curLDAGPosition : Integer = ldagNodes(ldagHead)(cur.value)
      if (nodeVisitor(cur, weight)) {
        cur.incoming.foreach {
          edge => if (ldagNodes(ldagHead).getOrElse(edge.from.value, 0) > curLDAGPosition) {
            val oldWeight = toVisit getOrElse(edge.from, 0.0)
            toVisit = toVisit + ((edge.from, oldWeight + weight * (edge.weight / weightDenominator)))
          }
        }
      }
    }
  }
  
  def ldagSize(ldagHead : Int) : Int = {
    computeLDAG(ldagHead)
    ldagNodes(ldagHead).keys.size
  }
}