package pl.szymonmatejczyk.competetiveShapley

import scala.collection._
import pl.szymonmatejczyk.competetiveShapley.coalitionGeneration.CoalitionGenerator
import scalax.collection.GraphTraversal

trait InfluenceNetworkLDAGApproximation {
  self: InfluenceNetwork =>

    /**
   * Computes influences approximated in LDAG from computeLDAG method and
   * nodes in the LDAG. (Algorithm 4)
   */
  def computeApproxInfluencesAndLDAGNodes(onNode: Int): (immutable.Map[Int, Double], immutable.Map[Int, Int]) = {
    val res = mutable.HashMap[Int, Double]().withDefault(_ => 0.0)
    res += onNode -> 1.0
    val (ldagNodes, _) = computeLDAG(onNode)

    import scalax.collection.GraphTraversal.VisitorReturn._
    def propagateInfluenceV(node: g.NodeT) = {
      Continue
    }
    def propagateInfluenceE(edge: g.EdgeT) = {
      res += (edge._1.value -> (res(edge._1.value) + edge.weight / weightDenominator *
        res(edge._2.value)))
      Continue
    }

    g.get(onNode).traverse(direction = GraphTraversal.Predecessors,
      breadthFirst = true,
      edgeFilter = { e: g.EdgeT =>
        SubgraphVisitor.edgeFilter(ldagNodes,
          true, true)(e.toEdgeIn)
      })(
        nodeVisitor = propagateInfluenceV,
        edgeVisitor = propagateInfluenceE)

    ((collection.immutable.HashMap[Int, Double]() ++ res, ldagNodes))
  }
    
  def computeInfluenceInLDAG(ldagHeadNode: Int, onNode: Int, from: Set[Int],
    blockedNodes: Set[Int] = Set()): Double = {
    var influence = 0.0
    ldagPredecessorsTraverse(ldagHeadNode, onNode) {
      case (node, pathValue) => if (from.contains(node.value)) {
        influence += pathValue
        false
      } else {
        !blockedNodes.contains(node.value)
      }
    }
    influence
  }
  

}