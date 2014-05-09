package pl.szymonmatejczyk.competetiveShapley.graphs
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scala.util.Random
import scalax.collection.GraphTraversal.VisitorReturn._
import scala.collection.immutable.TreeMap
import pl.szymonmatejczyk.competetiveShapley.utils.Cache

class Network[N, E[X] <: EdgeLikeIn[X]](val graph: Graph[N, E], 
                                        val weightDenominatorOpt : Option[Double]) 
    extends Cache {
  type Valuation = Set[N] => Double

  def clearCache() {}

  def approximateShapleyValue(node: N,
    payoff: Valuation,
    iterations: Int = 1000): Double = {
    (0 until iterations).iterator.map {
      case i =>
        val permutation = Random.shuffle(graph.nodes.toOuterNodes)
        val coalition = permutation.takeWhile(_ != node).toSet
        payoff(coalition + node) - payoff(coalition)
    }.sum / iterations
  }

  def approximateAbsoluteBanzhafIndex(node: N,
    payoff: Valuation,
    iterations: Int = 1000): Double = {
    (0 until iterations).iterator.map {
      case i =>
        val coalition = graph.nodes.toOuterNodes.toSet.filter(_ => Random.nextBoolean())
        payoff(coalition + node) - payoff(coalition - node)
    }.sum / iterations
  }

  def computeBanzhafForSimpleInfectionPropagation(node: N): Double = {
    val singleOnSingleInfluence = Array.fill(graph.order, graph.order)(0.0)
    val currentValues = Array.fill(graph.order)(0.0)

    val nodesToIntMap = graph.nodes.toSeq.zip(0 until graph.order).toMap

    def addN(node: graph.NodeT) = {
      //      singleOnSingleInfluece
      Continue
    }
    def addE(edge: graph.EdgeT) = {
      val node1num = nodesToIntMap(edge._1)
      val node2num = nodesToIntMap(edge._2)
      currentValues(node2num) += node1num * edge.weight
    }
    val traversal = graph.newTraversal(nodeVisitor = addN, edgeVisitor = addE)
    traversal(graph get node, breadthFirst = true)
    0.0
  }

  def chooseTopKByBIApprox(k: Int, payoff: Valuation, iterations: Int = 1000): Seq[N] = {
    if (k >= graph.nodes.size)
      return Seq() ++ graph.nodes.map { _.value }
    var treeMap = new TreeMap[Double, List[N]]()(Ordering[Double].reverse)
    graph.nodes.foreach {
      case node =>
        val value = approximateAbsoluteBanzhafIndex(node.value, payoff, iterations)
        val list = treeMap.getOrElse(value, List())
        treeMap = treeMap + (value -> (node.value :: list))
    }
    // taking k first N values form the map
    takeFirstKValuesFromDeepTreeMap[N](k, treeMap)
  }

  def takeFirstKValuesFromDeepTreeMap[K](k: Int, treeMap: TreeMap[_, Seq[K]]): Seq[K] = {
    val result = Seq.newBuilder[K]
    val it = treeMap.iterator
    var counter = 0
    while (counter < k) {
      it.next() match {
        case (key, list) =>
          val len = list.length
          result ++= (list.take(k - counter))
          counter += len
      }
    }
    result.result()
  }
}

