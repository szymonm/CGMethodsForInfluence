package pl.szymonmatejczyk.competetiveShapley.graphs

import scala.collection.immutable.TreeMap
import scala.util.Random

import pl.szymonmatejczyk.competetiveShapley.Valuation
import pl.szymonmatejczyk.competetiveShapley.common.takeFirstKValuesFromDeepTreeMap
import scalax.collection.GraphPredef.EdgeLikeIn
import scalax.collection.GraphPredef.graphParamsToPartition

trait MonteCarloAbsoluteBanzhaffIndexNetworkComputation[N, E[X] <: EdgeLikeIn[X]] {
  self : Network[N, E] =>
  
  def approximateAbsoluteBanzhafIndex(node: N,
    payoff: Valuation[N],
    iterations: Int = 1000): Double = {
    (0 until iterations).iterator.map {
      case i =>
        val coalition = graph.nodes.toOuterNodes.toSet.filter(_ => Random.nextBoolean())
        payoff(coalition + node) - payoff(coalition - node)
    }.sum / iterations
  }
  
  def chooseTopKByBIApprox(k: Int, payoff: Valuation[N], iterations: Int = 1000): Seq[N] = {
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
}