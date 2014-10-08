package pl.szymonmatejczyk.competetiveShapley.graphs

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scala.util.Random

import pl.szymonmatejczyk.competetiveShapley.Valuation

trait MonteCarloShapleyValueNetworkComputation[N, E[X] <: EdgeLikeIn[X]] {
  self : Network[N, E] => 
    
  def approximateShapleyValue(node: N,
    payoff: Valuation[N],
    iterations: Int = 1000): Double = {
    (0 until iterations).iterator.map {
      case i =>
        val permutation = Random.shuffle(graph.nodes.toOuterNodes)
        val coalition = permutation.takeWhile(_ != node).toSet
        payoff(coalition + node) - payoff(coalition)
    }.sum / iterations
  }
}