package pl.szymonmatejczyk.competetiveShapley.michalakGames

import collection._
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge.Implicits._

import pl.szymonmatejczyk.competetiveShapley.graphs.WeightedDirectedNetwork
import pl.szymonmatejczyk.competetiveShapley.utils.math.Erf._
import pl.szymonmatejczyk.competetiveShapley._
import pl.szymonmatejczyk.competetiveShapley.common._

trait InfluenceAboveThresholdGameSV {
  self : WeightedDirectedNetwork => 
    
   type M = Function1[Int, Double]
    
  /**
   * For each v, sum of weights of incoming edges.
   */
  def alphas(f : (Double => Double) = identity[Double]) : M = {
    g.nodes.map{
      x => (x.value, x.incoming.map(e => f(e.weight / weightDenominator)).sum)
    }.toMap  
  }
  
  def betas() : M = alphas(x => x * x)
  
  def computeSingleSV(node : Int, cutoff : M, alpha : M, beta : M) : Double = {
    var res = 0.0
    val inDegree = g.get(node).inDegree
    0.until(inDegree).foreach{
      m => 
        val mi = m.toDouble / (inDegree) * alpha(node)
        val sigma = m.toDouble * (inDegree - m) / (inDegree * (inDegree - 1)) * 
          (beta(node) - alpha(node)/inDegree)
        val p = normalCDF(cutoff(node), mi, sigma)
        res += p / (1.0 + inDegree)
    }
    g.get(node).inNeighbors.foreach {
      neighbour =>
        var p = 0.0
        0.to(neighbour.inDegree).foreach{
          m =>
            val wij = g.get(node).findOutgoingTo(neighbour).fold(0.0)(_.weight)
            val mi = m / (neighbour.inDegree - 1) * (alpha(neighbour) - wij)
            val sigma = m * (neighbour.inDegree - 1 - m) / ((neighbour.inDegree - 1) * 
                (neighbour.inDegree - 2)) * (beta(neighbour) - wij * wij) -
                (alpha(neighbour) - wij * wij) / (neighbour.inDegree - 1)
            val z = 0.5 * (erf((cutoff(neighbour) - mi)/ (math.sqrt(2) * sigma)) - 
                erf((cutoff(neighbour) - wij - m) / math.sqrt(2) * sigma))
            p += z * (neighbour.inDegree - m) / (neighbour.inDegree * (neighbour.inDegree + 1))
        }
        res += p        
    }
    res
  }
  
  def computeInfluenceAboveThresholdGameSV(cutoff : Function1[Int, Double]) : Map[Int, Double] = {
    val alpha = alphas()
    val beta = betas()
    
    g.nodes.toOuterNodes.par.map{x => (x, computeSingleSV(x, cutoff, alpha, beta))}.toMap.seq
  }
}

object InfluenceAboveThresholdGameSV {
  def influenceHeuristic(cutoff : Double) = new InfluenceHeuristic("influenceAboveThreshold", 
      (in : IN) => (k : Int) => topKFromMap[Int](k, 
          in.computeInfluenceAboveThresholdGameSV(_ => cutoff)))
}