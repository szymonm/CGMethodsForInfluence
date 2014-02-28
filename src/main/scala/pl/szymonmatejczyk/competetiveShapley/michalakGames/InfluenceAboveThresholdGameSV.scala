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
import pl.szymonmatejczyk.competetiveShapley.utils.TestingUtils
import scala.concurrent.duration.Duration

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

  def pii(m: Int, inD: Int, node: Int, alpha: M, beta: M,
    cutoff: M) = {
    if (inD == 0)
      1.0
   else if (inD == 1)
      0.5 + 0.5 * (1 - alpha(node.value)) // 1/2 + 1/2 * (1 - wij)  
   else {
      val mi = m.toDouble / (inD) * alpha(node)
      val sigma = m.toDouble * (inD - m) / (inD * (inD - 1)) *
        (beta(node) - alpha(node) / inD)
      normalCDF(cutoff(node), mi, sigma)
    }
  }
  
  def computeSingleSV(node : Int, cutoff : M, alpha : M, beta : M) : Double = {
    var res = 0.0
    val inDegree = g.get(node).inDegree
    0.until(inDegree).foreach{
      m => res += pii(m, inDegree, node, alpha, beta, cutoff)/ (1.0 + inDegree)
    }
    g.get(node).inNeighbors.foreach {
      neighbour =>
        var p = 0.0
        0.to(neighbour.inDegree).foreach{
          m =>
            if (neighbour.inDegree <= 2) {
              p += 1 / 2 * g.get(node).findOutgoingTo(neighbour).fold(0.0)(_.weight) // 1/2 * wij
            } else {
              val wij = g.get(node).findOutgoingTo(neighbour).fold(0.0)(_.weight)
              val mi = m / (neighbour.inDegree - 1) * (alpha(neighbour) - wij)
              val sigma = m * (neighbour.inDegree - 1 - m) / ((neighbour.inDegree - 1) *
                (neighbour.inDegree - 2)) * (beta(neighbour) - wij * wij) -
                (alpha(neighbour) - wij * wij) / (neighbour.inDegree - 1)
              val z = 0.5 * (erf((cutoff(neighbour) - mi) / (math.sqrt(2) * sigma)) -
                erf((cutoff(neighbour) - wij - m) / math.sqrt(2) * sigma))
              p += z * (neighbour.inDegree - m) / (neighbour.inDegree * (neighbour.inDegree + 1))
            }
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
  val NAME = "influenceAbThreshold"
  def influenceHeuristic(cutoff : Double) = new InfluenceHeuristic(NAME , 
      (in : IN) => (k : Int) => topKFromMap[Int](k, 
          in.computeInfluenceAboveThresholdGameSV(_ => cutoff)))
  
  def influenceHeuristicForSequenceOfK(cutoff : Double): InfluenceHeuristicForSequenceOfK = {
    def influence(in: InfluenceNetwork)(ks: Seq[Int]): Seq[(Seq[Int], Duration)] = {
      val (rank, rtime) = TestingUtils.time(in.computeInfluenceAboveThresholdGameSV(_ => cutoff))
      topKsFromMap(ks, rank).map(x => (x, rtime))
    }
    (NAME, (influence _))
  }
}