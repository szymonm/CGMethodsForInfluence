package pl.szymonmatejczyk.competetiveShapley.michalakGames

import scala.collection._
import scalax.collection.GraphPredef.graphParamsToPartition

import pl.szymonmatejczyk.competetiveShapley.graphs.WeightedDirectedNetwork
import pl.szymonmatejczyk.competetiveShapley._
import pl.szymonmatejczyk.competetiveShapley.common._

trait KFringeGameSV {
  self : WeightedDirectedNetwork =>
  
  def computeKFringeSV(k : Int) : Map[Int, Double] = {
    g.nodes.toOuterNodes.map{x => (x, computeSingleSV(x, k))}.toMap
  }
  
  def degreeFactor(d : Int, k : Int) = math.max((1.0 + d - k) / (1.0 + d)*d, 0)
  
  def computeSingleSV(node : Int, k : Int) : Double = {
    val n = g.get(node) 
    n.diSuccessors.view.map{x => degreeFactor(x.inDegree, k)}.sum + math.min(1, (k / (1.0 + n.inDegree)))
  }
}

object KFringeGameSV {
    def influenceHeuristic(fringeK : Int) : InfluenceHeuristic = new InfluenceHeuristic("KFringeGame", 
      (in : IN) => (k : Int) => topKFromMap[Int](k, 
          in.computeKFringeSV(fringeK)))
}