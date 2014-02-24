package pl.szymonmatejczyk.competetiveShapley.michalakGames

import scala.collection._
import scalax.collection.GraphPredef.graphParamsToPartition

import pl.szymonmatejczyk.competetiveShapley.graphs.WeightedDirectedNetwork
import pl.szymonmatejczyk.competetiveShapley._
import pl.szymonmatejczyk.competetiveShapley.common._

trait FringeGameSV {
  self : WeightedDirectedNetwork =>
  
  def computeFringeGameSV() : Map[Int, Double] = {
    g.nodes.toOuterNodes.par.map{x => (x, computeSingleSV(x))}.toMap.seq
  }
  
  def degreeFactor(d : Int) = 1.0 / (1.0 + d)
  
  def computeSingleSV(node : Int) : Double = {
    val n = g.get(node)
    n.diSuccessors.view.map{x => degreeFactor(x.inDegree)}.sum + degreeFactor(n.inDegree)
  }
}

object FringeGameSV {
  def influenceHeuristic : InfluenceHeuristic = new InfluenceHeuristic("fringeGame", (in : IN) => 
    (k : Int) => topKFromMap[Int](k, in.computeFringeGameSV()))
}