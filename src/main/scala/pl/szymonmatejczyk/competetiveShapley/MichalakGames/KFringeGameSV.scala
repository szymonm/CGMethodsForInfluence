package pl.szymonmatejczyk.competetiveShapley.MichalakGames

import scala.collection._
import pl.szymonmatejczyk.competetiveShapley.WeightedDirectedNetwork
import scalax.collection.GraphPredef.graphParamsToPartition

trait KFringeGameSV {
  self : WeightedDirectedNetwork =>
  
  def computeSV(k : Int) : Map[Int, Double] = {
    g.nodes.toOuterNodes.map{x => (x, computeSingleSV(x, k))}.toMap
  }
  
  def degreeFactor(d : Int, k : Int) = (1.0 + d + k) / (1.0 + d)*d
  
  def computeSingleSV(node : Int, k : Int) : Double = {
    val n = g.get(node) 
    n.diSuccessors.view.map{x => degreeFactor(x.inDegree, k)}.sum + degreeFactor(n.inDegree, k)
  }
}