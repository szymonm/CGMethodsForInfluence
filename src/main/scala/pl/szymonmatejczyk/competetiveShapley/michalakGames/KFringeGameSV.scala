package pl.szymonmatejczyk.competetiveShapley.michalakGames

import scala.collection._
import pl.szymonmatejczyk.competetiveShapley.graphs.WeightedDirectedNetwork
import scalax.collection.GraphPredef.graphParamsToPartition

trait KFringeGameSV {
  self : WeightedDirectedNetwork =>
  
  def computeSV(k : Int) : Map[Int, Double] = {
    g.nodes.toOuterNodes.map{x => (x, computeSingleSV(x, k))}.toMap
  }
  
  def degreeFactor(d : Int, k : Int) = math.max((1.0 + d - k) / (1.0 + d)*d, 0)
  
  def computeSingleSV(node : Int, k : Int) : Double = {
    val n = g.get(node) 
    n.diSuccessors.view.map{x => degreeFactor(x.inDegree, k)}.sum + math.min(1, (k / (1.0 + n.inDegree)))
  }
}