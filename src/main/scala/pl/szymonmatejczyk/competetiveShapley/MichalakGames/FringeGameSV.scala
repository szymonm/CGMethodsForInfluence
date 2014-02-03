package pl.szymonmatejczyk.competetiveShapley.MichalakGames

import scala.collection._
import pl.szymonmatejczyk.competetiveShapley.InfluenceNetwork
import scalax.collection.GraphPredef.graphParamsToPartition

trait FringeGameSV {
  self : InfluenceNetwork =>
  
  def computeSV() : Map[Int, Double] = {
    g.nodes.toOuterNodes.map{x => (x, computeSingleSV(x))}.toMap
  }
  
  def degreeFactor(d : Int) = 1.0 / (1.0 + d)
  
  def computeSingleSV(node : Int) : Double = {
    g.get(node).diSuccessors.view.map{x => degreeFactor(x.outDegree)}.sum
  }
}