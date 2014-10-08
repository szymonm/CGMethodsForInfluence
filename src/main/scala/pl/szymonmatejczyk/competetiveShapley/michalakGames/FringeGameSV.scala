package pl.szymonmatejczyk.competetiveShapley.michalakGames

import scala.collection._
import scalax.collection.GraphPredef.graphParamsToPartition
import pl.szymonmatejczyk.competetiveShapley.graphs.WeightedDirectedNetwork
import pl.szymonmatejczyk.competetiveShapley._
import pl.szymonmatejczyk.competetiveShapley.common._
import pl.szymonmatejczyk.competetiveShapley.utils.TestingUtils
import scala.concurrent.duration.Duration

trait FringeGameSV {
  self : WeightedDirectedNetwork =>
  
  def computeFringeGameSV() : Map[Int, Double] = {
    graph.nodes.toOuterNodes.par.map{x => (x, computeSingleSV(x))}.toMap.seq
  }
  
  def degreeFactor(d : Int) = 1.0 / (1.0 + d)
  
  def computeSingleSV(node : Int) : Double = {
    val n = graph.get(node)
    n.diSuccessors.view.map{x => degreeFactor(x.inDegree)}.sum + degreeFactor(n.inDegree)
  }
}

object FringeGameSV {
  def NAME = "fringeGame"
  def influenceHeuristic : InfluenceHeuristic = new InfluenceHeuristic(NAME, (in : IN) => 
    (k : Int) => topKFromMap[Int](k, in.computeFringeGameSV()))
  
  def influenceHeuristicForSequenceOfK: InfluenceHeuristicForSequenceOfK = {
    def influence(in: InfluenceNetwork)(ks: Seq[Int]): Seq[(Seq[Int], Duration)] = {
      val (rank, rtime) = TestingUtils.time(in.computeFringeGameSV())
      topKsFromMap(ks, rank).map(x => (x, rtime))
    }
    (NAME, (influence _))
  }
}