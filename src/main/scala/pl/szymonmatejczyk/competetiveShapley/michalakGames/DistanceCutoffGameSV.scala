package pl.szymonmatejczyk.competetiveShapley.michalakGames

import scala.collection._
import pl.szymonmatejczyk.competetiveShapley.graphs.WeightedDirectedNetwork
import scalax.collection.GraphPredef.graphParamsToPartition
import pl.szymonmatejczyk.competetiveShapley.utils.PIMap
import pl.szymonmatejczyk.competetiveShapley._
import pl.szymonmatejczyk.competetiveShapley.common._
import pl.szymonmatejczyk.competetiveShapley.utils.TestingUtils
import scala.concurrent.duration.Duration

trait DistanceCutoffGameSV {
  self : WeightedDirectedNetwork =>
    
  def floydWarshall() : PIMap[Int, Double] = {
    val distMap = new PIMap[Int, Double](Double.MaxValue)
    def dist(i : Int, j : Int) = distMap(((i, j)))
    
    g.nodes.foreach {n => distMap += (((n.value, n.value), 0.0))}
    g.edges.foreach {e => distMap += (((e._1.value, e._2.value), e.weight / weightDenominator))}
    g.nodes.foreach{
      k => g.nodes.foreach{
        i => g.nodes.foreach {
          j => 
            if (dist(i.value, j.value) > dist(i.value, k.value) + dist(k.value, j.value))
              distMap(((i.value, j.value))) = dist(i.value, k.value) + dist(k.value, j.value)
        }
      }
    }
    distMap
  }
  
  def computeDistanceCutoffSV(cutoff : Double) : Map[Int, Double] = {
    val distances = floydWarshall()
    g.nodes.toOuterNodes.map{x => 
      (x, computeSingleSV(x, cutoff, distances, extInDegreeMap(distances, cutoff)))}.toMap
  }
  
  def degreeFactor(d : Double) = 1.0  / (1.0 + d)
  
  def extInDegreeMap(distMap : PIMap[Int, Double], cutoff : Double) : Map[Int, Double] = {
    var res = mutable.Map[Int, Double]()
    g.nodes.foreach{
      node => 
        res(node.value) = distMap.bySecondIterator(node.value).filter(_._2 <= cutoff).size - 1
    }
    res
  }
  
  def computeSingleSV(node : Int, cutoff : Double, distances : PIMap[Int, Double], 
                      extInDegreeMap : Map[Int, Double]) : Double = {
    distances.byFirstIterator(node).filter{_._2 <= cutoff}.map{
      x => degreeFactor(extInDegreeMap(x._1._2))}.sum
  }
}

object DistanceCutoffGameSV {
  val NAME = "distanceCutoff"
    
  def influenceHeuristic(cutoff : Double) : InfluenceHeuristic = new InfluenceHeuristic(NAME, 
      (in : IN) => (k : Int) => topKFromMap[Int](k, in.computeDistanceCutoffSV(cutoff)))
  
  def influenceHeuristicForSequenceOfK(cutoff : Double): InfluenceHeuristicForSequenceOfK = {
    def influence(in: InfluenceNetwork)(ks: Seq[Int]): Seq[(Seq[Int], Duration)] = {
      val (rank, rtime) = TestingUtils.time(in.computeDistanceCutoffSV(cutoff))
      topKsFromMap(ks, rank).map(x => (x, rtime))
    }
    (NAME, (influence _))
  }
}