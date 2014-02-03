package pl.szymonmatejczyk.competetiveShapley.MichalakGames

import scala.collection._
import pl.szymonmatejczyk.competetiveShapley.InfluenceNetwork
import scalax.collection.GraphPredef.graphParamsToPartition
import pl.szymonmatejczyk.competetiveShapley.utils.PIMap

trait DistanceCutoffGameSV {
  self : InfluenceNetwork =>
    
  def floydWarshall() : PIMap[Int, Double] = {
    val dist = new PIMap[Int, Double](Double.MaxValue)
    g.nodes.foreach {n => dist += (((n.value, n.value), 0.0))}
    g.edges.foreach {e => dist += (((e._1.value, e._2.value), e.weight))}
    g.nodes.foreach{
      k => g.nodes.foreach{
        i => g.nodes.foreach {
          j => 
            if (dist(((i.value, j.value))) > dist(((i.value, k.value))) + dist(((k.value, j.value))))
              dist(((i.value, j.value))) = dist(((i.value, k.value))) + dist(((k.value, j.value)))
        }
      }
    }
    dist
  }
  
  def computeSV(k : Int, cutoff : Double) : Map[Int, Double] = {
    g.nodes.toOuterNodes.map{x => (x, computeSingleSV(x, k))}.toMap
  }
  
  def degreeFactor(d : Int) = 1.0  / (1.0 + d)
  
  def computeSingleSV(node : Int, cutoff : Double) : Double = {
    val m = floydWarshall()
    m.byFirstIterator(node).filter{_._2 < cutoff}.map{x => degreeFactor(x._1._2)}.sum
  }
}