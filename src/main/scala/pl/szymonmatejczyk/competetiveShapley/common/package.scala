package pl.szymonmatejczyk.competetiveShapley

import collection._
import scala.collection.mutable.PriorityQueue
import scala.collection.immutable.SortedSet
import pl.szymonmatejczyk.competetiveShapley.utils.TestingUtils

package object common {
  def doubleToLong(x: Double, denominator: Long) = (x * denominator).toLong

  def longToDouble(l: Long, denominator: Double) = l.toDouble / denominator
  
  def rankFromMap[K](map : Map[K, Double]) : Seq[K] = {
    map.toSeq.sortBy(-_._2).map(_._1)
//    (SortedSet[(K, Double)]()(Ordering.by[(K, Double), Double](-_._2)) ++ map).map(_._1).toSeq
  }
  
  def topKFromMap[K](k : Int, map : Map[K, Double]) : Seq[K] = {
    rankFromMap(map).take(k)
  }
  
  def topKsFromRank[K](ks : Seq[Int], rank : Seq[K]) : Seq[Seq[K]] = {
    ks.map{
      k => rank.take(k)
    }
  }

  def topKsFromMap[K](ks: Seq[Int], map: Map[K, Double]): Seq[Seq[K]] = {
    topKsFromRank(ks, rankFromMap(map))
  }
}