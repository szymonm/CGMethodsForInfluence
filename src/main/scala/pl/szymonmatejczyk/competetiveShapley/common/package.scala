package pl.szymonmatejczyk.competetiveShapley

import collection._
import scala.collection.mutable.PriorityQueue
import scala.collection.immutable.SortedSet
import pl.szymonmatejczyk.competetiveShapley.utils.TestingUtils

package object common {
  def doubleToLong(x: Double, denominator: Long) = (x * denominator).toLong

  def longToDouble(l: Long, denominator: Double) = l.toDouble / denominator
  
  def topKFromMap[K](k : Int, map : Map[K, Double]) : Seq[K] = {
    (SortedSet[(K, Double)]()(Ordering.by[(K, Double), Double](-_._2)) ++ map).
      take(k).map(_._1).toSeq
  }

  def topKsFromMap[K](ks: Seq[Int], map: Map[K, Double]): Seq[Seq[K]] = {
    ks.map {
      k => topKFromMap[K](k, map)
    }
  }
}