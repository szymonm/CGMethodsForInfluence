package pl.szymonmatejczyk.competetiveShapley

import scala.collection.mutable.PriorityQueue
import scala.collection.immutable.SortedSet

package object common {
  def doubleToLong(x: Double, denominator: Long) = (x * denominator).toLong

  def longToDouble(l: Long, denominator: Double) = l.toDouble / denominator
  
  def topKFromMap[K](k : Int, map : Map[K, Double]) : Seq[K] = {
    (SortedSet[(K, Double)]()(Ordering.by[(K, Double), Double](-_._2)) ++ map).
      take(k).map(_._1).toSeq
  }
}