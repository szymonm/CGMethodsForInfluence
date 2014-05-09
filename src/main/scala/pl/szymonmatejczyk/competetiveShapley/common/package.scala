package pl.szymonmatejczyk.competetiveShapley

import collection._

package object common {
  def doubleToLong(x: Double, denominator: Long) = (x * denominator).toLong

  def longToDouble(l: Long, denominator: Double) = l.toDouble / denominator
  
  def rankFromMap[K](map : Map[K, Double]) : Seq[K] = {
    map.toSeq.sortBy(-_._2).map(_._1)
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