package pl.szymonmatejczyk.competetiveShapley

import collection._
import scala.collection.immutable.TreeMap

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
  
  def takeFirstKValuesFromDeepTreeMap[K](k: Int, treeMap: TreeMap[_, Seq[K]]): Seq[K] = {
    val result = Seq.newBuilder[K]
    val it = treeMap.iterator
    var counter = 0
    while (counter < k) {
      it.next() match {
        case (key, list) =>
          val len = list.length
          result ++= (list.take(k - counter))
          counter += len
      }
    }
    result.result()
  }
}