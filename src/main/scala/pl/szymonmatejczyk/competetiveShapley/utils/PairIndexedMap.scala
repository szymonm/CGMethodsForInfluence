package pl.szymonmatejczyk.competetiveShapley

import scala.collection.mutable.HashSet
import scala.collection.generic.MutableMapFactory
import scala.collection.mutable.HashMap

/**
 * Failed to implement it this way. Use PIMap instead.
 */
@deprecated("Use PIMap instead.", "")
trait PairIndexedMap[B] extends collection.mutable.Map[(Int, Int), B] {
  private val pairs = collection.mutable.HashMap[Int, (HashSet[Int], HashSet[Int])]().
    withDefault(_ => (HashSet(), HashSet()))

  //  val self = HashMap.empty[(Int, Int), B]
  //  override def empty : PairIndexedMap[B] = PairIndexedMap.empty

  def from(kv: ((Int, Int), B)): Int = kv._1._1
  def to(kv: ((Int, Int), B)): Int = kv._1._2

  override def withDefault(d: ((Int, Int)) => B): PairIndexedMap[B] =
    new PairIndexedMap.WithDefault[B](this, d)
  override def withDefaultValue(d: B): PairIndexedMap[B] =
    new PairIndexedMap.WithDefault[B](this, x => d)

  abstract override def +=(kv: ((Int, Int), B)): PairIndexedMap.this.type = {
    if (pairs.contains(from(kv))) {
      pairs(from(kv))._2 += to(kv)
    } else {
      pairs += from(kv) -> ((HashSet(), HashSet(to(kv))))
    }
    if (pairs.contains(to(kv))) {
      pairs(to(kv))._1 += from(kv)
    } else {
      pairs += to(kv) -> ((HashSet(from(kv)), HashSet()))
    }
    super.+=(kv)
  }

  abstract override def -=(k: (Int, Int)): PairIndexedMap.this.type = {
    pairs(k._1)._2 -= k._2
    pairs(k._2)._1 -= k._1
    super.-=(k)
  }

  class PairIndexedMapIteratorBy(by: Int, first: Boolean) extends Iterator[((Int, Int), B)] {
    val pairsIterator = if (first) pairs(by)._2.iterator else pairs(by)._1.iterator
    def hasNext: Boolean = pairsIterator.hasNext
    def next(): ((Int, Int), B) = {
      val pair = pairsIterator.next()
      if (first) {
        ((by, pair) -> PairIndexedMap.this((by, pair)))
      } else {
        ((pair, by) -> PairIndexedMap.this((pair, by)))
      }
    }
  }

  def byFirstIterator(first: Int): PairIndexedMapIteratorBy = {
    new PairIndexedMapIteratorBy(first, true)
  }

  def bySecondIterator(second: Int): PairIndexedMapIteratorBy = {
    new PairIndexedMapIteratorBy(second, false)
  }
}

//abstract class PairIndexedMapFactory[CC[B] <: PairIndexedMap[B]] extends MutableMapFactory[CC] {
//}
//
//class PairIndexedHashMap[B] extends HashMap[(Int, Int), B] with PairIndexedMap[B] {
//  override def empty : PairIndexedHashMap[B] = PairIndexedHashMap.empty[B]
//}
//object PairIndexedHashMap {
//  def empty[B]: PairIndexedHashMap[B] = new PairIndexedHashMap[B]
//}

object PairIndexedMap {
  //  def empty[B] : PairIndexedMap[B] = new PairIndexedHashMap[B]

  class WithDefault[B](underlying: PairIndexedMap[B], d: ((Int, Int)) => B) extends scala.collection.mutable.Map.WithDefault[(Int, Int), B](underlying, d) with PairIndexedMap[B] {
    //    override def empty = new WithDefault(underlying.empty, d)

    override def withDefault(d: ((Int, Int)) => B): PairIndexedMap[B] = new WithDefault[B](underlying, d)
    override def withDefaultValue(d: B): PairIndexedMap[B] = new WithDefault[B](underlying, x => d)
  }
}