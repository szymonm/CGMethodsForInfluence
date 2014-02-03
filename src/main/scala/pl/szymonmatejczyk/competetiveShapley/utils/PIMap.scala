package pl.szymonmatejczyk.competetiveShapley.utils
import scala.collection._
import scala.collection.mutable.HashSet

class PIMap[N, B](defaultValue: B) extends mutable.MapProxy[(N, N), B] {
  val self = mutable.HashMap.empty[(N, N), B].withDefaultValue(defaultValue)
  private val pairs = collection.mutable.HashMap[N, (HashSet[N], HashSet[N])]().
    withDefault(_ => (HashSet(), HashSet()))

  def from(kv: ((N, N), B)): N = kv._1._1
  def to(kv: ((N, N), B)): N = kv._1._2

  override def +=(kv: ((N, N), B)): this.type = {
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

  override def -=(k: (N, N)): this.type = {
    pairs(k._1)._2 -= k._2
    pairs(k._2)._1 -= k._1
    super.-=(k)
  }

  class PairIndexedMapIteratorBy(by: N, first: Boolean) extends Iterator[((N, N), B)] {
    val pairsIterator = if (first) pairs(by)._2.iterator else pairs(by)._1.iterator
    def hasNext: Boolean = pairsIterator.hasNext
    def next(): ((N, N), B) = {
      val pair = pairsIterator.next()
      if (first) {
        ((by, pair) -> self((by, pair)))
      } else {
        ((pair, by) -> self((pair, by)))
      }
    }
  }

  def all: Iterator[N] = {
    pairs.keysIterator
  }

  def byFirstIterator(first: N): PairIndexedMapIteratorBy = {
    new PairIndexedMapIteratorBy(first, true)
  }

  def bySecondIterator(second: N): PairIndexedMapIteratorBy = {
    new PairIndexedMapIteratorBy(second, false)
  }
}