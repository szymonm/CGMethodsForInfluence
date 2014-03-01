package pl.szymonmatejczyk.competetiveShapley.topKNodesAlgorithms.cg

import scala.collection._
import collection.JavaConversions._
import pl.szymonmatejczyk.competetiveShapley.utils.GeneratorOverIterator
import java.util.concurrent.atomic.AtomicInteger

trait NaiveSVApproximator {
  def approximate[T](generator: () => Option[Seq[T]], calculator: Seq[T] => Map[T, Double],
      chunkSize : Int = 1): Map[T, Double] = {
    val res = new java.util.concurrent.ConcurrentHashMap[T, Double]()
    var counter = new AtomicInteger(0)
    new GeneratorOverIterator(generator).grouped(chunkSize).foreach {
      _.par.foreach {
        seq =>
          counter.incrementAndGet()
          calculator(seq).foreach {
            case (t, value) => res.update(t, res.getOrElse(t, 0.0) + value)
          }
      }
    }
    res.mapValues(_ / counter.get())
  }

}