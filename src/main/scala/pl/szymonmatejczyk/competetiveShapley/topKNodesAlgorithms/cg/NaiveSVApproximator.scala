package pl.szymonmatejczyk.competetiveShapley.topKNodesAlgorithms.cg

import collection.JavaConversions._
import pl.szymonmatejczyk.competetiveShapley.utils.GeneratorOverIterator
import java.util.concurrent.atomic.AtomicInteger

trait NaiveSVApproximator {
  def approximate[T1, T2](generator: () => Option[Seq[T1]], calculator: Seq[T1] => collection.Map[T2, Double],
      chunkSize : Int = 1): collection.Map[T2, Double] = {
    val res = new java.util.concurrent.ConcurrentHashMap[T2, Double]()
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