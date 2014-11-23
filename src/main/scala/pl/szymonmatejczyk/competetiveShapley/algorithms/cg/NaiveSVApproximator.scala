package pl.szymonmatejczyk.competetiveShapley.algorithms.cg

import collection.JavaConversions._
import pl.szymonmatejczyk.competetiveShapley.utils.GeneratorOverIterator
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import scala.concurrent.Await
import scala.concurrent.duration.Duration

trait NaiveSVApproximator {
  def approximate[T1, T2](generator: () => Option[Seq[T1]], calculator: Seq[T1] => collection.Map[T2, Double])
    (implicit executionContext: ExecutionContext): collection.Map[T2, Double] = {
    val res = new java.util.concurrent.ConcurrentHashMap[T2, Double]()
    var counter = new AtomicInteger(0)
    val futures = new GeneratorOverIterator(generator).map {
      seq =>
        Future {
          counter.incrementAndGet()
          calculator(seq).foreach {
            case (t, value) => res.update(t, res.getOrElse(t, 0.0) + value)
          }
        }
    }
    Await.result(Future.sequence(futures), Duration.Inf)
    res.mapValues(_ / counter.get())
  }
}