package pl.szymonmatejczyk.competetiveShapley.algorithms.cg

import pl.szymonmatejczyk.competetiveShapley.utils.GeneratorOverIterator
import pl.szymonmatejczyk.competetiveShapley.utils.GeneratorToStream
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import scala.concurrent.Await
import scala.concurrent.duration.Duration

class BanzhaffIndexApproximator {
  def approximateAbsoluteBanzhaffIndex[C](valuation: C => Double,
    mkCoalition: () => Option[C])(implicit executionContext: ExecutionContext): Double = {
    val futures = GeneratorToStream(mkCoalition).map { x => Future {valuation(x)} }
    mean(Await.result(Future.sequence(futures), Duration.Inf))
  }

  def mean(it: Seq[Double]): Double = {
    if (it.isEmpty)
      return 0.0

    var count = 0.0
    it.foldLeft(0.0)((x, y) => { count += 1; x + y }) / count
  }
}