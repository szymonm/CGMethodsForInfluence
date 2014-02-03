package pl.szymonmatejczyk.competetiveShapley

import pl.szymonmatejczyk.competetiveShapley.utils.GeneratorOverIterator

class BanzhaffIndexApproximator {
  def approximateAbsoluteBanzhaffIndex[C](valuation: C => Double,
    mkCoalition: () => Option[C]): Double = {
    mean(new GeneratorOverIterator(mkCoalition).map { x => valuation(x) })
  }

  /** TODO: move inside previous method, fix tests */
  def mean(it: Iterator[Double]): Double = {
    if (it.isEmpty)
      return 0.0

    var count = 0.0
    it.foldLeft(0.0)((x, y) => { count += 1; x + y }) / count
  }
}