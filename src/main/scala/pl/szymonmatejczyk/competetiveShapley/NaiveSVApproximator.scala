package pl.szymonmatejczyk.competetiveShapley

import scala.collection._
import pl.szymonmatejczyk.competetiveShapley.utils.GeneratorOverIterator

trait NaiveSVApproximator {
  def approximate[T](generator : () => Option[Seq[T]], calculator : Seq[T] => Map[T, Double]) :
      Map[T, Double] = {
    val res = mutable.Map[T, Double]()
    var counter = 0
    new GeneratorOverIterator(generator).foreach {
      counter += 1
      seq => calculator(seq).foreach {
        case (t, value) => res.update(t, res.getOrElse(t, 0.0) + value)
      }
    }
    res.mapValues(_ / counter)
  }
}