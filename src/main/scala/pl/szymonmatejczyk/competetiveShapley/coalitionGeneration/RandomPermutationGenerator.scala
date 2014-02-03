package pl.szymonmatejczyk.competetiveShapley.coalitionGeneration

import scala.util.Random

class RandomPermutationGenerator[T](val s: Set[T], iterNo: Int) extends PermutationGenerator[T] {
  var counter = 0
  val r = new Random
  val seq = s.toSeq

  def generate(): Option[Seq[T]] = {
    if (s.isEmpty) return None

    if (counter < iterNo) {
      counter += 1
      Some(r.shuffle(seq))
    } else {
      None
    }
  }
}