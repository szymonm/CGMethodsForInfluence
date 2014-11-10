package pl.szymonmatejczyk.competetiveShapley.coalitionGeneration

import scala.util.Random
import java.util.concurrent.atomic.AtomicInteger
import com.typesafe.scalalogging.LazyLogging

class RandomPermutationGenerator[T](val s: Set[T], iterNo: Int) extends PermutationGenerator[T] with LazyLogging {
  val counter = new AtomicInteger(0)
  val r = new Random
  val seq = s.toSeq

  def generate(): Option[Seq[T]] = {
    if (s.isEmpty) return None
    logger.info(s"Coalition no: ${counter.get()}")
    if (counter.getAndIncrement() < iterNo) {
      Some(r.shuffle(seq))
    } else {
      None
    }
  }
}