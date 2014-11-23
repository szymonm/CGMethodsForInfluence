package pl.szymonmatejczyk.competetiveShapley

import scala.concurrent.duration.Duration
import pl.szymonmatejczyk.competetiveShapley.utils.TestingUtils._

package object algorithms {
  def streamToInfluenceHeuristic(name: String, prepareStream: (IN => Stream[Int])): 
      InfluenceHeuristicForSequenceOfK = {
    new InfluenceHeuristicForSequenceOfK(name, (in: InfluenceNetwork) => (ks: Seq[Int]) => {
      val (s, prepareTime) = time(prepareStream(in))
      val additional = ks.map {
        d => time(s.take(d).toList)
      }
      def append(a: Vector[(Seq[Int], Duration)], b: (Seq[Int], Duration)) = {
        val last = a.lastOption.getOrElse((Seq[Int](), prepareTime))
        a :+ ((b._1, last._2 + b._2))
      }
      additional.foldLeft(Vector[(Seq[Int], Duration)]())(append)
    })
  }
}