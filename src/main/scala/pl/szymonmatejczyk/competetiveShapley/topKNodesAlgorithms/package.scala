package pl.szymonmatejczyk.competetiveShapley

import scala.concurrent.duration.Duration
import pl.szymonmatejczyk.competetiveShapley.utils.TestingUtils._

package object topKNodesAlgorithms {
  def streamToInfluenceHeuristic(name : String, prepareStream : (IN => Stream[Int])) 
      : InfluenceHeuristicForSequenceOfK = {
    new InfluenceHeuristicForSequenceOfK(name, (in : InfluenceNetwork) => (ks : Seq[Int]) => {
      val s = prepareStream(in)
//      val differences = ks.foldLeft(Vector[Int]())((a, b) => a :+ (b - a.sum))
      val additional = ks.map{
        d => time(Seq[Int]() ++ s.take(d))
      }
      def append(a : Vector[(Seq[Int], Duration)], b : (Seq[Int], Duration)) = {
        val last = a.lastOption.getOrElse((Seq[Int](), Duration.fromNanos(0))) 
        a :+ ((b._1, last._2 + b._2))
      }
      additional.foldLeft(Vector[(Seq[Int], Duration)]())(append)
    })
  }
}