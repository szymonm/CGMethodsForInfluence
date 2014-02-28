package pl.szymonmatejczyk

import scala.concurrent.duration.Duration
package object competetiveShapley {
  type IN = InfluenceNetwork
  type InfluenceHeuristic = (String, (InfluenceNetwork => Int => Seq[Int]))
  type InfluenceHeuristicForSequenceOfK = (String, (InfluenceNetwork => Seq[Int] => 
    Seq[(Seq[Int], Duration)]))
}