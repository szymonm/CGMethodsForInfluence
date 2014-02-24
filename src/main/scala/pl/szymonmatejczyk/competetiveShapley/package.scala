package pl.szymonmatejczyk

package object competetiveShapley {
  type IN = InfluenceNetwork
  type InfluenceHeuristic = (String, (InfluenceNetwork => Int => Seq[Int]))
}