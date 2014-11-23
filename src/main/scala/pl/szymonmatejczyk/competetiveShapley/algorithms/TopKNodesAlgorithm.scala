package pl.szymonmatejczyk.competetiveShapley.algorithms

import pl.szymonmatejczyk.competetiveShapley.InfluenceNetwork

trait TopKNodesAlgorithm[N] {
  def topknodes: Stream[N]
}