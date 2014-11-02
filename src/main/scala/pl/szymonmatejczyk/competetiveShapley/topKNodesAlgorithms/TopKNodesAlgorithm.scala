package pl.szymonmatejczyk.competetiveShapley.topKNodesAlgorithms

import pl.szymonmatejczyk.competetiveShapley.InfluenceNetwork

trait TopKNodesAlgorithm[N] {
  def topknodes: Stream[N]
}