package pl.szymonmatejczyk.competetiveShapley

trait RankingComputation[N] {
  def ranking(): collection.Map[N, Double]
}