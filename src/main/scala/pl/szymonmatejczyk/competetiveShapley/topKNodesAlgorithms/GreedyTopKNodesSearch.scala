package pl.szymonmatejczyk.competetiveShapley.topKNodesAlgorithms

import pl.szymonmatejczyk.competetiveShapley.InfluenceComputation

trait GreedyTopKNodesSearch extends InfluenceComputation {
  def threshold: Double
  def threshold_=(threshold: Double)

  def greedyMostInfluentSearch(k: Int, threshold_ : Option[Double] = None): List[Int] = {
    threshold_ match {
      case Some(x) => threshold_=(x)
      case None => {}
    }
    val builder = List.newBuilder[Int]
    prepareData()

    while (currentInitialSet.size < k) {
      debug("IncInf:")
      debug(incrementalInfluence)
      debug("AP")
      debug(activationProbability)
      try {
        val current = incrementalInfluence.maxBy(_._2)._1 //s
        builder += current
        addInfluenceNode(current)

        debug("Total influence")
        debug(computeTotalInfluenceFromActivationProbabilities())
        debug("Activation probabilities")
        debug(activationProbability.mkString(";"))
      } catch {
        case e: UnsupportedOperationException =>
          println(incrementalInfluence.mkString(" "))
          println(g.mkString(" "))
          throw e
      }
    }
    builder.result
  }
}