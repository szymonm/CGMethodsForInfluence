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
      logger.debug("IncInf:")
      logger.debug(incrementalInfluence.toString)
      logger.debug("AP")
      logger.debug(activationProbability.toString)
      try {
        val current = incrementalInfluence.maxBy(_._2)._1 //s
        builder += current
        addInfluenceNode(current)

        logger.debug("Total influence")
        logger.debug(computeTotalInfluenceFromActivationProbabilities().toString)
        logger.debug("Activation probabilities")
        logger.debug(activationProbability.mkString(";"))
      } catch {
        case e: UnsupportedOperationException =>
          logger.warn(incrementalInfluence.mkString(" "))
          logger.warn(g.mkString(" "))
          throw e
      }
    }
    builder.result
  }
}