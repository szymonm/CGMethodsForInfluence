package pl.szymonmatejczyk.competetiveShapley.topKNodesAlgorithms

import pl.szymonmatejczyk.competetiveShapley.InfluenceComputation
import pl.szymonmatejczyk.competetiveShapley.InfluenceNetwork
import scala.annotation.tailrec
import pl.szymonmatejczyk.competetiveShapley.InfluenceHeuristic

trait LDAGGreedyTopNodes extends InfluenceComputation {
  def threshold: Double
  def threshold_=(threshold: Double)

  def ldagGreedyTopNodes(threshold_ : Option[Double] = None): Stream[Int] = {
    threshold_ match {
      case Some(x) => threshold_=(x)
      case None => {}
    }
    prepareData()

    def getNext() : Int = {
      logger.debug("IncInf:")
      logger.debug(incrementalInfluence.toString)
      logger.debug("AP")
      logger.debug(activationProbability.toString)
      try {
        val current = incrementalInfluence.maxBy(_._2)._1 //s
        addInfluenceNode(current)
        logger.debug("Total influence")
        logger.debug(computeTotalInfluenceFromActivationProbabilities().toString)
        logger.debug("Activation probabilities")
        logger.debug(activationProbability.mkString(";"))
        current
      } catch {
        case e: UnsupportedOperationException =>
          logger.warn(incrementalInfluence.mkString(" "))
          logger.warn(g.mkString(" "))
          throw e
      }
    }
    
    def getStream() : Stream[Int] = {
      getNext() #:: getStream()
    }
    
    getStream()
  }
}

object GreedyLDAGTopNodes {
  val NAME = "greedyLDAG"
  def influenceHeuristic(LDAG_THRESHOLD : Double) = 
    new InfluenceHeuristic("greedyLdag", (in : InfluenceNetwork) => (k : Int) => 
        in.ldagGreedyTopNodes(Some(LDAG_THRESHOLD)).take(k))

  def influenceHeuristicForSequenceOfK(threshold : Double) = streamToInfluenceHeuristic(NAME,
    (in: InfluenceNetwork) => in.ldagGreedyTopNodes(Some(threshold)))
}