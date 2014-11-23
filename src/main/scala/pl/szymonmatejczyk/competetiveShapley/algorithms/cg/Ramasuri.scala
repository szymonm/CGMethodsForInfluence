package pl.szymonmatejczyk.competetiveShapley.algorithms.cg

import pl.szymonmatejczyk.competetiveShapley.InfluenceNetwork
import pl.szymonmatejczyk.competetiveShapley.coalitionGeneration.PermutationGenerator
import pl.szymonmatejczyk.competetiveShapley.RankingComputation
import pl.szymonmatejczyk.competetiveShapley.algorithms.TopKNodesAlgorithm
import pl.szymonmatejczyk.competetiveShapley.common._
import scala.concurrent.ExecutionContext

/**
 * Calculation of the Shapley value using MC method for the LT model, where
 * spread function is calculated using MC.
 */
class Ramasuri(influenceNetwork: InfluenceNetwork, outerMC: Int = 10000, innerMC: Int = 4000)
    (implicit executionContext: ExecutionContext)
  extends NaiveSVApproximator
  with RankingComputation[Int] 
  with TopKNodesAlgorithm[Int] 
{
  def marginalContributions(nodes: Seq[influenceNetwork.g.NodeT]): collection.Map[Int, Double] = {
    val incInfluences = influenceNetwork.mcIncrementalLTInfluence(nodes.map(n => Set(n.value)), innerMC)
    Map[Int, Double]() ++ (nodes.map(_.value)).zip(incInfluences)
  }

  def apply(): collection.Map[Int, Double] = {
    approximate[influenceNetwork.g.NodeT, Int](
        PermutationGenerator(influenceNetwork.g.nodes.toSet, outerMC).generate, 
        marginalContributions)
  }
  
  def ranking(): collection.Map[Int, Double] = apply()
  
  def topknodes(): Stream[Int] = {
    rankFromMap(ranking()).toStream
  }
}

object Ramasuri {
  
  
}