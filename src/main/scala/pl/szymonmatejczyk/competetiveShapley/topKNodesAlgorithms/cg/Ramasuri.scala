package pl.szymonmatejczyk.competetiveShapley.topKNodesAlgorithms.cg

import pl.szymonmatejczyk.competetiveShapley.InfluenceNetwork
import pl.szymonmatejczyk.competetiveShapley.coalitionGeneration.PermutationGenerator
import pl.szymonmatejczyk.competetiveShapley.RankingComputation
import pl.szymonmatejczyk.competetiveShapley.topKNodesAlgorithms.TopKNodesAlgorithm
import pl.szymonmatejczyk.competetiveShapley.common._

/**
 * Calculation of the Shapley value using MC method for the LT model, where
 * spread function is calculated using MC.
 */
class Ramasuri(influenceNetwork: InfluenceNetwork, outerMC: Int = 10000, innerMC: Int = 4000)
  extends NaiveSVApproximator
  with RankingComputation[Int] 
  with TopKNodesAlgorithm[Int] 
{
  def marginalContributions(nodes: Seq[influenceNetwork.g.NodeT]): collection.Map[Int, Double] = {
    val incInfluences = influenceNetwork.mcIncrementalInfluence(Set(), nodes.map(Seq(_)), innerMC)
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