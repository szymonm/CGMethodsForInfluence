package pl.szymonmatejczyk.competetiveShapley.topKNodesAlgorithms.cg

import scala.collection._
import pl.szymonmatejczyk.competetiveShapley.coalitionGeneration.CoalitionGenerator
import pl.szymonmatejczyk.competetiveShapley.InfluenceNetwork
import scalax.collection.GraphPredef.graphParamsToPartition
import pl.szymonmatejczyk.competetiveShapley.InfluenceNetwork

trait LDAGBanzhafIndex {
  self : InfluenceNetwork =>  
    
  def computeLDAGBI(node: Int, iterNo: Int): Double = {
    var bi = 0.0
    (g.nodes).toOuterNodes.foreach {
      (
        ldagHead =>
          if (ldagHead != node) {
            computeBIInLDAG(ldagHead, node, iterNo)
          })
    }
    bi / g.nodes.size
  }

  def computeBIRanking(threshold_ : Double = DEFAULT_THRESHOLD,
    iterNo: Int = 10): Seq[(Int, Double)] = {
    //computeApproximatedBanzhafIndexAssumingAdditivity(threshold_).iterator.toSeq.sortBy(-_._2)
    threshold = threshold_
    g.nodes.toOuterNodes.map { n => (n -> computeLDAGBI(n, iterNo)) }.toSeq.sortBy(-_._2)
  }

  def computeBIInLDAG(ldagHead: Int, node: Int, iterNo: Int = 10): Double = {
    val BIA = new BanzhaffIndexApproximator

    val coalitionGeneratorSucc = CoalitionGenerator(computeLDAGSuccessors(ldagHead, node), iterNo)
    val succSum = BIA.approximateAbsoluteBanzhaffIndex[Set[Int]](
      set => computeInfluenceInLDAG(ldagHead, ldagHead, Set(node), set),
      coalitionGeneratorSucc.generate _)

    val coalitionGeneratorPred = CoalitionGenerator(computeLDAGPredecessors(ldagHead, node), iterNo)
    val predSum = BIA.approximateAbsoluteBanzhaffIndex[Set[Int]](
      set => 1 - computeInfluenceInLDAG(ldagHead, node, set),
      coalitionGeneratorPred.generate _)
    succSum * predSum
  }
}