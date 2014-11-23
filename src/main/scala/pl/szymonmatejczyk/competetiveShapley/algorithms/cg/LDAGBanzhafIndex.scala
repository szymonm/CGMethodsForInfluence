package pl.szymonmatejczyk.competetiveShapley.algorithms.cg

import scala.collection._
import pl.szymonmatejczyk.competetiveShapley.coalitionGeneration.CoalitionGenerator
import pl.szymonmatejczyk.competetiveShapley.InfluenceNetwork
import scalax.collection.GraphPredef.graphParamsToPartition
import pl.szymonmatejczyk.competetiveShapley.InfluenceNetwork
import pl.szymonmatejczyk.competetiveShapley.InfluenceHeuristic
import pl.szymonmatejczyk.competetiveShapley._
import pl.szymonmatejczyk.competetiveShapley.common._
import pl.szymonmatejczyk.competetiveShapley.utils.TestingUtils
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext

trait LDAGBanzhafIndex {
  self: InfluenceNetwork =>

  def computeLDAGBI(node: Int, iterNo: Int)(implicit executionContext: ExecutionContext): Double = {
    var bi = 0.0
    (g.nodes).toOuterNodes.foreach {
      (
        ldagHead =>
          if (ldagHead != node) {
            bi += computeBIInLDAG(ldagHead, node, iterNo)
          })
    }
    bi / g.nodes.size
  }

  def computeBIRanking(threshold_ : Double = DEFAULT_THRESHOLD,
    iterNo: Int = 10)(implicit executionContext: ExecutionContext): Seq[(Int, Double)] = {
    //computeApproximatedBanzhafIndexAssumingAdditivity(threshold_).iterator.toSeq.sortBy(-_._2)
    threshold = threshold_
    g.nodes.toOuterNodes.map { n => (n -> computeLDAGBI(n, iterNo)) }.toSeq.sortBy(-_._2)
  }

  def computeBIInLDAG(ldagHead: Int, node: Int, iterNo: Int = 10)(implicit executionContext: ExecutionContext): Double = {
    val BIA = new BanzhaffIndexApproximator()

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

object LDAGBanzhafIndex {
  val NAME = "ldagBanzhaf"

  def influenceHeuristic(iterNo: Int, threshold: Double)(implicit executionContext: ExecutionContext): InfluenceHeuristic = new InfluenceHeuristic(NAME,
    (in: IN) => (k: Int) => in.computeBIRanking(threshold, iterNo).map(_._1).take(k))

  def influenceHeuristicForSequenceOfK(iterations: Int, threshold: Double)(implicit executionContext: ExecutionContext): InfluenceHeuristicForSequenceOfK = {
      def influence(in: InfluenceNetwork)(ks: Seq[Int]): Seq[(Seq[Int], Duration)] = {
        val (rank, rtime) = TestingUtils.time(in.computeBIRanking(threshold, iterations).map(_._1))
        topKsFromRank(ks, rank).map(x => (x, rtime))
      }
    (NAME, (influence _))
  }
}