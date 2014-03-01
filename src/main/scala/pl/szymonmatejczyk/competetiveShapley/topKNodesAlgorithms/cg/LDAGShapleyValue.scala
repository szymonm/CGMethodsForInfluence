package pl.szymonmatejczyk.competetiveShapley.topKNodesAlgorithms.cg

import scala.collection._
import scalax.collection.Graph
import scalax.collection.edge.WDiEdge
import scalax.collection.GraphPredef.graphParamsToPartition
import pl.szymonmatejczyk.competetiveShapley._
import pl.szymonmatejczyk.competetiveShapley.common._
import pl.szymonmatejczyk.competetiveShapley.coalitionGeneration.PermutationGenerator
import pl.szymonmatejczyk.competetiveShapley.InfluenceComputation
import scala.concurrent.duration.Duration
import pl.szymonmatejczyk.competetiveShapley.utils.TestingUtils


trait LDAGShapleyValue extends InfluenceComputation with NaiveSVApproximator {
  val g: Graph[Int, WDiEdge]

  val DEFAULT_THRESHOLD: Double
  def threshold_=(threshold: Double)

  def computeSV(iterNo: Int): Map[Int, Double] = {
    val permutationGenerator = PermutationGenerator(g.nodes.toOuterNodes.toSet, iterNo)
    def calculator(seq: Seq[Int]): Map[Int, Double] = {
      val res = mutable.Map[Int, Double]()
      var previousValuation = 0.0
      prepareData()
      seq.foreach { node =>
        {
          addInfluenceNode(node)
          val valuation = computeTotalInfluenceFromActivationProbabilities()
          res += node -> (valuation - previousValuation)
          previousValuation = valuation
        }
      }
      res
    }
    approximate(permutationGenerator.generate _, calculator _)
  }

  def computeSVRanking(iterNo: Int, threshold_ : Double = DEFAULT_THRESHOLD): Seq[(Int, Double)] = {
    threshold_=(threshold_)
    computeSV(iterNo).toSeq.sortBy(-_._2)
  }
}

object LDAGShapleyValue {
  val NAME = "ldagShapley"
    
  def influenceHeuristic(ITER_NO : Int): InfluenceHeuristic = new InfluenceHeuristic(NAME, 
      (in: IN) => (k: Int) => topKFromMap[Int](k, in.computeSV(ITER_NO)))

  def influenceHeuristicForSequenceOfK(iterations : Int, threshold : Double) 
      : InfluenceHeuristicForSequenceOfK = {
    def influence(in: InfluenceNetwork)(ks: Seq[Int]): Seq[(Seq[Int], Duration)] = {
      val (rank, rtime) = TestingUtils.time(in.computeSVRanking(iterations, threshold))
      topKsFromRank(ks, rank.map(_._1)).map(x => (x, rtime))
    }
    (NAME, (influence _))
  }
}