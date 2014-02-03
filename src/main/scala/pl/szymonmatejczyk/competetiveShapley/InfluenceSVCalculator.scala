package pl.szymonmatejczyk.competetiveShapley

import scala.collection._
import pl.szymonmatejczyk.competetiveShapley.coalitionGeneration.PermutationGenerator
import scalax.collection.Graph
import scalax.collection.edge.WDiEdge

trait InfluenceSVCalculator extends InfluenceComputation with NaiveSVApproximator {
  val g : Graph[Int, WDiEdge]
  
  val DEFAULT_THRESHOLD : Double
  def threshold_=(threshold : Double)
  
  def computeSV(iterNo : Int) : Map[Int, Double] = {
    val permutationGenerator = PermutationGenerator(g.nodes.toOuterNodes.toSet, iterNo)
    def calculator(seq : Seq[Int]) : Map[Int, Double] = {
      val res = mutable.Map[Int, Double]()
      var previousValuation = 0.0
      prepareData()
      seq.foreach { node => {
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
  
  def computeSVRanking(iterNo : Int, threshold_ : Double = DEFAULT_THRESHOLD) : Seq[(Int, Double)] = {
    threshold_=(threshold_)
    computeSV(iterNo).toSeq.sortBy(-_._2)
  }
}