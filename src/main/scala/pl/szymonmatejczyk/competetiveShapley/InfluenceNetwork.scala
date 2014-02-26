package pl.szymonmatejczyk.competetiveShapley

import scala.io.Source
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge.Implicits._
import scalax.collection.edge.WDiEdge
import scalax.collection.config.CoreConfig
import scalax.collection.GraphTraversal
import scala.util.Random
import scala.collection.mutable.PriorityQueue
import scala.collection._
import scala.collection.mutable.HashMap
import scala.collection.immutable.HashSet
import scala.collection.mutable.ListBuffer
import scalax.collection.GraphTraversal
import java.io.IOException
import com.typesafe.scalalogging.slf4j.Logging
import com.typesafe.scalalogging.slf4j.Logger
import pl.szymonmatejczyk.competetiveShapley.utils.PIMap
import pl.szymonmatejczyk.competetiveShapley.coalitionGeneration.CoalitionGenerator
import pl.szymonmatejczyk.competetiveShapley.graphs.readers.GMLFileReader
import pl.szymonmatejczyk.competetiveShapley.coalitionGeneration.PermutationGenerator
import pl.szymonmatejczyk.competetiveShapley.graphs.readers.GraphFromFileReader
import pl.szymonmatejczyk.competetiveShapley.graphs.SubgraphFromExtension
import pl.szymonmatejczyk.competetiveShapley.graphs.WeightedDirectedNetwork
import pl.szymonmatejczyk.competetiveShapley.topKNodesAlgorithms.GreedyTopKNodesSearch
import pl.szymonmatejczyk.competetiveShapley.topKNodesAlgorithms.cg.LDAGBanzhafIndex
import pl.szymonmatejczyk.competetiveShapley.topKNodesAlgorithms.cg.InfluenceSVCalculator
import pl.szymonmatejczyk.competetiveShapley.ldags.InfluenceNetworkLDAGApproximation
import pl.szymonmatejczyk.competetiveShapley.ldags.LDAGApproximation
import pl.szymonmatejczyk.competetiveShapley.graphs.SizeRestriction
import pl.szymonmatejczyk.competetiveShapley.topKNodesAlgorithms.DegreeDiscount
import pl.szymonmatejczyk.competetiveShapley.topKNodesAlgorithms.CelfPlusPlus
import pl.szymonmatejczyk.competetiveShapley.liveGraphs.LiveGraph
import pl.szymonmatejczyk.competetiveShapley.liveGraphs.IncrementalInfluence
import pl.szymonmatejczyk.competetiveShapley.michalakGames.FringeGameSV
import pl.szymonmatejczyk.competetiveShapley.michalakGames.DistanceCutoffGameSV
import pl.szymonmatejczyk.competetiveShapley.michalakGames.InfluenceAboveThresholdGameSV
import pl.szymonmatejczyk.competetiveShapley.michalakGames.KFringeGameSV
import pl.szymonmatejczyk.competetiveShapley.topKNodesAlgorithms.RandomNodes
import pl.szymonmatejczyk.competetiveShapley.topKNodesAlgorithms.cg.ShapleyValueWithDiscount

class InfluenceNetwork(override val g: Graph[Int, WDiEdge], override val weightDenominator: Double = 100000.0)
      extends WeightedDirectedNetwork(g, weightDenominator)
        with LiveGraph
        with IncrementalInfluence
        with LinearThreshold
        with Logging 
        with GreedyTopKNodesSearch 
        with InfluenceSVCalculator 
        with LDAGApproximation 
        with SizeRestriction 
        with SingularInfluences 
        with InfluenceNetworkLDAGApproximation 
        with LDAGBanzhafIndex
        with DegreeDiscount
        with CelfPlusPlus
        with FringeGameSV
        with DistanceCutoffGameSV
        with InfluenceAboveThresholdGameSV 
        with KFringeGameSV
        with RandomNodes
        with ShapleyValueWithDiscount {
  implicit val config = new CoreConfig()
  
  def this(wdn : WeightedDirectedNetwork) = this(wdn.g, wdn.weightDenominator)
  
  val r = new Random
  val DEFAULT_THRESHOLD = 0.3
  private var _threshold = DEFAULT_THRESHOLD

  def threshold = _threshold
  def threshold_=(threshold: Double) {
    if (threshold != _threshold) {
      _threshold = threshold
      clearCache()
    }
  }

  override def clearCache() {
  }
  
  def size = g.nodes.size
  
  @deprecated("computes only sum of single influences", "ever")
  def computeTotatInfluences(): immutable.Map[Int, Double] = {
    val influences = getInfluences()
    def add(a: Double, b: ((Int, Int), Double)) = a + b._2

    val total = influences.all.map {
      node =>
        (node -> influences.byFirstIterator(node).foldLeft(0.0)(add _))
    }
    collection.immutable.HashMap[Int, Double]() ++ total
  }

  def debug(s: Any) {
    logger.trace(s.toString)
  }

  @deprecated("computes only sum of single influences", "ever")
  def computeTotalInfluenceOnNetwork(node: Int): Double = {
    getInfluences().foldLeft(0.0)((a, b) => b match {
      case ((from, to), value) =>
        if (from == node) a + value else a
    })
  }
}

object InfluenceNetwork extends Logging {
  val WEIGHT_DENOMINATOR = 100000L

  def fromGML(filename: String, withWeights: Boolean = false): WeightedDirectedNetwork = {
    val reader = new GMLFileReader()
    if (withWeights)
      new InfluenceNetwork(reader.readFromGMLWeighted(filename, WEIGHT_DENOMINATOR), WEIGHT_DENOMINATOR)
    else
      new InfluenceNetwork(reader.readFromGML(filename, WEIGHT_DENOMINATOR), WEIGHT_DENOMINATOR)
  }

  def fromFile(filename: String, filetype: GraphFromFileReader.FileType, withWeights: Boolean): 
      InfluenceNetwork = {
    new InfluenceNetwork(GraphFromFileReader.read(filename, filetype, withWeights,
      WEIGHT_DENOMINATOR))
  }
  
  def apply(wdn : WeightedDirectedNetwork) : InfluenceNetwork = {
    new InfluenceNetwork(wdn.g, wdn.weightDenominator)
  }
}