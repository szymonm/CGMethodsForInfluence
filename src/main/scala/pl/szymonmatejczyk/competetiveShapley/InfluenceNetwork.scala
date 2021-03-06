package pl.szymonmatejczyk.competetiveShapley

import scala.collection.immutable
import scala.util.Random

import com.typesafe.scalalogging.LazyLogging

import pl.szymonmatejczyk.competetiveShapley.utils.Cache
import pl.szymonmatejczyk.competetiveShapley.graphs.SizeRestriction
import pl.szymonmatejczyk.competetiveShapley.graphs.WeightedDirectedNetwork
import pl.szymonmatejczyk.competetiveShapley.graphs.readers.GMLFileReader
import pl.szymonmatejczyk.competetiveShapley.graphs.readers.GraphFromFileReader
import pl.szymonmatejczyk.competetiveShapley.ldags.InfluenceNetworkLDAGApproximation
import pl.szymonmatejczyk.competetiveShapley.ldags.LDAGApproximation
import pl.szymonmatejczyk.competetiveShapley.liveGraphs.IncrementalInfluence
import pl.szymonmatejczyk.competetiveShapley.liveGraphs.LiveGraph
import pl.szymonmatejczyk.competetiveShapley.michalakGames.DistanceCutoffGameSV
import pl.szymonmatejczyk.competetiveShapley.michalakGames.FringeGameSV
import pl.szymonmatejczyk.competetiveShapley.michalakGames.InfluenceAboveThresholdGameSV
import pl.szymonmatejczyk.competetiveShapley.michalakGames.KFringeGameSV
import pl.szymonmatejczyk.competetiveShapley.algorithms.CelfPlusPlus
import pl.szymonmatejczyk.competetiveShapley.algorithms.DegreeDiscount
import pl.szymonmatejczyk.competetiveShapley.algorithms.LDAGGreedyTopNodes
import pl.szymonmatejczyk.competetiveShapley.algorithms.RandomNodes
import pl.szymonmatejczyk.competetiveShapley.algorithms.cg.LDAGBanzhafIndex
import pl.szymonmatejczyk.competetiveShapley.algorithms.cg.LDAGShapleyValue
import pl.szymonmatejczyk.competetiveShapley.algorithms.cg.ShapleyValueWithDiscount
import scalax.collection.Graph
import scalax.collection.config.CoreConfig
import scalax.collection.edge.WDiEdge

class InfluenceNetwork(override val g: Graph[Int, WDiEdge], override val weightDenominator: Double = 100000.0)
      extends WeightedDirectedNetwork(g, weightDenominator)
        with LiveGraph
        with IncrementalInfluence
        with LinearThreshold
        with LazyLogging 
        with LDAGGreedyTopNodes 
        with LDAGShapleyValue 
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
  
  def this(wdn : WeightedDirectedNetwork) = this(wdn.graph, wdn.weightDenominator)
  
  val r = new Random
  
  def size = g.nodes.size
  
  @deprecated("computes only sum of single influences", "ever")
  def computeTotatInfluences(): immutable.Map[Int, Double] = {
    val influences = getInfluences()
    def addSecond(a: Double, b: ((Int, Int), Double)) = a + b._2

    val total = influences.all.map {
      node =>
        node -> influences.byFirstIterator(node).foldLeft(0.0)(addSecond)
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

object InfluenceNetwork extends LazyLogging {
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
    new InfluenceNetwork(wdn.graph, wdn.weightDenominator)
  }
}