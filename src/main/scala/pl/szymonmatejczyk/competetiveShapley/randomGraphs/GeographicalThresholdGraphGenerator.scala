package pl.szymonmatejczyk.competetiveShapley.randomGraphs

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge.Implicits._
import scala.util.Random
import scalax.collection.edge.WDiEdge

import pl.szymonmatejczyk.competetiveShapley.common._

class GeographicalThresholdGraphGenerator(theta: Double, alpha: Double = 2,
  val weightDenominator: Long = GraphGenerator.DEFAULT_WEIGHT_DENOMINATOR)
  extends GraphGenerator {
  val r = new Random

  import pl.szymonmatejczyk.competetiveShapley.utils.random.RandomPowerLawExtension._

  override def generateGraph[N: Manifest](nodes: Seq[N]): Graph[N, WDiEdge] = {
    val positions: Map[N, (Double, Double)] = Map() ++ nodes.
      map { x => x -> (r.nextDouble, r.nextDouble) }
    val weights = Map() ++ nodes.map { x => x -> r.nextPowerLaw(10) }

    def dist(x: (Double, Double), y: (Double, Double)): Double = {
      math.pow(math.pow(x._1 - y._1, 2) + math.pow(x._1 - y._1, 2), 0.5)
    }

    val edges: Seq[WDiEdge[N]] = for (
      x <- nodes; y <- nodes if ((x != y) &&
        theta * math.pow(dist(positions(x), positions(y)), alpha) <= weights(x) + weights(y))
    ) yield x ~> y % doubleToLong(weights(x) / weights(y), weightDenominator)

    Graph.from(nodes, edges)
  }
}