package pl.szymonmatejczyk.competetiveShapley.graphs.readers

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge.Implicits._

@RunWith(classOf[JUnitRunner])
class GMLFileReaderTest extends FlatSpec with ShouldMatchers {
  val WEIGHT_DENOMINATOR = 10000L
  val reader = new GMLFileReader()
  "GMLFileReader" should "read GMl with weights" in {
    val g = reader.readFromGMLWeighted("../graphs/gml/simple.gml", WEIGHT_DENOMINATOR)
    g.get(1).outgoing.size shouldEqual (2)
    g.get(3).incoming.size shouldEqual (2)
    for (i <- 1 to 3) {
      g.get(i).incoming.map(_.weight).sum should be <= WEIGHT_DENOMINATOR
    }
  }
  it should "be able to read lesmiserables" in {
    val g = reader.readFromGMLWeighted("../graphs/gml/lesmiserables[W].gml", WEIGHT_DENOMINATOR)
  }
  
  it should "be alge to normalize weights" in {
    val g = Graph(1, 2, 3, 2 ~> 1 %2, 3 ~> 1 % 4)
    println(reader.normalizeWeights(g, 100))
  }
}