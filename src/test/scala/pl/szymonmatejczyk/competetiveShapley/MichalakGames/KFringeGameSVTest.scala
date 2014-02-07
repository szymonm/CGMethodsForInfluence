package pl.szymonmatejczyk.competetiveShapley.MichalakGames

import scala.language.postfixOps
import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import scala.collection.mutable.HashMap
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge.Implicits._
import scalax.collection.edge.WDiEdge
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import pl.szymonmatejczyk.competetiveShapley.graphs.WeightedDirectedNetwork
import pl.szymonmatejczyk.competetiveShapley.graphs.Network
import pl.szymonmatejczyk.competetiveShapley.graphs.WeightedDirectedNetwork

@RunWith(classOf[JUnitRunner])
class KFringeGameSVTest extends FlatSpec with ShouldMatchers {
  val twoNodes = Graph(1 ~> 2 % 1)
  val n = new WeightedDirectedNetwork(twoNodes, 1) with KFringeGameSV
  "FringeGameSV" should "compute SV in simplest case" in {
    n.computeSV(2) shouldEqual Map(1 -> 1.0, 2 -> 1.0)
    n.computeSV(1) shouldEqual Map(1 -> 1.5, 2 -> 0.5)
  }
  
  it should "compute SV for cycle3" in {
    val cycle3 = Graph(1 ~> 2 % 1, 2 ~> 3 % 1, 3 ~> 1 % 1)
    val n1 = new WeightedDirectedNetwork(cycle3, 1) with KFringeGameSV
    n1.computeSV(2) shouldEqual Map(1 -> 1.0, 2 -> 1.0, 3 -> 1.0)
  }
}