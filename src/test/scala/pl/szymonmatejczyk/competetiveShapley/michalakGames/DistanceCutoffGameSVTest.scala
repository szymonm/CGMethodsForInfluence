package pl.szymonmatejczyk.competetiveShapley.michalakGames

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

@RunWith(classOf[JUnitRunner])
class DistanceCutoffGameSVTest extends FlatSpec with ShouldMatchers {
  val twoNodes = Graph(1 ~> 2 % 1)
  val n = new WeightedDirectedNetwork(twoNodes, 2) with DistanceCutoffGameSV
  "FringeGameSV" should "compute SV in simplest case" in {
  }
}