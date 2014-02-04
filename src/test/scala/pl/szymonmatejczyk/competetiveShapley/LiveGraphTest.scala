package pl.szymonmatejczyk.competetiveShapley

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

@RunWith(classOf[JUnitRunner])
class LiveGraphTest  extends FlatSpec with ShouldMatchers {
  "Live graph" should "enable random spread computation" in {
    val cycle6 = Graph(1 ~> 2 % 1, 2 ~> 3 % 1, 3 ~> 4 % 1, 4 ~> 5 % 1, 5 ~> 6 % 1, 6 ~> 1 % 1)
    val network = new InfluenceNetwork(cycle6, 1)
    Await.result(network.randomSpreadingFrom(Set(1), 1000), 1.second) shouldEqual 6.0
    
    val network1 = new InfluenceNetwork(cycle6, 2)
    Await.result(network1.randomSpreadingFrom(Set(1), 1000), 1.second) should be (
        2.0 plusOrMinus 0.1)
    
    Await.result(network1.randomSpreadingFrom(Set(1, 4), 1000), 1.second) should be (
        3.5 plusOrMinus 0.1)
  }
}