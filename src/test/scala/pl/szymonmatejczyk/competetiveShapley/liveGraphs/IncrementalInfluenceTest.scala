package pl.szymonmatejczyk.competetiveShapley.liveGraphs

import collection._
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.util.Random
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge.Implicits._
import scalax.collection.edge.WDiEdge
import pl.szymonmatejczyk.competetiveShapley.graphs.WeightedDirectedNetwork
import pl.szymonmatejczyk.competetiveShapley.InfluenceNetwork
import org.scalatest.Matchers


@RunWith(classOf[JUnitRunner])
class IncrementalInfluenceTest extends FlatSpec with Matchers {
  "IncrementalInfluence" should "compute random live graph" in {
    /**
     * 1 ~~40~~>  3  ~~99~~>    5   ~~~70~~~> 6
     * ^~~~20~~~ | ^~~~60~~~~| | ^            |
     *                        20 10           |
     *                         v |            |
     * 2 ~~30~~~~~~~~~~~~~~~>  4              |
     * ^~~~50~~~~~~~~~~~~~~~~~| ^~~~30~~~~~~~~ 
     */
    val g1 = Graph(1,2,3,4,5,6, 1~>3 % 40, 3~>1 %20, 3~>5 % 99, 5~>3 % 60, 
        2~>4 % 30, 4~>2 % 50, 4~>5 % 10, 5~>4 % 20, 5~>6 % 70, 6~>4 % 30)
    val gNet = new InfluenceNetwork(g1, 100)
    
    gNet.mcIncrementalInfluence(immutable.Set(gNet.g.get(3)), Seq(Seq(gNet.g.get(5))), 10) shouldEqual List(0)
  }
}