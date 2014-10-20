package pl.szymonmatejczyk.competetiveShapley.liveGraphs

import org.scalatest.WordSpec
import org.scalatest.Matchers
import pl.szymonmatejczyk.competetiveShapley.TestGraphs
import pl.szymonmatejczyk.competetiveShapley.InfluenceNetwork
import scala.collection.immutable.Queue
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scalax.collection.Graph

import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge.Implicits._

@RunWith(classOf[JUnitRunner])
class LiveGraphTest extends WordSpec with Matchers {
  "Live graph" should {
    "compute randomly influenced from queue" which {
      "is empty if the queue is empty" in {
        val gNet = TestGraphs.g1
        val in = new InfluenceNetwork(gNet, 10)
        in.randomlyInfluencedFromQueue(Queue(), Set()).size should be (0)
        in.randomlyInfluencedFromQueue(Queue(), Set(1,2,3,4)).size should be (0)
      }
      
      "contains seed nodes if visited is empty" in {
        val gNet = TestGraphs.g1
        val in = new InfluenceNetwork(gNet, 10)
        in.randomlyInfluencedFromQueue(Queue(in.g.get(3)), Set()) should
          contain(3)
        in.randomlyInfluencedFromQueue(Queue(in.g.get(1)), Set()) should
          contain(1)
      }
      
      "contains reachable part of graph if weights are 1" in {
        val g = Graph(1 ~> 2 % 1, 2 ~> 3 % 1, 5 ~> 1 % 1)
        val in = new InfluenceNetwork(g, 1)
        in.randomlyInfluencedFromQueue(Queue(in.g.get(1)), Set()) should 
          be (Set(1, 2, 3))
        in.randomlyInfluencedFromQueue(Queue(in.g.get(1), in.g.get(2)), Set()) should 
          be (Set(1,2,3))
      }
      
      "does not contain blocked nodes that paths are blocked by visited nodes" in {
        val g = Graph(1 ~> 2 % 1, 2 ~> 3 % 1, 5 ~> 1 % 1)
        val in = new InfluenceNetwork(g, 1)
        in.randomlyInfluencedFromQueue(Queue(in.g.get(1)), Set(2)) should be (Set(1))
        
        in.randomlyInfluencedFromQueue(Queue(in.g.get(5), in.g.get(2)), Set(1,3)) should 
          be (Set(2,5))
      }
      
      "does not contain unreachable nodes" in {
        val g = Graph(1 ~> 2 % 1, 2 ~> 3 % 1, 3~> 1 % 1, 5)
        val in = new InfluenceNetwork(g, 1)
        in.randomlyInfluencedFromQueue(Queue(in.g.get(1)), Set()) should
          be (Set(1,2,3))
      }
      
      "contains all nodes that are in the queue" in {
        val g = Graph(1 ~> 2 % 1, 2 ~> 3 % 1, 3~> 1 % 1, 5, 6, 7)
        val in = new InfluenceNetwork(g, 100)
        val allNodes = in.g.nodes.toSet
        in.randomlyInfluencedFromQueue(Queue() ++ allNodes, Set()) should
          be (in.g.nodes.toOuter)
      }
    }
  }
}