package pl.szymonmatejczyk.competetiveShapley

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge.Implicits._

class LDAGApproximationTest extends FlatSpec with ShouldMatchers {
  val g1 = Graph(1,2,3,4,5,6, 1~>3 % 40, 3~>1 %20, 3~>5 % 90, 5~>3 % 60, 
	    2~>4 % 30, 4~>2 % 50, 4~>5 % 10, 5~>4 % 20, 5~>6 % 70, 6~>4 % 30)
  val gNet = new InfluenceNetwork(g1, 100)
	
  "LDAG approximation" should "compute ldag" in {
    gNet.threshold = 0.1
  }
  
  it should "allow predecessor traverse" in {
    gNet.ldagPredecessorsTraverse(4, 3){
      case (node, weight) => if (Set(5, 1, 7).contains(node.value)) {
        false
      } else {
        true
      }
    }
  }
}