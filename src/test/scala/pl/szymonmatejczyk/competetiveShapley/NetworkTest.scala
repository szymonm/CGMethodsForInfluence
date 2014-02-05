package pl.szymonmatejczyk.competetiveShapley

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge.WLDiEdge

class NetworkTest extends FlatSpec with ShouldMatchers {

  val n = new Network(Graph(1,2,3, 1~2), None)
  def majorityVoting(s : Set[Int]) : Double = if (s.size > 1) 1 else 0
  
  def vetoGroup(group : Set[Int])(s : Set[Int]) : Double = if (group.diff(s).isEmpty) 1 else 0
    
  "A Network" should "compute ABI for majority voting" in {
    n.approximateAbsoluteBanzhafIndex(1, majorityVoting) - 0.5 should be < (0.1)
    n.approximateAbsoluteBanzhafIndex(2, majorityVoting) - 0.5 should be < (0.1)
  }
  
  it should "compute SV for majority voting" in {
	n.approximateShapleyValue(3, majorityVoting) - 0.33 should be < (0.1)
  }
  
  it should "compute Bests by BI" in {
    n.chooseTopKByBIApprox(2, vetoGroup(Set(1,2))).sorted should be (Seq(1,2))
  }
}