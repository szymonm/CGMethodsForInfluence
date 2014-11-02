package pl.szymonmatejczyk.competetiveShapley

import scala.BigDecimal
import scala.collection.immutable.HashMap
import org.junit.runner.RunWith
import org.scalatest.Finders
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scalax.collection.Graph
import scalax.collection.GraphPredef.EdgeAssoc
import scalax.collection.GraphPredef.anyToNode
import scalax.collection.edge.Implicits.edge2WDiEdgeAssoc
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class InfluenceNetworkTest extends FlatSpec with Matchers {
	val g = Graph(1, 2, 3, 4, 1~>2 % 2000, 2~>3 % 1000)
	val inet = new InfluenceNetwork(g, 10000)
	
	"Live graph for 1" should "be g" ignore {
		inet.randomLiveGraphFrom(Set(1)) shouldEqual Set(1,2,3) 
	}
	
	"Live graph for 2" should "be (2,3)" ignore {
		inet.randomLiveGraphFrom(Set(2)) shouldEqual Set(2,3) 
	}
	
	"Live graph for 3" should "be (3)" ignore {
		inet.randomLiveGraphFrom(Set(3)) shouldEqual Set(3) 
	}
	
	val path = Graph(1,2,3,4,5, 1~>2 % 50, 2~>3 % 50, 3~>4 % 50, 4~>5 % 50)
	val cycleNet = new InfluenceNetwork(path, 100)
	"ldag for last node in cycle" should "contain previous nodes" in {
	  cycleNet.threshold = 0.24
	  cycleNet.computeLDAG(5)._1.keySet shouldEqual Set(3, 4, 5) 
	}
	
	val paths = path + (6 ~> 5 % 50) + (7 ~> 6 % 50)
	val pathsNet = new InfluenceNetwork(paths, 100)
	"ldag for last node in 2paths" should "contain previous nodes" in {
	  pathsNet.threshold = 0.2
	  pathsNet.computeLDAG(5)._1.keySet shouldEqual Set(3, 4, 5, 6, 7) 
	}
	
	val g1 = Graph(1,2,3,4,5,6, 1~>3 % 40, 3~>1 %20, 3~>5 % 90, 5~>3 % 60, 
	    2~>4 % 30, 4~>2 % 50, 4~>5 % 10, 5~>4 % 20, 5~>6 % 70, 6~>4 % 30)
	val gNet = new InfluenceNetwork(g1, 100)
	
	"influences for node 5 in g1" should "be correct" in {
	  gNet.threshold = 0.25
	  val ldagInfluences = gNet.computeApproxInfluencesAndLDAGNodes(5)
	  ldagInfluences._2 shouldEqual Map(5 -> 1, 1 -> 3, 3 -> 2)
	  ldagInfluences._2.size should be (3)
	  ldagInfluences._1(5) should be (1.0)
 	  ldagInfluences._1(1) should be (0.36 +- 0.01)
 	  ldagInfluences._1(3) should be (0.9 +- 0.01)
	}
	
	def round(d : Double) : Double = {
	  BigDecimal(d).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
	}
	
	"total influences" should "be correct" ignore {
	  gNet.threshold = 0.25
	  gNet.computeTotatInfluences().toArray.sortBy(-_._2).map(x => (x._1, round(x._2))) shouldEqual
	  	Array((3,2.9), (5,2.71), (1,2.01), (4,1.5), (6,1.3), (2,1.3))
	}
	
	"greedy influence algorithm" should "work correctly" in {
	  gNet.threshold = 0.3
	  gNet.ldagGreedyTopNodes().take(3) shouldEqual List(3, 4, 1)
	  gNet.threshold = 0.1
	  gNet.computeTotalInfluence(Set(3,4,1)) should be  (5.03 +- 0.1)
	}
	
	it should "compute ldag predecessors and successors" in {
	  gNet.threshold = 0.25
	  gNet.clearCache()
	  gNet.computeLDAGPredecessors(4, 3) shouldEqual Set()
	  gNet.computeLDAGSuccessors(4, 3) shouldEqual Set(5,6,4)
	}
	
	it should "compute BI using ldags" in {
	  val ranking = gNet.computeBIRanking(0.1, 100)
	  val value = gNet.computeTotalInfluence(ranking.iterator.map(_._1).take(3).toIterable)
	}
	
	it should "parse GML graph correctly" in {
	  InfluenceNetwork.fromGML("src/main/resources/football.gml")
	}
	
	it should "load bigger data and compute ldags" in {
	  val n1 = InfluenceNetwork.fromGML("src/main/resources/football.gml")
	}
	
}