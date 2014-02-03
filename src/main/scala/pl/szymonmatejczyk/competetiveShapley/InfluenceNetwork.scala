package pl.szymonmatejczyk.competetiveShapley

import scala.io.Source
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge.Implicits._
import scalax.collection.edge.WDiEdge
import scalax.collection.config.CoreConfig
import scalax.collection.GraphTraversal
import scala.util.Random
import scala.collection.mutable.PriorityQueue
import scala.collection._
import scala.collection.mutable.HashMap
import scala.collection.immutable.HashSet
import scala.collection.mutable.ListBuffer
import scalax.collection.GraphTraversal
import java.io.IOException
import com.typesafe.scalalogging.slf4j.Logging
import com.typesafe.scalalogging.slf4j.Logger
import pl.szymonmatejczyk.competetiveShapley.utils.PIMap
import pl.szymonmatejczyk.competetiveShapley.coalitionGeneration.CoalitionGenerator
import pl.szymonmatejczyk.competetiveShapley.utils.RandomDivisionSampler
import pl.szymonmatejczyk.competetiveShapley.graphs.readers.GMLFileReader
import pl.szymonmatejczyk.competetiveShapley.coalitionGeneration.PermutationGenerator
import pl.szymonmatejczyk.competetiveShapley.graphs.readers.GraphFromFileReader
import pl.szymonmatejczyk.competetiveShapley.graphs.SubgraphFromExtension

class InfluenceNetwork(g: Graph[Int, WDiEdge], val weightDenominator: Double = 100000.0)
  extends Network[Int, WDiEdge](g) with LiveGraph with Logging 
          with GreedyTopKNodesSearch with InfluenceSVCalculator with LDAGApproximation {
  implicit val config = new CoreConfig()
  val r = new Random
  val DEFAULT_THRESHOLD = 0.3
  private var _threshold = DEFAULT_THRESHOLD
  
  // Cached data
  var singlularInfluences : Option[PIMap[Int, Double]] = None
  
  def threshold = _threshold
  def threshold_=(threshold : Double) {
    if (threshold != _threshold) {
      _threshold = threshold
      clearCache()
    }
  }
  
  override def clearCache() {
    logger.debug("Clear influences")
    singlularInfluences = None
    super.clearCache()
  }

  def restrictSize(maxSize : Int, fromNode : Option[Int] = None) : InfluenceNetwork = {
    if (g.size < maxSize) 
      this
    else {
      val newGraph = SubgraphFromExtension.randomSubgraph(g, maxSize, fromNode)
      new InfluenceNetwork(newGraph, weightDenominator)
    }
  }
  /**
   * Computes influences approximated in LDAG from computeLDAG method and
   * nodes in the LDAG. (Algorithm 4)
   */
  def computeApproxInfluencesAndLDAGNodes(onNode: Int): 
	  (immutable.Map[Int, Double], immutable.Map[Int, Int]) = {
    val res = HashMap[Int, Double]().withDefault(_ => 0.0)
    res += onNode -> 1.0
    val (ldagNodes, _) = computeLDAG(onNode)

    object Visitors {
      import scalax.collection.GraphTraversal.VisitorReturn._
      def propagateInfluenceV(node: g.NodeT) = {
        Continue
      }
      def propagateInfluenceE(edge: g.EdgeT) = {
        res += (edge._1.value -> (res(edge._1.value) + edge.weight / weightDenominator *
          res(edge._2.value)))
        Continue
      }
    }
    import Visitors._

    g.get(onNode).traverse(direction = GraphTraversal.Predecessors,
      breadthFirst = true,
      edgeFilter = {e : g.EdgeT => SubgraphVisitor.edgeFilter(ldagNodes,
            												  true, true)(e.toEdgeIn)})(
        nodeVisitor = propagateInfluenceV,
        edgeVisitor = propagateInfluenceE)

    ((collection.immutable.HashMap[Int, Double]() ++ res, ldagNodes))
  }

  def computeApproxAllInfluences() {
    if (!singlularInfluences.isEmpty)
      return
      
    singlularInfluences = Some(new PIMap[Int, Double](0.0))
    g.nodes.foreach {
      headNode =>
        val (onNodeInfluence, nodesPriorities) = computeApproxInfluencesAndLDAGNodes(headNode.value)
//        ldagNodes += headNode.value -> nodesPriorities
        onNodeInfluence.foreach {
          case (fromNode, influence) => singlularInfluences.get += (fromNode, headNode.value) -> influence
        }
    }
  }

  def getInfluences(): PIMap[Int, Double] = {
    if (singlularInfluences.isEmpty) {
      computeApproxAllInfluences()
    }
    singlularInfluences.get
  }

  def computeTotatInfluences(): immutable.Map[Int, Double] = {
    val influences = getInfluences()
    def add(a: Double, b: ((Int, Int), Double)) = a + b._2
    
    val total = influences.all.map {
      node => 
        (node -> influences.byFirstIterator(node).foldLeft(0.0)(add _))
    }
    collection.immutable.HashMap[Int, Double]() ++ total
  }
  
  def debug(s : Any) {
    logger.trace(s.toString)
  }
  
  def computeApproximatedBanzhafIndexAssumingAdditivity(threshold_ : Double = DEFAULT_THRESHOLD) : Map[Int, Double] = {
    threshold = threshold_
    val ibic = new InfluenceAdditiveBICalculator()
    val influences = getInfluences()
    val totalInfluences = computeTotatInfluences()
    debug("ldags: \n" + ldagNodes.mkString("\n\t"))
    debug("influences: \n" + influences.mkString("\n\t"));
    debug("totalInfluences: \n" + totalInfluences.mkString("\n\t"));
    ibic.calculateBI[Int](totalInfluences, influences)
  }
  
  def computeBIRanking(threshold_ : Double = DEFAULT_THRESHOLD,
		  			   iterNo : Int = 10) : Seq[(Int, Double)] = {
    //computeApproximatedBanzhafIndexAssumingAdditivity(threshold_).iterator.toSeq.sortBy(-_._2)
    threshold = threshold_
    g.nodes.toOuterNodes.map{n => (n -> computeBI(n, iterNo))}.toSeq.sortBy(-_._2)
  }

  def computeTotalInfluenceOnNetwork(node: Int): Double = {
    getInfluences().foldLeft(0.0)((a, b) => b match {
      case ((from, to), value) =>
        if (from == node) a + value else a
    })
  }
  
  def computeInfluenceInLDAG(ldagHeadNode : Int, onNode : Int, from : Set[Int],
                             blockedNodes : Set[Int] = Set()) : Double = {
    var influence = 0.0
    ldagPredecessorsTraverse(ldagHeadNode, onNode){
      case (node, pathValue) => if (from.contains(node.value)) {
    	influence += pathValue
    	false
      } else {
        !blockedNodes.contains(node.value)
      }
    }
    influence
  }
  
  def computeBIInLDAG(ldagHead : Int, node : Int, iterNo : Int = 10) : Double = {
    val BIA = new BanzhaffIndexApproximator
    
    val coalitionGeneratorSucc = CoalitionGenerator(computeLDAGSuccessors(ldagHead, node) , iterNo)
    val succSum = BIA.approximateAbsoluteBanzhaffIndex[Set[Int]](
        set => computeInfluenceInLDAG(ldagHead, ldagHead, Set(node), set), 
        coalitionGeneratorSucc.generate _)
        
    val coalitionGeneratorPred = CoalitionGenerator(computeLDAGPredecessors(ldagHead, node), iterNo)
    val predSum = BIA.approximateAbsoluteBanzhaffIndex[Set[Int]](
        set => 1 - computeInfluenceInLDAG(ldagHead, node, set),
        coalitionGeneratorPred.generate _)
    succSum * predSum
  }
  
  def computeBI(node : Int, iterNo : Int) : Double = {
    var bi = 0.0
    g.nodes.toOuterNodes.foreach{
      ldagHead => if (ldagHead != node) {
        bi += computeBIInLDAG(ldagHead, node, iterNo)
      }
    }
    bi / g.nodes.size
  }
  
}
object InfluenceNetwork extends Logging {
  val WEIGHT_DENOMINATOR = 100000L
  
  def fromGML(filename : String, withWeights : Boolean = false) : InfluenceNetwork = {
    val reader = new GMLFileReader()
    if (withWeights)
      new InfluenceNetwork(reader.readFromGMLWeighted(filename, WEIGHT_DENOMINATOR), WEIGHT_DENOMINATOR)
    else
      new InfluenceNetwork(reader.readFromGML(filename, WEIGHT_DENOMINATOR), WEIGHT_DENOMINATOR)
  }
  
  def fromFile(filename : String, filetype : GraphFromFileReader.FileType, withWeights : Boolean) :
    InfluenceNetwork = {
    new InfluenceNetwork(GraphFromFileReader.read(filename, filetype, withWeights, 
        WEIGHT_DENOMINATOR))
  }
}