package pl.szymonmatejczyk.competetiveShapley.graphs.readers

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import pl.szymonmatejczyk.competetiveShapley.TestGraphs
import pl.szymonmatejczyk.competetiveShapley.graphs.SubgraphFromExtension
import org.scalatest.junit.JUnitRunner
import org.scalatest.WordSpec
import scalax.collection.Graph
import scalax.collection.edge.WDiEdge

@RunWith(classOf[JUnitRunner])
class SubgraphFromExtensionTest extends WordSpec with Matchers {
  
  val randomSubgraph: Graph[Int, WDiEdge] = SubgraphFromExtension.randomSubgraph(TestGraphs.g1, 4, Some(1), 2)
  
  "Random subgraph " should {
    "have no more than vertices than specified" in {
      randomSubgraph.nodes.size should be <= 4 
    }
  }
}
	