package pl.szymonmatejczyk.competetiveShapley.graphs.readers
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge.Implicits._
import scalax.collection.config.CoreConfig
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import pl.szymonmatejczyk.competetiveShapley.graphs.SubgraphFromExtension
import pl.szymonmatejczyk.competetiveShapley.graphs.SubgraphFromExtension

@RunWith(classOf[JUnitRunner])
class SubgraphFromExtensionTest extends FlatSpec with ShouldMatchers {
  val g = Graph(1,2,3,4,5, 6, 7, 8, 9, 1~>2 % 4, 2~>1 % 5, 3~>4 % 3, 4~>3 % 5, 2~>4 % 4, 1~>4 % 6, 2~>5 % 4, 
      4~>5 % 7)
  
  println(SubgraphFromExtension.randomSubgraph(g, 4, Some(1)).mkString("\n"))
}
