package pl.szymonmatejczyk.competetiveShapley.graphs.readers

import scala.collection._
import scalax.collection.Graph
import scalax.collection.edge.WDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge.Implicits._
import scala.io.Source
import scala.collection.mutable.ListBuffer
import com.typesafe.scalalogging.LazyLogging

class TXTFileReader extends GraphFromFileReader with LazyLogging {
  val EdgePat = """\s*(\d+)\s+(\d+)""".r
  val CommentPat = """(^#.*)""".r

  def readFromTxt(filename: String, weightDenominator: Long): Graph[Int, WDiEdge] = {
    val nodes = Set.newBuilder[Int]

    val edgesByTarget = mutable.Map.empty[Int, ListBuffer[Int]].
      withDefaultValue(ListBuffer[Int]())

    Source.fromFile(filename).getLines.foreach {
      case EdgePat(Int(source), Int(target)) =>
        nodes += source
        nodes += target
        edgesByTarget.getOrElseUpdate(target, ListBuffer()) += source
      case CommentPat(s) => ()
      case e => throw new IllegalStateException("unknow line " + e)
    }

    val edges = assignRandomWeights(edgesByTarget, weightDenominator)

    Graph.from[Int, WDiEdge](nodes.result.toIterable, edges)
  }
}