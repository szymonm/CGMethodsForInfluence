
package pl.szymonmatejczyk.competetiveShapley.graphs.readers

import scala.collection._
import scalax.collection.Graph
import scalax.collection.edge.WDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge.Implicits._
import scala.io.Source
import java.io.IOException
import com.typesafe.scalalogging.slf4j.Logging
import scala.collection.mutable.ListBuffer

class GMLFileReader extends GraphFromFileReader with Logging {
  val EdgePat = """(\s*edge)""".r
  val SourcePat = """\s*source (\d+)""".r
  val TargetPat = """\s*target (\d+)""".r
  val ValuePat = """\s*value ([.\d]+)""".r

  def readFromGML(filename: String, weightDenominator: Double): Graph[Int, WDiEdge] = {
    val nodes = Set.newBuilder[Int]
    val edgesByTarget = mutable.Map.empty[Int, ListBuffer[Int]].
      withDefaultValue(ListBuffer[Int]())

    var source: Int = -1
    Source.fromFile(filename).getLines.foreach {
      case EdgePat(_) =>
        if (source != -1)
          throw new IOException("file format wrong")
      case SourcePat(Int(s)) =>
        source = s
        nodes += s
      case TargetPat(t) =>
        if (source == -1)
          throw new IOException("file format wrong")
        else {
          nodes += t.toInt
          edgesByTarget.getOrElseUpdate(t.toInt, ListBuffer()) += source
          source = -1
        }
      case _ => {}
    }
    val edges = assignRandomWeights(edgesByTarget, weightDenominator)
    Graph.from[Int, WDiEdge](nodes.result.toList, edges)
  }

  def readFromGMLWeighted(filename: String, weightDenominator: Double): Graph[Int, WDiEdge] = {
    val nodes = Set.newBuilder[Int]
    val edgesByTarget = mutable.Map.empty[Int, ListBuffer[(Int, Double)]]

    var source: Option[Int] = None
    var target: Option[Int] = None
    Source.fromFile(filename).getLines.foreach {
      case EdgePat(_) =>
        if (!source.isEmpty && !target.isEmpty)
          throw new IOException("file format wrong")
      case SourcePat(Int(s)) => {
        source = Some(s)
        nodes += s
      }
      case TargetPat(Int(t)) => {
        target = Some(t)
        nodes += t
      }
      case ValuePat(v) => {
        edgesByTarget.getOrElseUpdate(target.get, ListBuffer()) += ((source.get, v.toDouble))
        source = None
        target = None
      }
      case _ => {}
    }

    val edges = normalizeWeights(edgesByTarget, weightDenominator)
    Graph.from[Int, WDiEdge](nodes.result.toList, edges)
  }

}