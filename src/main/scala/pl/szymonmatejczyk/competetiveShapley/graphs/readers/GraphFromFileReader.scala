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
import pl.szymonmatejczyk.competetiveShapley.utils.random.RandomDivisionSampler

trait GraphFromFileReader extends Logging {

  object Int {
    def unapply(s: String): Option[Int] = try {
      Some(s.toInt)
    } catch {
      case _: java.lang.NumberFormatException => None
    }
  }

  def assignRandomWeights(edgesByTarget: Map[Int, Seq[Int]],
    weightDenominator: Double): List[WDiEdge[Int]] = {
    val edges = List.newBuilder[WDiEdge[Int]]

    edgesByTarget.foreach {
      case (target, listOfSourcesBuilder) =>
        val listOfSources = listOfSourcesBuilder
        import RandomDivisionSampler._
        (listOfSources.view zip sampleDivisionOfUnitUniformly(listOfSources.length).view).foreach {
          case (source, weight) =>
            if ((weight * weightDenominator).toLong == 0) {
              logger.debug("Edge weight == 0. Deleting.")
            } else {
              edges += source ~> target % (weight * weightDenominator).toLong
            }
        }
    }
    edges.result
  }

  def normalizeWeights[N](edgesByTarget: Map[N, Seq[(N, Double)]],
    weightDenominator: Double): List[WDiEdge[N]] = {
    val edges = List.newBuilder[WDiEdge[N]]

    edgesByTarget.foreach {
      case (target, listOfSourcesBuilder) =>
        val listOfSources = listOfSourcesBuilder
        val normalizationFactor = 1.00 / listOfSources.iterator.map(_._2).sum
        listOfSources.foreach {
          case (source, weight) =>
            if ((normalizationFactor * weight * weightDenominator).toLong == 0) {
              logger.debug("Edge weight == 0. Deleting.")
            } else {
              edges += source ~> target % (normalizationFactor *
                weight * weightDenominator).toLong
            }
        }
    }
    edges.result
  }

  def normalizeWeights[N](graph: Graph[N, WDiEdge], weightDenominator: Double): List[WDiEdge[N]] = {
    val edgesByTarget = graph.edges.map {
      case e => e.to.value -> (e.from.value, e.weight / weightDenominator)
    }.groupBy(_._1).map {
      case (k, seq) => (k, seq.map(_._2).toSeq)
    }
    normalizeWeights(edgesByTarget, weightDenominator)
  }
}

object GraphFromFileReader {
  sealed trait FileType
  case object GML extends FileType
  case object TXT extends FileType

  def read(filename: String, typ: FileType, withWeights: Boolean, weightDenominator: Long): Graph[Int, WDiEdge] = {
    (typ, withWeights) match {
      case (GML, true) => (new GMLFileReader()).readFromGMLWeighted(filename, weightDenominator)
      case (GML, false) => (new GMLFileReader()).readFromGML(filename, weightDenominator)
      case (TXT, false) => (new TXTFileReader).readFromTxt(filename, weightDenominator)
      case (TXT, true) => throw new UnsupportedOperationException
    }

  }
}