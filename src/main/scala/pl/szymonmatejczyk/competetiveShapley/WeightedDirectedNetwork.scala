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

class WeightedDirectedNetwork(val g: Graph[Int, WDiEdge], val weightDenominator: Double = 100000.0)
        extends Network[Int, WDiEdge](g, Some(weightDenominator))  {
}
