package pl.szymonmatejczyk.competetiveShapley

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge.Implicits._

object TestGraphs {
   val g1 = Graph(1,2,3,4,5, 6, 7, 8, 9, 1~>2 % 4, 2~>1 % 5, 3~>4 % 3, 
      4~>3 % 5, 2~>4 % 4, 1~>4 % 6, 2~>5 % 4, 4~>5 % 7)
}