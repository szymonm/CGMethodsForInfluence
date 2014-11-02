package pl.szymonmatejczyk.competetiveShapley

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge.Implicits._

object TestGraphs {
   val g1 = Graph(1,2,3,4,5, 6, 7, 8, 9, 1~>2 % 4, 2~>1 % 5, 3~>4 % 3, 
      4~>3 % 5, 2~>4 % 4, 1~>4 % 6, 2~>5 % 4, 4~>5 % 7)
      
   /** Graph g2
     * 1 ~~40~~>  3  ~~99~~>    5   ~~~70~~~> 6
     * ^~~~20~~~ | ^~~~60~~~~| | ^            |
     *                        20 10           |
     *                         v |            |
     * 2 ~~30~~~~~~~~~~~~~~~>  4              |
     * ^~~~50~~~~~~~~~~~~~~~~~| ^~~~30~~~~~~~~ 
     */
   val g2 = Graph(1,2,3,4,5,6, 1~>3 % 40, 3~>1 %20, 3~>5 % 99, 5~>3 % 60, 
        2~>4 % 30, 4~>2 % 50, 4~>5 % 10, 5~>4 % 20, 5~>6 % 70, 6~>4 % 30)
}