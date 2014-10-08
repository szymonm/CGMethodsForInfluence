package pl.szymonmatejczyk.competetiveShapley.graphs.algorithms

import pl.szymonmatejczyk.competetiveShapley.graphs.WeightedDirectedNetwork
import pl.szymonmatejczyk.competetiveShapley.utils.PIMap

class FloydWarshall(network : WeightedDirectedNetwork) {
  def apply() : PIMap[Int, Double] = {
    val distMap = new PIMap[Int, Double](Double.MaxValue)
    def dist(i : Int, j : Int) = distMap(((i, j)))
    
    network.graph.nodes.foreach {n => distMap += (((n.value, n.value), 0.0))}
    network.graph.edges.foreach {e => distMap += (((e._1.value, e._2.value), 
                                                  e.weight / network.weightDenominator))}
    network.graph.nodes.foreach{
      k => network.graph.nodes.foreach{
        i => network.graph.nodes.foreach {
          j => 
            if (dist(i.value, j.value) > dist(i.value, k.value) + dist(k.value, j.value))
              distMap(((i.value, j.value))) = dist(i.value, k.value) + dist(k.value, j.value)
        }
      }
    }
    distMap
  }
}

object FloydWarshall {
  def apply(g : WeightedDirectedNetwork) = {
    new FloydWarshall(g).apply()
  }
}