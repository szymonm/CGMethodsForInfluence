package pl.szymonmatejczyk.competetiveShapley

import pl.szymonmatejczyk.competetiveShapley.utils.PIMap
import scala.collection.SortedMap
import scala.collection.immutable.TreeMap

class InfluenceAdditiveBICalculator {
  /**
   * totalInflueces := node -> total influence on network
   * singleToSingleInfluecs := (fromNode, toNode) -> influence
   */
  def calculateBI[N](totalInfluences: Map[N, Double], singleToSingleInfluence: PIMap[N, Double]): Map[N, Double] = {
    Map() ++ totalInfluences.map {
      case (node, value) =>
        val singleInfluenceOfOthersSum = singleToSingleInfluence.bySecondIterator(node).map {
          case ((from, `node`), strength) => if (from != node) strength else 0.0
        }.fold(0.0)((x, y) => x * y)
        (node -> (0.5 * value - (1 - singleInfluenceOfOthersSum)))
    }
  }
}