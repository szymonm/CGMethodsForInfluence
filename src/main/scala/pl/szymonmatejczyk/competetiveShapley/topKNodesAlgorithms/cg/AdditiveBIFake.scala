package pl.szymonmatejczyk.competetiveShapley.topKNodesAlgorithms.cg

import pl.szymonmatejczyk.competetiveShapley.topKNodesAlgorithms.cg.InfluenceAdditiveBICalculator
import pl.szymonmatejczyk.competetiveShapley.InfluenceNetwork

trait AdditiveBIFake {
  self: InfluenceNetwork =>

  def computeApproximatedBanzhafIndexAssumingAdditivity(threshold_ : Double = DEFAULT_THRESHOLD): Map[Int, Double] = {
    threshold = threshold_
    val ibic = new InfluenceAdditiveBICalculator()
    val influences = getInfluences()
    val totalInfluences = computeTotatInfluences()
    debug("ldags: \n" + ldagNodes.mkString("\n\t"))
    debug("influences: \n" + influences.mkString("\n\t"));
    debug("totalInfluences: \n" + totalInfluences.mkString("\n\t"));
    ibic.calculateBI[Int](totalInfluences, influences)
  }
}