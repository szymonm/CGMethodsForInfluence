package pl.szymonmatejczyk.competetiveShapley

import pl.szymonmatejczyk.competetiveShapley.utils.PIMap
import pl.szymonmatejczyk.competetiveShapley.utils.Cache
import com.typesafe.scalalogging.LazyLogging

trait SingularInfluences extends Cache with LazyLogging {
  self : InfluenceNetwork => 
  // Cached data
  var singlularInfluences: Option[PIMap[Int, Double]] = None
  
  abstract override def clearCache() {
    logger.debug("Clear singular influences")
    singlularInfluences = None
    super.clearCache()
  }
  
  def computeApproxAllInfluences() {
    if (!singlularInfluences.isEmpty)
      return

    singlularInfluences = Some(new PIMap[Int, Double](0.0))
    g.nodes.foreach {
      headNode =>
        val (onNodeInfluence, nodesPriorities) = computeApproxInfluencesAndLDAGNodes(headNode.value)
        //        ldagNodes += headNode.value -> nodesPriorities
        onNodeInfluence.foreach {
          case (fromNode, influence) => singlularInfluences.get += (fromNode, headNode.value) -> influence
        }
    }
  }

  def getInfluences(): PIMap[Int, Double] = {
    if (singlularInfluences.isEmpty) {
      computeApproxAllInfluences()
    }
    singlularInfluences.get
  }
}