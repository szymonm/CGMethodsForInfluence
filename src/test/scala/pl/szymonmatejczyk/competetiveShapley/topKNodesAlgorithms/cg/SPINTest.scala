package pl.szymonmatejczyk.competetiveShapley.topKNodesAlgorithms.cg

import org.scalatest.WordSpec
import org.scalatest.Matchers
import pl.szymonmatejczyk.competetiveShapley.TestGraphs
import pl.szymonmatejczyk.competetiveShapley.InfluenceNetwork
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SPINTest extends WordSpec with Matchers {
      /**
     * 1 ~~40~~>  3  ~~99~~>    5   ~~~70~~~> 6
     * ^~~~20~~~ | ^~~~60~~~~| | ^            |
     *                        20 10           |
     *                         v |            |
     * 2 ~~30~~~~~~~~~~~~~~~>  4              |
     * ^~~~50~~~~~~~~~~~~~~~~~| ^~~~30~~~~~~~~ 
     */
  "SPIN" should {
    "pick correct nodes" in {
      val g = TestGraphs.g2
      val in = new InfluenceNetwork(g)
      val spin = new SPIN(in)
      spin.topknodes(Seq(1, 5, 6, 3, 2, 4)) should be (Stream(1, 5, 2, 6, 3, 4))
    }
  }
}