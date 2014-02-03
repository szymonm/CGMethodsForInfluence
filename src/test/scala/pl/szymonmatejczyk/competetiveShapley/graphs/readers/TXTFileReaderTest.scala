package pl.szymonmatejczyk.competetiveShapley.graphs.readers

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TXTFileReaderTest extends FlatSpec with ShouldMatchers {
  val WEIGHT_DENOMINATOR = 10000L
  val reader = new TXTFileReader()
  "TXTFileReader" should "read as-skitter" in {
    val g = reader.readFromTxt("../graphs/txt/simple.txt", WEIGHT_DENOMINATOR)
    print(g.mkString("\n"))
  }
}