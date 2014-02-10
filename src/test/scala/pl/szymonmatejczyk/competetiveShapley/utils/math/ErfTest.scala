package pl.szymonmatejczyk.competetiveShapley.utils.math

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.util.Random

import Erf._

@RunWith(classOf[JUnitRunner])
class ErfTest extends FlatSpec with ShouldMatchers {
  "Erf" should "work for N(0,1) cdf" in {
    normalCDF(0, 0, 1) should be (0.5 plusOrMinus 0.01)
    normalCDF(-2.0, 0, 1) should be < 0.05
    normalCDF(2.0, 0, 1) should be > 0.95
    normalCDF(-1.0, 0, 1) should be (0.15 plusOrMinus 0.02)
    normalCDF(-19, 0, 1) should be (0.0 plusOrMinus 0.01)
  }
  
  it should "work for N(2,2), N(2,4) cdf" in {
    normalCDF(5.5, 2, 2) should be (0.96 plusOrMinus 0.01)
    normalCDF(1.7, 2, 4) should be (0.47 plusOrMinus 0.01)
  }
}