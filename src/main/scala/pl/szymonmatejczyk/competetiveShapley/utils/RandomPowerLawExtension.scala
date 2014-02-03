package pl.szymonmatejczyk.competetiveShapley.utils

import scala.util.Random

object RandomPowerLawExtension {
  implicit class RandomPowerLawExtension(r: Random) {
    def nextPowerLaw(power: Int): Double = {
      math.pow(r.nextDouble, -power)
    }
  }
}