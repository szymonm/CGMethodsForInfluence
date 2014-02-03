package pl.szymonmatejczyk.competetiveShapley

package object common {
  def doubleToLong(x : Double, denominator : Long) = (x * denominator).toLong
  
  def longToDouble(l : Long, denominator : Double) = l.toDouble / denominator
}