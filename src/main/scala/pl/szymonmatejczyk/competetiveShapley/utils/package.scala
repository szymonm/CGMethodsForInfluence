package pl.szymonmatejczyk.competetiveShapley

package object utils {
  def setSimilarity[N](set1 : Set[N], set2: Set[N]) : Double = {
    set1.intersect(set2).size.toDouble / (set1 ++ set2).size
  }
}