package pl.szymonmatejczyk.competetiveShapley.coalitionGeneration

class AllPermutationsGenerator[T](val s: Set[T]) extends PermutationGenerator[T] {
  val iter = s.toList.permutations
  def generate(): Option[Seq[T]] = this.synchronized {
    if (s.isEmpty) return None
    
    if (iter.hasNext)
      Some(iter.next())
    else
      None
  }
}