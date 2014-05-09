package pl.szymonmatejczyk.competetiveShapley.coalitionGeneration

class AllPermutationsGenerator[T](val s: Set[T]) extends PermutationGenerator[T] {
  val iter = s.toList.permutations
  def generate(): Option[Seq[T]] = {
    if (s.isEmpty) return None

    if (iter.hasNext)
      Some(iter.next())
    else
      None
  }
}