package pl.szymonmatejczyk.competetiveShapley.coalitionGeneration

trait PermutationGenerator[T] {
  def generate(): Option[Seq[T]]
}

object PermutationGenerator {
  /**
   * Returns a generator of either all permutations of `set` (if iterNo > possible permutations)
   * or a generator of random permutations.
   */
  def apply[T](set: Set[T], iterNo: Int): PermutationGenerator[T] = {
    if (math.pow(2, set.size) > iterNo) {
      new RandomPermutationGenerator(set, iterNo)
    } else
      new AllPermutationsGenerator(set)
  }
}