package pl.szymonmatejczyk.competetiveShapley.utils

class GeneratorOverIterator[T](generator: () => Option[T]) extends Iterator[T] {
  var nextElement = generator()
  def hasNext: Boolean = {
    return !nextElement.isEmpty
  }
  override def next(): T = {
    nextElement match {
      case Some(x) =>
        nextElement = generator()
        x
      case None => throw new NoSuchElementException("next on None generator")
    }
  }
}