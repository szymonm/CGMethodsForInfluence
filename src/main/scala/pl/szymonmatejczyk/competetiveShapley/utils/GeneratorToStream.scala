package pl.szymonmatejczyk.competetiveShapley.utils

object GeneratorToStream {
  def generatorToStream[T](g: (() => Option[T])): Stream[T] = {
    g().map(_ #:: generatorToStream(g)).getOrElse(Stream.empty[T]) 
  }
  
  def apply[T](g: (() => Option[T])) = generatorToStream[T](g: (() => Option[T]))
}