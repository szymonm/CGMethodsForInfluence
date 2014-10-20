package pl.szymonmatejczyk.competetiveShapley.utils

object Tee {
  def apply[T](t: T): T = {
    println(t)
    t
  }
}