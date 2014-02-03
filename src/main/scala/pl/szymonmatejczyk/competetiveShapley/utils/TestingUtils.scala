package pl.szymonmatejczyk.competetiveShapley.utils

object TestingUtils {
  def time[A](s: String, a: => A) = {
    val now = System.nanoTime
    val result = a
    val micros = (System.nanoTime - now) / 1000
    println("%s %d microseconds".format(s, micros))
    result
  }
}