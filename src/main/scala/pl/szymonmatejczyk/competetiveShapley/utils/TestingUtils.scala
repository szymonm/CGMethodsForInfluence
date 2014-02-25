package pl.szymonmatejczyk.competetiveShapley.utils

import scala.concurrent.duration._

object TestingUtils {
  def time[A](s: String, a: => A) = {
    val now = System.nanoTime
    val result = a
    val micros = (System.nanoTime - now) / 1000
    println("%s %d microseconds".format(s, micros))
    result
  }
  
  def time[A](a : => A) : (A, Duration) = {
    val now = System.nanoTime
    val result = a
    val micros = (System.nanoTime - now) / 1000
    (result, Duration(micros, "micro") )
  }
}