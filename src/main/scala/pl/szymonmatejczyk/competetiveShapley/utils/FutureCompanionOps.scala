package pl.szymonmatejczyk.competetiveShapley.utils

import scala.language.postfixOps
import scala.util._
import scala.util.control.NonFatal
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global

object FutureExtensions {
  implicit class FutureCompanionOps[T](val f: Future.type) extends AnyVal {

    /**
     * Given a list of futures `fs`, returns the future holding the list of values of all the futures from `fs`.
     *  The returned future is completed only once all of the futures in `fs` have been completed.
     *  The values in the list are in the same order as corresponding futures `fs`.
     *  If any of the futures `fs` fails, the resulting future also fails.
     */
    def all[T](fs: List[Future[T]]): Future[List[T]] = {
      var rs = Promise[List[T]]
      rs.success(Nil)
      fs.foldRight(rs.future) {
        (fut, acc) => for (elem <- fut; l <- acc) yield elem :: l
      }
    }

    /**
     * Given a list of futures `fs`, returns the future holding the value of the future from `fs` that completed first.
     *  If the first completing future in `fs` fails, then the result is failed as well.
     *
     *  E.g.:
     *
     *      Future.any(List(Future { 1 }, Future { 2 }, Future { throw new Exception }))
     *
     *  may return a `Future` succeeded with `1`, `2` or failed with an `Exception`.
     */
    def any[T](fs: List[Future[T]]): Future[T] = {
      var rs = Promise[T]
      fs.foreach {
        fut =>
          fut.onComplete {
            t => rs.tryComplete(t)
          }
      }
      rs.future
    }

    /**
     * Returns a future with a unit value that is completed after time `t`.
     */
    def delay(t: Duration): Future[Unit] = future {
      blocking {
        Thread.sleep(t.toMillis)
      }
    }

    /**
     * Completes this future with user input.
     */
    def userInput(message: String): Future[String] = Future {
      readLine(message)
    }
  }
}