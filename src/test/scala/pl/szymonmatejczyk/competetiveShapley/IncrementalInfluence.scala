package pl.szymonmatejczyk.competetiveShapley

import scala.collection._
import scala.concurrent._

trait IncrementalInfluence {
  self : LiveGraph => 
 
  def mcInfluecne(seed : Set[Int], mcIterations : Int) : Future[Double]
  
  def step(node : Int, visited : immutable.Set[Int], mcIterations : Int) : Set[Int] = {
    ???
  }
  
  
}