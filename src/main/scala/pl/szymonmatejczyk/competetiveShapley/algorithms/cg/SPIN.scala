package pl.szymonmatejczyk.competetiveShapley.algorithms.cg

import scala.Vector
import scala.collection.Seq
import scala.collection.Set
import scala.collection.immutable.Stream.consWrapper
import pl.szymonmatejczyk.competetiveShapley.InfluenceHeuristic
import pl.szymonmatejczyk.competetiveShapley.InfluenceNetwork
import pl.szymonmatejczyk.competetiveShapley.algorithms.streamToInfluenceHeuristic
import scala.concurrent.ExecutionContext

class SPIN(influenceNetwork: InfluenceNetwork, outerMC: Int = SPIN.OUTER_MC , 
    innerMC: Int = SPIN.INNER_MC )(implicit executionContext: ExecutionContext) 
extends Ramasuri(influenceNetwork, outerMC, innerMC)(executionContext) {
  def topknodes(ranking: Seq[Int]): Stream[Int] = {
    
    def neighborOfTaken(id: Int, taken: Set[Int]): Boolean = {
      influenceNetwork.g.get(id).neighbors.find(x => taken.contains(x.value)).isDefined
    }
    
    def next(ranking: List[Int], left: Vector[Int], taken: Set[Int]): Stream[Int] = {
      if (ranking.isEmpty) {
        left.toStream
      } else {
        if (!neighborOfTaken(ranking.head, taken)) {
          ranking.head #:: next(ranking.tail, left, taken + ranking.head)
        } else {
          next(ranking.tail, left :+ ranking.head, taken)
        }
      }
    }
    
    next(ranking.toList, Vector(), Set())
  }
  
  override def topknodes() = topknodes(ranking().toSeq.sortBy(-_._2).map(_._1))
}

object SPIN {
  val NAME = "SPIN"
  
  val OUTER_MC = 10000
  val INNER_MC = 4000
    
  def influenceHeuristic(outerMc: Int = OUTER_MC , innerMC: Int = INNER_MC )(implicit executionContext: ExecutionContext): 
    InfluenceHeuristic = 
    new InfluenceHeuristic(NAME, (in: InfluenceNetwork) => (k: Int) => new SPIN(in, outerMc, innerMC).topknodes.take(k))
  
  def influenceHeuristicForSequenceOfK(outerMc: Int = OUTER_MC , innerMC: Int = INNER_MC )
  (implicit executionContext: ExecutionContext) = streamToInfluenceHeuristic(NAME,
    (in: InfluenceNetwork) => new SPIN(in, outerMc, innerMC).topknodes())
}