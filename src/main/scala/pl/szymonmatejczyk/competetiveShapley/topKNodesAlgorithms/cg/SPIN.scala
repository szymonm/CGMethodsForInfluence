package pl.szymonmatejczyk.competetiveShapley.topKNodesAlgorithms.cg

import pl.szymonmatejczyk.competetiveShapley.InfluenceNetwork
import scala.collection._
import pl.szymonmatejczyk.competetiveShapley.common._
import scala.annotation.tailrec

class SPIN(influenceNetwork: InfluenceNetwork, outerMC: Int = 10000, innerMC: Int = 4000) 
extends Ramasuri(influenceNetwork, outerMC, innerMC) {
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
  
  override def topknodes() = topknodes(super.ranking().toSeq.sortBy(-_._2).map(_._1))
}