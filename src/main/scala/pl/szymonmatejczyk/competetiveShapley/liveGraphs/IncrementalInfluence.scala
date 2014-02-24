package pl.szymonmatejczyk.competetiveShapley.liveGraphs

import scala.collection._
import scala.concurrent._
import pl.szymonmatejczyk.competetiveShapley.InfluenceNetwork

trait IncrementalInfluence extends LiveGraph {
  self : InfluenceNetwork =>
  
  def randomStep(node : g.NodeT, visited : Set[Int]) : Set[Int] = {
    randomlyReachableFromQueue(immutable.Queue(node), visited)
  }
  
  def mcStep(node : g.NodeT, visited : immutable.Set[Int], mcIterations : Int) : Double = {
    0.to(mcIterations).par.map{
      _ => randomStep(node, visited).size.toDouble 
    }.sum / mcIterations
  }
  
  type TotalInfluence = Int
  type LastIncrementalInfluence = Int
  type Influence = (TotalInfluence, LastIncrementalInfluence)
  
  def randomInfluence(seed : immutable.Set[g.NodeT], 
                      additionalNodeSeqs : Seq[Seq[g.NodeT]]) : Seq[Influence] = {
    val initiallyVisited = randomlyReachableFromQueue(immutable.Queue() ++ seed, Set[Int]())
    additionalNodeSeqs.map{
      case last +: Seq() => 
        val incrementalInfluence = randomlyReachableFromQueue(immutable.Queue(last), 
                                                              initiallyVisited).size
        (initiallyVisited.size + incrementalInfluence, incrementalInfluence)
      case init :+ last =>
        val butLastVisited = randomlyReachableFromQueue(immutable.Queue() ++ init, 
            initiallyVisited)
        val lastIncrementalInfluence = randomlyReachableFromQueue(immutable.Queue(last), 
            initiallyVisited ++ butLastVisited).size
        val totalInfluence = initiallyVisited.size + butLastVisited.size + lastIncrementalInfluence 
        (totalInfluence, lastIncrementalInfluence)
    }
  }
  
  def mcInfluence(unpack : Influence => Int)(seed : immutable.Set[g.NodeT], 
      additionalNodeSeqs : Seq[Seq[g.NodeT]], mcIterations : Int) : Seq[Double] = {
    def zippedSum(l1 : Seq[Int], l2 : Seq[Int]) = l1.zip(l2).map(x => x._1 + x._2)
    
    0.to(mcIterations).par.map{
      _ => randomInfluence(seed, additionalNodeSeqs).map(unpack)
    }.foldLeft(Seq.tabulate(additionalNodeSeqs.size)(_ => 0)){case (x,y) => zippedSum(x, y)}.
    map(_.toDouble / mcIterations)
  }
  
  def mcTotalInfluence = mcInfluence((x : Influence) => x._1) _
  def mcIncrementalInfluence = mcInfluence((x : Influence) => x._2) _
  
}