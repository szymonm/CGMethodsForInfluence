package pl.szymonmatejczyk.competetiveShapley.liveGraphs

import scala.collection._
import scala.concurrent._
import pl.szymonmatejczyk.competetiveShapley.InfluenceNetwork

trait IncrementalInfluence extends LiveGraph {
  self : InfluenceNetwork =>
  
  def randomStep(node : g.NodeT, visited : Set[Int]) : Set[Int] = {
    randomlyInfluencedFromQueue(immutable.Queue(node), visited)
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
    val seedIds = seed.map(_.value)
    val initiallyInfluenced = seedIds ++ randomlyInfluencedFromQueue(immutable.Queue() ++ seed, Set[Int]())
    additionalNodeSeqs.map {
      case last +: Seq() =>
        val additionalInfluencersQueue = immutable.Queue(last).filterNot(q => initiallyInfluenced.contains(q.value))
        val incrementallyVisited = randomlyInfluencedFromQueue(additionalInfluencersQueue, 
            initiallyInfluenced)
        val incrementalInfluence = incrementallyVisited.size
        (initiallyInfluenced.size + incrementalInfluence, incrementalInfluence)
      case init :+ last =>
        val butLastInfluenced = randomlyInfluencedFromQueue(
            (immutable.Queue() ++ init).filterNot(q => initiallyInfluenced.contains(q.value)), 
            initiallyInfluenced)
        val alreadyInfluenced = initiallyInfluenced ++ butLastInfluenced
        val lastIncrementalInfluence = randomlyInfluencedFromQueue(immutable.Queue(last).filterNot(q => alreadyInfluenced.contains(q.value)), 
                               									  alreadyInfluenced)
        val totalInfluence = alreadyInfluenced.size + lastIncrementalInfluence.size
        (totalInfluence, lastIncrementalInfluence.size)
    }
  }
  
  def mcInfluence(extractInt : Influence => Int)(seed : immutable.Set[g.NodeT], 
      additionalNodeSeqs : Seq[Seq[g.NodeT]], mcIterations : Int) : Seq[Double] = {
    def zippedSum(l1 : Seq[Int], l2 : Seq[Int]) = l1.zip(l2).map(x => x._1 + x._2)

    1.to(mcIterations).par
      .map {
        _ => randomInfluence(seed, additionalNodeSeqs).map(extractInt)
      }
      .foldLeft(Seq.tabulate(additionalNodeSeqs.size)(_ => 0)) { case (x, y) => zippedSum(x, y) }
      .map(_.toDouble / mcIterations)
  }
  
  def mcTotalInfluence = mcInfluence((x : Influence) => x._1) _
  def mcIncrementalInfluence = mcInfluence((x : Influence) => x._2) _
}