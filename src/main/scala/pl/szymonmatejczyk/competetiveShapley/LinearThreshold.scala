package pl.szymonmatejczyk.competetiveShapley

import collection._
import scala.collection.parallel._
import scala.annotation.tailrec

trait LinearThreshold {
  self : InfluenceNetwork =>

  private val uniformActivationThresholdGenerator: () => Double = r.nextDouble _
  
  def incrementalRandomLTInfluence(incrementalSeeds: Seq[Set[Int]],
      activationThresholdGenerator: (() => Double) = uniformActivationThresholdGenerator): Stream[Int] = {
    
    val activationThresholdsLeft = new Array[Double](self.maxNodeId + 1)
    graph.nodes.foreach(n => activationThresholdsLeft(n.value) = activationThresholdGenerator())
    
    @tailrec
    def activateNodes(activated: Seq[Int], allActivated: Set[Int], lastPhaseActivated: Set[Int], depth: Int =1): Seq[Int] = {
      if (depth%1000 == 0) logger.info(s"depth: $depth")
      if (lastPhaseActivated.isEmpty)
        activated
      else {
        val thresholdsFilled: Set[Int] = lastPhaseActivated.toSeq
          .flatMap(node => 
            graph.get(node).outgoing
              .filterNot(e => allActivated.contains(e._2.value))
              .map(e => (e._2, e.weight.toDouble / weightDenominator)))
          .groupBy(_._1)
          .map{ case (node, listOfInfluences) => (node, listOfInfluences.map(_._2).sum)}
          .flatMap{ case (node, sumOfInfluence) =>
            activationThresholdsLeft(node.value) -= sumOfInfluence
            if (activationThresholdsLeft(node.value) <= 0) Some(node.value) else None}.toSet
        
        activateNodes(activated ++ thresholdsFilled, allActivated ++ thresholdsFilled, 
          thresholdsFilled, depth+ 1)
      } 	
    }
  
    def activateNodesIncremental(activated: Set[Int], nextParts: Seq[Set[Int]], depth:Int =1): Stream[Int] = {
      if (depth%1000 == 0) logger.info(s"depth: $depth")
      nextParts match {
        case Seq() => Stream.empty[Int]
        case head +: tail =>
          val seed = head.filterNot(activated.contains)
          val incrementallyActivated = activateNodes(seed.toVector, activated ++ head, seed)
          incrementallyActivated.size #:: activateNodesIncremental(activated ++ incrementallyActivated, tail, depth + 1)
      }
    }
    activateNodesIncremental(Set(), incrementalSeeds)
  }
  
  def randomLTInfluence(seed : Seq[Int],
      activationThresholdGenerator : (() => Double) = uniformActivationThresholdGenerator) : Int = 
        incrementalRandomLTInfluence(Seq(seed.toSet), activationThresholdGenerator).head

  def mcIncrementalLTInfluence(seeds: Seq[Set[Int]], runs: Int, 
      atg: (() => Double) = uniformActivationThresholdGenerator): Seq[Double] = {
    val incrementalInfluences: Array[Array[Int]] = (1 to runs).par
      .map(_ => incrementalRandomLTInfluence(seeds, atg).toArray).toArray
    
    (0 until seeds.size).map {
      i => (0 until runs).foldLeft(0.0){case (cur, next) => cur + incrementalInfluences(next)(i)} / runs
    }
  }
  
  def mcLTInfluence(seed : Seq[Int], runs : Int, 
      atg : (() => Double) = uniformActivationThresholdGenerator) : Double = {
    1.to(runs).par.map{_ => randomLTInfluence(seed, atg).toDouble}.sum / runs
  }
}