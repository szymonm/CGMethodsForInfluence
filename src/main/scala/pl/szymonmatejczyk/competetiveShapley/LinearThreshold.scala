package pl.szymonmatejczyk.competetiveShapley

import collection._
import scala.collection.parallel._
import scala.annotation.tailrec

trait LinearThreshold {
  self : InfluenceNetwork =>

  def uniformActivationThresholdGenerator() : Double = r.nextDouble()
  
  def incrementalRandomLinearThresholdSeedQuality(incrementalSeeds: Seq[Set[Int]],
      activationThresholdGenerator: (() => Double) = uniformActivationThresholdGenerator): Stream[Int] = {
    
    val activationThresholdsLeft = new Array[Double](self.maxNodeId + 1)
    graph.nodes.foreach(n => activationThresholdsLeft(n.value) = activationThresholdGenerator())
    
    @tailrec
    def activateNodes(initiallyActivated: ParSet[Int], activated: ParSet[Int], lastPhaseActivated: ParSet[Int]): ParSet[Int] = {
      if (lastPhaseActivated.isEmpty)
        activated.diff(initiallyActivated)
      else {
        val thresholdsFilled: ParSet[Int] = lastPhaseActivated.toSeq
          .flatMap(node => 
            g.get(node).outgoing
              .filterNot(e => activated.contains(e._2))
              .map(e => (e._2, e.weight.toDouble / weightDenominator)))
          .groupBy(_._1)
          .map{ case (node, listOfInfluences) => (node, listOfInfluences.map(_._2).sum)}
          .flatMap{ case (node, sumOfInfluence) =>
            activationThresholdsLeft(node) -= sumOfInfluence
            if (activationThresholdsLeft(node) <= 0) ParSet[Int](node) else ParSet[Int]()}.toSet
        
        activateNodes(initiallyActivated, activated ++ thresholdsFilled, thresholdsFilled)
      } 
    }
  
    def activateNodesIncremental(activated: ParSet[Int], nextParts: Seq[Set[Int]]): Stream[Int] = {
      nextParts match {
        case Seq() => Stream.empty[Int]
        case head +: tail =>
          val incrementallyActivated = activateNodes(activated.par, activated ++ head, head.par)
          incrementallyActivated.size #:: activateNodesIncremental(activated ++ incrementallyActivated, tail)
      }
    }
    
    activateNodesIncremental(ParSet(), incrementalSeeds)
  }
  
  def randomLinearThresholdSeedQuality(seed : Seq[Int],
      activationThresholdGenerator : (() => Double) = uniformActivationThresholdGenerator) : Int = 
        incrementalRandomLinearThresholdSeedQuality(Seq(seed.toSet), activationThresholdGenerator).head

  def mcLinearThresholdSeedQuality(seed : Seq[Int], runs : Int, 
      atg : (() => Double) = uniformActivationThresholdGenerator) : Double = {
    1.to(runs).par.map{_ => randomLinearThresholdSeedQuality(seed, atg).toDouble}.sum / runs
  }
}