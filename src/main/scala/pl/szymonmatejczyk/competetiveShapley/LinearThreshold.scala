package pl.szymonmatejczyk.competetiveShapley

import collection._
import scala.collection.parallel.immutable.ParSet
import scala.annotation.tailrec

trait LinearThreshold {
  self : InfluenceNetwork =>

  def activationThresholdGenerator() : Double = r.nextDouble()
  
  def randomLinearThresholdSeedQuality(seed : Seq[Int],
      activationThresholdGenerator : (() => Double) = activationThresholdGenerator) : Int = {

    val activationThresholdsLeft = new Array[Double](self.maxNodeId + 1)
    graph.nodes.foreach(n => activationThresholdsLeft(n.value) = activationThresholdGenerator())
      
    @tailrec
    def activateNodes(activated: ParSet[Int], lastPhaseActivated: ParSet[Int]): ParSet[Int] = {
      if (lastPhaseActivated.isEmpty)
        activated
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
        
        activateNodes(activated ++ thresholdsFilled, thresholdsFilled)
      } 
    }
    activateNodes(ParSet() ++ seed, ParSet() ++ seed).size
  }
  
  def mcLinearThresholdSeedQuality(seed : Seq[Int], runs : Int, 
      atg : (() => Double) = activationThresholdGenerator) : Double = {
    1.to(runs).par.map{_ => randomLinearThresholdSeedQuality(seed, atg).toDouble}.sum / runs
  }
}